################准备工作############################################
# 载入包
library('ggplot2') # 画图包
library('ggthemes') # 画图包
library('scales') # 画图包
library('dplyr') # 数据处理包
library('mice') # 数据处理包
library('lattice')
library('randomForest') # 算法包
library('ggplot')
# 载入数据
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # 按行合并数据
str(full) ## 查看数据

###############特征工程###########################################
# Name变量信息提取
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)# 从名称获得称谓
gsub("(.*, )|(\\..*)","","McCarthy, Mr. Timothy J")
# “.#, ”表示“, ”前面的均替换为空，
# 而“\\..*”表示“.*”之前到后面的均替换为空
table(full$Sex, full$Title)# 按性别查看称谓分布情况
# 个位数字的称谓重新定义
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# 调整其他变量名
full$Title[full$Title == 'Mlle'] <- 'Miss' 
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
# rare_title是否包含full$Title，若为真，则变量名为Rare Title
table(full$Sex, full$Title) #再按性别展示称谓
# 从名字里面提取姓氏
full$Surname <- sapply(full$Name,  
                       function(x) {strsplit(x, split = '[,.]')[[1]][1]
                       })
# sapply(a,b)函数是将数据a照函数b计算
full$Surname <- gsub('.*. ','',full$Name)# 上面代码的简化版
head(full$Surname)
cat(paste('We have', nlevels(factor(full$Surname)), 
# R中vector是factor的特殊形式，只要有字符串出现，R就认为
# 数据类型是factor
          'unique surnames. 
          I would be interested to infer ethnicity based on surname --- 
          another time.'))
#cat输出语句，即paste连接后的句子

# 创建家庭成员数目变量（包括乘客自己）
full$Fsize <- full$SibSp + full$Parch + 1
# 创建带成员数目的家庭变量 
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# 画出家庭变量图像
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size',y = 'counts') + 
  theme_bw()
# 按家庭成员数目分类
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
# 按重新定义的变量展示家庭成员数目存活情况
mosaicplot(table(full$FsizeD, full$Survived), 
            main='Family Size by Survival', shade = TRUE)
#马赛克图表示单身乘客和大家庭乘客存活率很低，但小规模家庭（2~5人）
# 存活率较高

#研究客舱等级对存活率的影响
full$Cabin[1:28]
# 提取第二为乘客客舱信息
strsplit(full$Cabin[2], NULL)[[1]]
# 创建客舱等级变量
full$Deck <- factor(sapply(full$Cabin, 
                           function(x) {
                             strsplit(x, NULL)[[1]][1]
                             }))
#######################缺失值处理############################################
# 处理缺失值
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)
# 画出箱线图查看缺失数据的情况 
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels = dollar_format()) +
  theme_few()
# 根据数据特征认为他们是c等舱
full$Embarked[c(62, 830)] <- 'C'
# 其他缺失值
full[1044,]
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)
# 取中位数补足缺失值
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', 
                               ]$Fare, na.rm = TRUE)
# 处理缺失的年龄
sum(is.na(full$Age))
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
# 将确实变量因子化
full[factor_vars] <- lapply(full[factor_vars], 
                            function(x) as.factor(x))
# 设随机种子
set.seed(129)

# 利用mice包进行计算
mice_mod <- mice(full[, !names(full) %in% 
                        c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], 
                 method='rf') 
# 保存
mice_output <- complete(mice_mod)
# 画概率分布图验证估计有效
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# 替换
full$Age <- mice_output$Age
# 缺失值补完
sum(is.na(full$Age))
# 查看Age和Survival的关系
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()
# 区分孩子和成年人
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# 按组展示结果
table(full$Child, full$Survived)
# 加入Mother变量
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# 再次按组展示结果
table(full$Mother, full$Survived)
# 因子化
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)
md.pattern(full)

###########预测######################################################
# 将数据分组
train <- full[1:891,]
test <- full[892:1309,]
# 设随机种子
set.seed(754)
# 建立随机森林模型
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)
# 画图查看误差
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# 参数按重要性排序  
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# 画图表示模型各参数的重要程度
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
# 利用测试集数据进行预测
prediction <- predict(rf_model, test)

# 保存预测结果
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# 生成文件并且保存
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

###############gusb函数简介#######################################
# grep、grepl、sub、gsub、regexpr、gregexpr等
# 都是利用正则表达式的规则进行匹配
sub("^a","",c("abcd","dcba"))# 若字符串开头为a，将其去掉
sub("a$","",c("abcd","dcba"))# 若字符串结尾为a，将其去掉
sub("a.c","",c("abcd","sdacd"))# 若字符串中含a.c（.除换行符外），将其一并去掉
sub("a*b","",c("aabcd","dcaaaba"))# 对*前的字符进行一次或者多次匹配，将其一并去掉
sub("a.*e","",c("abcde","edcba"))# .*匹配任意字符，将其一并去掉
sub("ab|ba","",c("abcd","dcba")) #|匹配或者，将其去掉
L="dsajfhdsaNNNN   NNNkdsjf" # 结果为dsajfhdsa   kdsjf
gsub('N','',L)

########################strsplit()###############################
strsplit("A text I want to display with spaces", NULL)
# 























































































































