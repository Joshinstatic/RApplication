###载入R包,还需pillar包####################################################
library("dplyr")


######数据框建立####################################################
name1 <- c("Bob","Mary","Jane","Kim")
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
birth <- c("1990-1","1980-2","1995-5","1996-4")
accept <- c("no","ok","ok","no")

df1 <- data.frame(name1,weight,height)
rownames(df1) <- letters[1:4] # 赋予行名
df2 <- data.frame(name2,birth,accept)

#除此之外还有两种方法建立数据框
#1.as.data.frame(x)
#2.通过读取文件来获得数据框

########提取####################################################
# 通过做坐标选取
df1[2,3]
# 分别对应第2行的第1和第3个，第4行的第1和第3个
df1[c(2,4),c(1,3)]
# 通过[]选择行列名
df1["a","weight"]
df1["a",2] # 名称和序号是相同的
df1[cbind(c(1,2,1),3:1)]# cbind按行排列：第一行的第三个，第二行的第二个，第一行的第一个
df1[cbind(c("a","b"), c("weight","height"))]# a行的weight，b行的height
df1[2,] # 取行
df1[,3] # 取列
df1[c(2,3),] # 取多行
df1[,c(2,3)] # 取多列
df1["a",]
df1[,"weight"]
df1[3] # 只接一个数字，选择列
df1[c(2,3)] # 使用向量则选择多列
df1$weight # 通过$选择列名名字来选择列，给出数值
# 列名放在[]中选取列
df1["weight"]#注意df1$weight与此的不同
df1[c("weight","height")] # 使用向量选择多列

select(df2,accept)  # dplyr包中函数，等价于 df2$accept
#select(df2,"Bob")无效说明只能selet()只能用于列

# 取行和取列默认不一样
class(df1[,3]) # "numeric"表示提取到的是一个向量
class(df1[2,]) # "data.frame"表示提取到的仍是一个数据框
# 另外三种
class(df1[3]) # "data.frame"
class(df1["weight"]) # "data.frame"
class(df1$weight) # "numeric"
# 转化，drop=T是降维的意思
class(df1[,3,drop=F]) # "data.frame"
class(df1[2,,drop=T]) # "list"
# 取行用drop也不能转化为向量，那就用unlist
class(unlist(df1[2,])) # "numeric"
# 对于只接受一个维度参数的方法来说[[ ]]相当于$，可以实现降维
class(df1[["weight"]]) # "numeric"
class(df1$weight)
class(df1[[2]]) # "numeric"
# 注意：[[]]不能作用到选取多列的情况上，因为多列谈不上降维
df1[[c("weight","height")]] # 报错
df1[[1:3]] # 报错
# 模糊匹配
df1$wei # 前面匹配就可以选出那一列，但是必须是唯一匹配
df1$weig <- 1:4 # 新增加一列weig
df1$wei # 匹配出两个都满足（weigth和weig），则返回NULL
# 通过逻辑值提取
# 通过判断语句
df2[df2$accept=="no"|name2=="Bob",]#|表示或
subset(df2,accept=="no") # R自带提取函数
filter(df2,accept=="no"|name2=="Bob") # dplyr包提取函数
df2$accept=="no"|name2=="Bob"#因没有[]所以不是选取而是判断
df2[df2$accept=="no"|name2=="Bob",]
df2[df1$weight>50,]#[]中不一定是本数据框中的变量，只是等长逻辑值向量
                    #即可，只是后面不可少了","


#########提取衍生物####################################################
 ##修改######
df1[2,3] <- 160;df1
df2$accept[df2$accept=="ok"] <- "yes"#此方法产生NA，似乎是数据类型不符

df2 <- data.frame(name2,birth,accept,stringsAsFactors=F)
within(df2,{accept[accept=="ok"]<-"yes"
name2[name2=="Bob"]<-"BOB"
birth[birth=="1990-1"] <- "1989-2"})#within(df,{中间没有","})

############计算###############
df2 <- data.frame(name2,birth,accept,stringsAsFactors=F)
attach(df2)
2*weight
detach(df2) #必须使用detach()消除关联，否则变量名称混乱会出错

with(df2,{a <- weight*2
a^3})# 推荐使用的方法，with(data,{expression}，表达式中间没有",")
      #并且with()用于计算，输出的是向量；within()则是修改

#####################用"-"删除################
df1[-c(2,3),]#删除第二和第三行
df1[,c(T,F,T)]#删除第二列，保留第一和第三列

#############用order函数排序##############
df1_order_row <- df1[order(df1$weight),]#注意有“,”
        #若出现报警信息“undefined columns selected”，则需要加“,”
order(df1$weight) # 给出体重的排序次序，不是表
# 先按体重，再按身高排序（当体重一样时）
df1_order_row <- df1[order(c(df1$weight,df1$height)),]
# 按照列名对列排序
df1_order_col <- df1[,order(colnames(df1))]
# 使用dplyr包对行更方便、高效地处理
arrange(df1,weight,height)

#########插入############
#R无法直接插入，只能分开——插入——拼接，较繁琐

#########管道操作%>%#############
#如果载入Hadley的包，就自动可以使用这个函数，否则需加载magrittr包
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
accept <- c("no","ok","ok","no")
df <- data.frame(name2,weight,height,accept)
# 下面两个操作等价
select(df,weight:accept) #只保留函数内部的变量
df %>% select(weight:accept)
# %>% 的作用在于把前面的内容放到后面函数中，作为第一个参数
# 管道操作的好处在于减少中间变量，并且更加易读
#比较
df %>% select(starts_with("w")) %>% `*`(2) %>% 
  unlist(.) %>% matrix(nrow=2) %>% colMeans(.) %>% plot(.)
# 可用“.”表示前面的变量
plot(colMeans(matrix(unlist(2*select(df,starts_with("w"))),
                     nrow=2)))

w <- select(df,starts_with("w"))
v <- unlist(w*2)
m <- matrix(v,nrow=2)
plot(colMeans(m))
# 一次使用多个函数计算
df$weight %>% {c(min(.), mean(.), max(.))} %>% floor #floor是取整函数
# 想作为函数第二个参数时，可以用.代替
2 %>% head(df,.)
# 将得到的结果赋值(?)
a <- df %>% .$name2 %>% grep("a",.)


