#####################################################################
###数据处理
#####################################################################

#载入R包,还需pillar包
library("dplyr")

###数据框建立
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

###提取
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
df2[df2$accept=="no"|name2=="Bob",]
subset(df2,accept=="no") # R自带提取函数
filter(df2,accept=="no"|name2=="Bob") # dplyr包提取函数

