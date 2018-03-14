#################分词#############################################
library(jiebaR)
engine<-worker()
words<-"想学R语言，那就赶紧拿起手机，打开微信，关注公众号《跟着菜鸟一起学R语言》，跟着菜鸟一块飞。"
segment(words,engine)
# 备用命令
engine<=words

#################添加用户自定义词或词库############################
engine_new_word<-worker()
new_user_word(engine_new_word, c("公众号","R语言"))
segment(words,engine_new_word)
# 使用user()添加词库
engine_user<-worker(user='dictionary.txt')# 添加到工作空间
segment(words,engine_user)

#################删除停用词####################################
engine_s<-worker(stop_word = "stopwords.txt")
segment(words,engine_s)

#################统计词频###################################################
freq(segment(words,engine_s))

################词性标注################################################
qseg[words]
qseg<=words
# 备用函数，将work()中的type改成tag
tagger<-worker(type="tag")
tagger<=words

#################提取关键字###################################################
keys<-worker(type="keywords",topn=2)
keys<=words
# 备用函数
keys2<-worker(type="simhash",topn=2)
keys2<=words

