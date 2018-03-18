library("RCurl")
library("XML")

url1 <- 'http://shenzhen.lashou.com/cate/meishi'
web <- readLines(url1, encoding = 'UTF-8')
for(i in 2:150){
  web1 <- NULL
  url <- paste(url, "/page", i, sep = "")
  web1 <- readLines(url, encoding = 'UTF-8')
  web <- c(web, web1)
}

###############提取关键词信息##################################################
place <- web[grep("class = \"goods-place\"", web)]
place <- substr(place, 
                regexpr("\">", place) + 3, 
                regexpr("<\"", place) - 2 #正则表达式中\"
                )
# 商品名称
goods_name <- web[grep("goods-name", web)]
goods_name <- substr(goods_name, 
                     regexpr("\">", goods_name) + 2, 
                     nchar(goods_name) - 4)
# 商品说明
goods_text <- web[grep("goods-text", web)]
goods_text <- substr(goods_text, 
                     regexpr("\">", goods_text) + 2, 
                     nchar(goods_text) - 4)
# 价格
price <- web[grep("class = \"price\"", web)]
goods_text <- substr(price, 
                     regexpr("</em>", price) + 5,
                     nchar(price) - 7
                     )
# 原价
org_price <- web[grep("class = \"money\"", web)]
org_price <- substr(org_price, 
                    regexpr("<del>", org_price) + 5,
                    nchar(org_price) - 13
                    )
# 结果
result <- data.frame(place = place, 
                     goods_name = goods_name, 
                     goods_text = goods_text, 
                     price = price, 
                     org_price = org_price)
head(result)
