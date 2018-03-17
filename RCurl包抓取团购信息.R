library("RCurl")
library("XML")
# 读取拉手深圳美食搜索
start_url <- "http://shenzhen.lashou.com/cate/meishi"
# 构造请求头
cust_header =c("User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:26.0) Gecko/20100101 Firefox/26.0",
               "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
               "Accept-Language"="en-us",
               "Connection"="keep-alive")
# 读取PageSouce
pagesource <- getURL(start_url,
                     httpheader=cust_header,
                     .encoding="utf-8")
# 解析页数
parseTotalPage <- function(pagesource){
  doc <- htmlParse(pagesource)  
  as.numeric(sapply(getNodeSet(doc, '//div[@class="page"]/a[last()-1]/text()'), 
                    xmlValue))
}
# 解析页面内容，获取门店名称、描述、优惠价，门店价
parseContent <-  function(pagesource){
  doc <- htmlParse(pagesource)
  goods_name <- sapply(getNodeSet(doc, '//div[contains(@class,"goods")]//a[@class="goods-name"]//text()'), 
                       xmlValue)
  goods_text <- sapply(getNodeSet(doc, '//div[contains(@class,"goods")]//a[@class="goods-text"]//text()'), 
                       xmlValue)
  price <- sapply(getNodeSet(doc, '//div[contains(@class,"goods")]//span[@class="price"]/text()'), 
                  xmlValue)
  org_price <- sapply(getNodeSet(doc, '//div[contains(@class,"goods")]//span[@class="money"]/del/text()'), 
                      xmlValue)
  result <- data.frame(goods_name, goods_text, price, org_price)
}
# 获取总页数和第一页内容
total_page <- parseTotalPage(pagesource)
pageresults <- parseContent(pagesource)
# 生成2-n页url
page = 1:(total_page -1)
url_list = ""
url_list[page] = paste0("http://shenzhen.lashou.com/cate/meishi/page",
                        page +1)
# 循环读取url，并进行下载解析
for (url in url_list){
  pagesource <- getURL(url,httpheader=cust_header,
                       .encoding="utf-8")
  pageresult <- parseContent(pagesource)
  pageresults <- rbind(pageresults,pageresult)
}
# 输出结果到文件
write.table(pageresults,"result.txt",row.names=FALSE)
