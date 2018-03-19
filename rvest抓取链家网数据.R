rm(list = ls())
library("rvest")
library("XML")
library('stringr')

url0 <- 'http://cd.fang.lianjia.com/loupan/'
name = area = price = type = address = status = NULL
for(i in 1:10){
  url <- paste(url0, "pg", i, sep = "")
  web <- read_html(url)
  name <- c(name, web %>% html_nodes('div.info-panel') %>%
              html_nodes('a') %>% html_text())
  address <- c(address, web %>% html_nodes('div.info-panel') %>%
                 html_nodes('div_where') %>% html_nodes('span_region') %>%
                 html_text())
  b = web %>% html_nodes('div.info-panel') %>% html_nodes('div.area')
  are = rep(0, length(b))
  for(i in 1:length(b)){
    if(str_length(b[i]) > 60){
      are[i] = b[i] %>% html_nodes('span') %>% html_text()
    }
    else{
      are[i] = 0
    }
  }
}
area = c(area, ifelse(are == '0', '0', 
                      unlist(str_extract(are, '[0-9] + ~ [0-9] + [0-9]+'))))
a <- web %>% html_nodes('div.info-panel') %>% html_nodes('div.average')
price = rep(0, length(a))
for(i in 1:length(a)){
  if(str_length(a[i]) > 100){
    price[i] = a[i] %>% html_nodes('span_num') %>% html_text()
  }
  else{
    price[i] = 0
  }
}
price = c(price, price)
type <- c(type, web %>% html_nodes('div.info-panel') %>%
            html_nodes('div.type') %>% html_nodes('span.live') %>%
            html_text())
status <- c(status, web %>% html_nodes('div.info-panel') %>% 
              html_nodes('div.type') %>% html_nodes('span.onsold') %>%
              html_text())
data <- data.frame(name, address, area, price = as.numeric(price),
                   type, status)
head(data)
DT::datatable(data) # DT包中的datatable函数
