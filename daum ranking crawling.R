if (!requireNamespace("DNH4")){
  source("https://install-github.me/forkonlp/DNH4")
}
# DNH4 : 다음댓글 크롤링을 위한 패키지
if(!require(rvest)){
  install.packages('rvest')
}
# rvest : 크롤링을 위한 패키지
if(!require(dplyr)){
  install.packages('dplyr')
}
# dplyr : 전처리를 위한 패키지
library(DNH4)
library(rvest)
library(dplyr)
#######################################################################
### url 생성 함수

myurl <- function(what, when){
  what <- readline(prompt = "what kind : ")
  when <- readline(prompt = "when : ")
  if(what == "popular"){
    daum_url <<- paste0("https://m.media.daum.net/m/media/ranking/popular?regDate=",when)
  }else
    if(what == "reply"){
      daum_url <<-paste0("https://m.media.daum.net/m/media/ranking/bestreply?regDate=",when)
    }else
      if(what == "age"){
        daum_url <<- paste0("https://m.media.daum.net/m/media/ranking/age?regDate=",when)
    }
}


#######################################################################
### what : popular, reply 일때

pnews_url <- daum_url

html <- read_html(pnews_url)
temp <- list()


temp <- html %>% html_nodes(css='.link_news') %>% html_attr('href')
## 이거 왜 여기서 html_node('a') 쓰지말아야 할까?

temp0 <- html %>% html_node(css='.link_news') %>% html_attr('alt')

ranked_url <- sub(pattern ="f=m", replacement = "", x = temp)
head(ranked_url)
# sub, gsub 함수 이용 해서 마지막 "f=m" 제거해 줘야 돌아감
# ranked_url : 1위부터 15위까지 기사 url 전처리 후 저장
# ?sub()
k <- DNH4::getComment(ranked_url[1], limit = "all") 
raw.data <- list()
for (i in 1:length(ranked_url)){
  k <- DNH4::getComment(ranked_url[i], limit = "all") 
  k1 <- k %>% select(content, likeCount)
  k2 <- k1 %>% arrange(desc(likeCount))
  raw.data[[i]] <- k2
}
# raw.data : ranked_url의 getComment함수를 적용하고 content(댓글)와 likeCount(공감수)를 추출한 후 likeCount(공감수)기준 내림차순으로 정렬시켜 할당


total <- NULL
for(i in 1:length(raw.data)){
  total <- bind_rows(total, raw.data[[i]][1:100,])
}
# total : 각 기사의 댓글중 공감수 많은 순으로 100개씩 추출 

sample <- total %>% filter(!is.na(content))

# sample : 총 댓글수가 100개가 안되는 기사를 고려해 결측치 제거

setwd("C:/Users/whddnr/Desktop")

write.csv(sample, "sample.csv", row.names = TRUE)

#######################################################################
### what : age  
### 미완 도움필요
pnews_url <- daum_url
html <- read_html(pnews_url)


View(html)
#######################################################################

