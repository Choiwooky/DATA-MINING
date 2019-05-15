# rvest : R에서의 크롤링에 필요한 패키지
if(!require(rvest)){
  install.packages('rvest')
}
# dplyr : 데이터 전처리에 필요한 패키지 설치 (%>% 사용)
if(!require(dplyr)){
  install.packages('dplyr')
}
# N2H4 : 네이버 댓글 크롤링을 위한 패키지 
if (!requireNamespace("N2H4")){
  source("https://install-github.me/forkonlp/N2H4")
}

library(rvest)
library(dplyr)
library(N2H4)

#################################################################
### 특정섹션, 특정기간 url 생성

naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='

# 섹션별 100:정치 101:경제 102:사회 103:생활/문화 104:세계 105:IT/과학

# 섹션, 날짜 지정 사용자 지정함수

myurl <- function(section, date) {
  section <- readline(prompt = "섹션지정: ")
  date <- readline(prompt = "날짜지정: ")
  naver_url_2 <- '&date='
  naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
  naver_url <<- paste0(naver_url_1,section,naver_url_2,date)
  print(naver_url)
}


# 전역변수 지역변수 개념
# function 내부에서 전역변수로 선언 : <<-

#################################################################
### 특정 기간의 전체 섹션 반복 url
naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
section <- 100:105 
naver_url_2 <- '&date='
# 구하려는 날 연도월일 year mn dy
# 달 바뀔때는 조심할 것 (435 이런식으로 넘어감)
date <- 20190427:20190430 

i <- 1
naver_url <- NULL
for (dt in date){
  for (sec in section){
    naver_url[i] <- paste0(naver_url_1,sec,naver_url_2,dt)
    i <- i + 1
  }
}

naver_url
# &sectionId=100&date=20190411 이런식으로 naver_url에 저장


################################################################
### 위 작업을 통한 url(특정 세션의 순위권 기사)에 대해 댓글 

# 위 작업을 통해 얻은 url중 하나 임의로 정함
##pnews_url = 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date=20190411'
myurl()
pnews_url <- naver_url
# 앞서 만든 myurl() 함수 활용

html <- read_html(pnews_url)
#read_html() : 해당 url의 html 소스코드를 가져옴


temp <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('href')
# rangking_item is num 생략
#특정 날짜, 특정 섹션의 1~30위 까지 기사의 url 크롤링

temp1 <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title')
# 특정 날짜, 특정 섹션의 기사 제목 크롤링


# 댓글이 있는 위치의 html을 확인해서 작업
# html_nodes() : 해당태그가 포함하고 있는 소스 및 코드 속성을 추출함
# html_attr() : 해당 속성 값을 추출함
# 속성이 class인 경우 'css' 약자를 사용


### 위 과정을 통해 구한 url에
### https://news.naver.com 를 앞에 붙여줘야 링크연결
char_1 <- 'https://news.naver.com'
ranked_url <- c()
for(i in 1:length(temp)){
  ranked_url[i] <- paste0(char_1,temp[i])
}

ranked_url
# ranked_url : 랭킹별 url 완성


raw.data <- list()

for(i in 1:10){
  url <- ranked_url[i]
  k <- getAllComment(url)
  k.con <- k %>% select(contents, sympathyCount)
  raw.data[[i]] <- k.con %>% arrange(desc(sympathyCount))
}
# raw.data : 1위부터 10위까지의 댓글을 모두 넣어줌

################################################################
### 연령별 (날짜별 데이터는 없다) 10 ~ 60

myurl_age <- function(age){
  url_age <<- paste0("https://news.naver.com/main/ranking/popularDay.nhn?rankingType=age&subType=",age)
}

html_age <- read_html(url_age)

temp0 <- html_age %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('href')

char_1 <- 'https://news.naver.com'
aged_url <- c()
for(i in 1:length(temp0)){
  aged_url[i] <- paste0(char_1,temp0[i])
}

aged_url
# aged_url : 나이별 url   

raw.data_age <- list()

for(i in 1:10){
  url <- aged_url[i]
  k <- getAllComment(url)
  k.con <- k %>% select(contents, sympathyCount)
  raw.data_age[[i]] <- k.con %>% arrange(desc(sympathyCount))
}
# raw.data_age : 연령별 1위부터 10위의 댓글 원 데이터 추출

################################################################
### csv 파일 작성
total <- NULL
for(i in 1:10){
  total <- bind_rows(total,raw.data[[i]][1:100,])
}
# total : raw.data의 갯수가 너무 많아 각각 기사의 댓글 중 100개 추출
#         이후 total에 열 기준 합침 

# setwd("C:/Users/whddnr/Desktop/rawdata")
write.csv(total, file = "sample.csv", row.names = TRUE)


## 크롤링 도움  https://kuduz.tistory.com/104
## 출처 : https://github.com/forkonlp/N2H4
