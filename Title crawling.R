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
# stringr : 문자열 전처리를 위한 패키지
if(!require(stringr)){
  install.packages("stringr")
}
# KoNLP : 한국어를 분석가능한 27가지 함수 제공

if(!require(KoNLP)){
  install.packages("KoNLP")
}


library(rvest)
library(dplyr)
library(N2H4)
library(stringr)
library(KoNLP)

###############################################################################
### 일 년 date 만들어주는 함수

fdate <- function(year) {
  mydate <<- NULL
  day <- 1:31
  for(i in 1:length(month.abb)) {
    if(i %in% c(1, 3, 5, 7, 8)) {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:31])))
    } else if(i %in% c(10, 12)) {
      mydate <<- c(mydate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:31])))
    } else if(i %in% c(4, 6, 9)) {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:30])))
    } else if(i %in% 11) {
      mydate <<- c(mydate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:30])))
    } else {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:28])))
    }
  }
}
# mydate : yyyymmdd 형태로 365일 저장

# 주의!!!
# 네이버 기사 크롤링은 2004년 4월 20일부터 가능함!!

#############################################################################
### 한 해 동안의 기사 제목에서 명사 추출 
# 정치섹션 많이본 기사 30위까지의 title 추출
# 섹션별 100:정치 101:경제 102:사회 103:생활/문화 104:세계 105:IT/과학
naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
section <- 100 
naver_url_2 <- '&date='

# 구하려는 날 연도월일 year mn dy
# 달 바뀔때는 조심할 것 (435 이런식으로 넘어감)

naver_url <- NULL
for (i in 1:length(mydate)){
    naver_url[i] <- paste0(naver_url_1,section,naver_url_2,mydate[i])
}

head(naver_url)
# naver_url : 댓글을 크롤링할 url 365개 완성

temp <- list()
for (i in 1:length(naver_url)){
  html <- read_html(naver_url[i])
  temp[[i]] <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title')
}
# temp : 추출한 타이틀 리스트에 저장
# mergeUserDic(data.frame('이명박','nqpc'))
# buildDictionary(ext_dic = c("sejong","woorimalsam"), user_dic = data.frame("이명박", "nqpc"), replace_usr_dic = T)
### 해결과제 : 대선주자 이름 dictionary에 추가?
#   -> 다른 방법을 사용하여 처리 -> 그런 이유로 이 부분은 주석으로 처리함.

### 기존 작성내용
# sentence <- unlist(temp)
# word_1 <- sapply(sentence, extractNoun, USE.NAMES = F)
# word_2 <- unlist(word_1)
# wordcount_2 <- table(word_2)
# head(sort(wordcount_2,decreasing = T),20)
# word_3 <-str_replace_all(word_2,'[전사수의나원위것0-9.]','')

## 해결과제 : 한자는 의미있는 단어이니 꼭 보존해야함
#            하지만 대부분의 의미있는 명사는 2글자 이상임
# Filter(function(x){nchar(x)>=2}, word_2) 코드활용
  
### 위 해결과제에 대한 코드
sentence <- unlist(temp)
word_1 <- sapply(sentence, extractNoun, USE.NAMES = F)
word_2 <- unlist(word_1)
wordcount_2 <- table(word_2)
head(sort(wordcount_2, decreasing = T), 20) # 우선 뽑은 데이터 확인

word_3 <- sapply(word_2, str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
word_3 <- sapply(unlist(word_3), paste, USE.NAMES = F)
word_3 <- Filter(function(x){x != ""}, word_3)
word_3 <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3)
word_3 <- word_3[ifelse(word_3 %in% as.character(0:9), F, T)]
word_3 # 필터링 거친 데이터 확인

# 필터링 거친 데이터 word_3를 가장 많이 나온 순으로 50개만 확인
w <- table(word_3)
head(sort(w, decreasing = T), 50)

## 남은 해결과제
# 1. '이명' -> '이명박' 등으로 바꾸는 작업 필요!
# 2. 특수문자는 거의 웬만한 건 다 저게되었으나, 아직 극히 일부 특수문자가 남아있음.
#    그래서 특수문자만 있는 리스트가 있는지 찾아볼 것!

wordcount_3 <- table(word_3)
head(sort(wordcount_2, decreasing = T),50)
View(sort(wordcount_3, decreasing = T))

#############################################################################
