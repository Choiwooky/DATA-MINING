######################################################################
### 연령별 워드 클라우드

if (!requireNamespace("N2H4")){
  source("https://install-github.me/forkonlp/N2H4")
}
# 네이버 댓글 패키지 설치
if(!require(stringr)){
  install.packages("stringr")
}
# stringr : 문자열을 가능한 한 쉽게 처리하도록 설계 함수 세트를 제공
if(!require(rvest)){
  install.packages('rvest')
}
# rveset : 크롤링을 위한 설계 함수 제공
if(!require(dplyr)){
  install.packages('dplyr')
}
# dplyr : 데이터 전처리를 위한 설계 함수 제공
if(!require(KoNLP)){
  install.packages("KoNLP")
}
# KoNLP : 한국어를 분석가능한 27가지 함수 제공
library(N2H4)
library(stringr)
library(rvest)
library(dplyr)
library(KoNLP)
# rJAVA 설치 필요

myurl_age <- function(age){
  age <- readline(prompt = "연령대: ")
  url_age <<- paste0("https://news.naver.com/main/ranking/popularDay.nhn?rankingType=age&subType=",age)
}

# 연령별 url뽑기

html_age <- read_html(url_age)
temp0 <- html_age %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('href')

# 1위~10위 url

char_1 <- 'https://news.naver.com'
aged_url <- NULL
for(i in 1:length(temp0)){
  aged_url[i] <- paste0(char_1,temp0[i])
}

head(aged_url)

# aged_url : 연령별 많이본 뉴스 url


raw.data_age <- list()
for(i in 1:10){
  url <- aged_url[i]
  k <- getAllComment(url)
  k.con <- k %>% select(contents, sympathyCount)
  raw.data_age[[i]] <- k.con %>% arrange(desc(sympathyCount))
}

# raw.data_age : 연령별 url에 따른 댓글 추출

head(raw.data_age[[1]])

total <- NULL
for (i in 1:10){
  total <- bind_rows(total,raw.data_age[[i]][1:300,])  
}
sample <- total %>% filter(!is.na(contents))
# sample : 총 댓글수가 300개가 안되는 기사를 고려해 결측치 제거
head(sample)

useSejongDic()
# sejong dictiony 부착

sentence <- sample$contents
# sectence : sample 데이터에 contents 만 저장
head(sentence)

word_1 <- sapply(sentence, extractNoun, USE.NAMES = F)
# word_1 : 각각의 문장에 KoNLP내장함수 extractNoun적용
# sapply : 벡터, 리스트, 표현식, 데이터 프레임 등에 함수를 적용하고 그 결과를 벡터 또는 행렬로 반환한다.
head(word_1)

word_2 <- unlist(word_1)
# word_2 : unlist로 리스트를 품
length(word_2)
wordcount_2 <- table(word_2)
wordcount_2
head(sort(wordcount_2,decreasing = T),20)

word_3 <- Filter(function(x){nchar(x)>=2}, word_2)
# word_3 : 문자열 수가 2미만인 경우 제거
length(word_3)
wordcount_3 <- table(word_3)
head(sort(wordcount_3, decreasing = T),20)

word_4 <- str_replace_all(word_3,'[A-zㄱ-ㅎ0-9.]','')
# word_4 : a~z, ㄱ~ㅎ, 0~9, . 제거
wordcount_4 <- table(word_4)
head(sort(wordcount_4, decreasing = T),20)

word_5 <- str_replace_all(word_4,'진짜','')
word_6 <- str_replace_all(word_5,'하게','')
word_7 <- str_replace_all(word_6,'해서','')
word_8 <- str_replace_all(word_7,'들이','')
# 의미없지만 많이 들어간 단어 제거 
head(word_8)
wordcount_8 <- table(word_8)
head(sort(wordcount_8, decreasing = T),20)

if(!require(wordcloud)){
  install.packages('wordcloud')
}
# wordcloud : 워드클라우드 생성 패키지 

library(wordcloud)

# 워드클라우드작성
# freq : 빈도는 언급된 단어수
# scale : 폰트 사이즈 조정
# rot.per : 수직 텍스트의 비율 조정
# min.freq : 최소 몇번의 언급된 단어만 추출
# random.order : 순서 랜덤
# random.color : 컬러 랜덤

wordcloud(names(wordcount_8),freq=wordcount_8,scale=c(25,0.3),rot.per=.1,min.freq=10,random.order=F,random.color=T)

#######################################################################