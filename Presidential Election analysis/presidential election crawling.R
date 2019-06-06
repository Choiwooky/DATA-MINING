## 17, 18대 대선 날짜 출력 함수
fdate1718 <- function(year) {
  mydate <<- NULL
  day <- 1:31
  for(i in 1:length(month.abb)) {
    if(i %in% c(1, 3, 5, 7, 8)) {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:31])))
    } else if(i == 10) {
      mydate <<- c(mydate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:31])))
    } else if(i %in% c(4, 6, 9)) {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:30])))
    } else if(i == 11) {
      mydate <<- c(mydate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:30])))
    } else if(i == 2) {
      mydate <<- c(mydate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:28])))
    } else {
      mydate <<- c(mydate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:18])))
    }
  }
}

# 변수 mydate에 날짜를 저장하고 전역변수로 만들었음.
# 날짜 사용은 함수를 실행하고 mydate를 호출하면 됨.

## 결과
# 2007년 대선(17대 대선)
fdate1718(2007)
mydate

# 2012년 대선(18대 대선)
fdate1718(2012)
mydate

## 박근혜 대통령 탄핵 가결일 2016/12/9 ~ 19대 대선 직전일 2017/5/9
spdate <- function(year1 = 2016, year2 = 2017) {
  mydate <- NULL
  day <- 1:31
  for(i in c(12, 1:5)) {
    if(i == 12) {
      mydate <- c(mydate, as.numeric(paste0(year1, i, 0, day[9])), as.numeric(paste0(year1, i, day[10:31])))
    } else if(i %in% c(1, 3)) {
      mydate <- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:31])))
    } else if(i == 2) {
      mydate <- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:28])))
    } else if(i == 4) {
      mydate <- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:30])))
    } else {
      mydate <- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])))
    }
  }
  date.0509 <<- mydate
}

# 변수 mydate에 날짜를 저장하고 전역변수로 만들었음.
# 날짜 사용은 함수를 실행하고 mydate를 호출하면 됨.

## 결과
spdate()
mydate

########################################################################################
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

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_211")
library(KoNLP)

##################################################################################
### 2017.05.09 (19대 대선) -> 문재인 당선
# 문재인, 홍준표, 안철수 ,박근혜

naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
# url 공통부분
section <- 100  
# 정치섹션 100
naver_url_2 <- '&date='

# 구하려는 날 연도월일 year mn dy
# 달 바뀔때는 조심할 것 (435 이런식으로 넘어감)

naver_url <- NULL
for (i in 1:length(date.0509)){
  naver_url[i] <- paste0(naver_url_1,section,naver_url_2,date.0509[i])
}

head(naver_url)


temp <- list()
for (i in 1:length(naver_url)){
  html <- read_html(naver_url[i])
  temp[[i]] <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title')
}

sentence <- unlist(temp)
word_1 <- sapply(sentence, extractNoun, USE.NAMES = F)
word_2 <- unlist(word_1)
wordcount_2 <- table(word_2)
head(sort(wordcount_2, decreasing = T), 20) 

word_3 <- sapply(word_2, str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
word_3 <- sapply(unlist(word_3), paste, USE.NAMES = F)
word_3 <- Filter(function(x){x != ""}, word_3)
word_3 <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3)
word_3 <- word_3[ifelse(word_3 %in% as.character(0:9), F, T)]
word_3 

w <- table(word_3)
head(sort(w, decreasing = T), 100)

wordcount_3 <- table(word_3)
head(sort(wordcount_3, decreasing = T),50)
View(sort(wordcount_3, decreasing = T))

m <- which(word_2 == '文' | word_2 == '문재인' | word_2 == '문재' | word_2 == '재인')
plot(m)
boxplot(m)
hist(m,xlim = c(1, length(word_2)))
length(m)

p <- which(word_2 == '朴' | word_2 == '박근혜' | word_2 == '박근' | word_2 == '근혜')
plot(p)
boxplot(p)
hist(p,xlim = c(1, length(word_2)))
length(p)

h <- which(word_2 == '洪' | word_2 == '홍준표' | word_2 == '홍준' | word_2 == '준표')
plot(h)
boxplot(h)
hist(h,xlim = c(1, length(word_2)))
length(h)

a <- which(word_2 == '安' | word_2 == '안철수' | word_2 == '안철' | word_2 == '철수')
plot(a)
boxplot(a)
hist(a,xlim = c(1, length(word_2)))
length(a)


#########################################################################################
### 2007년 대선(17대 대선) -> 이명박 당선
# 이명박, 정동영, 이회창

fdate1718(2007)
mydate


naver_url.07 <- NULL
for (i in 1:length(mydate)){
  naver_url.07[i] <- paste0(naver_url_1,section,naver_url_2,mydate[i])
}

head(naver_url.07)

temp <- list()
for (i in 1:length(naver_url.07)){
  html <- read_html(naver_url.07[i])
  temp[[i]] <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title')
}

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
word_3 

w <- table(word_3)
head(sort(w, decreasing = T), 50)

# 이명박
m <- which(word_2 == '李' | word_2 == '이명박' | word_2 == '이명' | word_2 == '명박' )
plot(m)
boxplot(m)
hist(m,xlim = c(1, length(word_2)))
length(m)

# 박근혜
p <- which(word_2 == '朴' | word_2 == '박근혜' | word_2 == '박근' | word_2 == '근혜')
plot(p)
boxplot(p)
hist(p,xlim = c(1, length(word_2)))
length(p)

# 정동영
j <- which(word_2 == '鄭' | word_2 == '정동영' | word_2 == '정동' | word_2 == '동영'  )
plot(j)
boxplot(j)
hist(j,xlim = c(1, length(word_2)))
length(j)

# 이회창
a <- which(word_2 == '昌 ' | word_2 == '이회창' | word_2 == '이회' | word_2 == '회창')
plot(a)
boxplot(a)
hist(a,xlim = c(1, length(word_2)))
length(a)

# 손학규
s <- which(word_2 == '孫' | word_2 == '손학규' | word_2 == '손학' | word_2 == '학규')
plot(s)
boxplot(s)
hist(s,xlim = c(1, length(word_2)))
length(s)

#########################################################################################
### 2012년 대선(18대 대선) -> 박근혜 당선
# 박근혜, (안철수), 문재인
fdate1718(2012)
mydate

naver_url.12 <- NULL
for (i in 1:length(mydate)){
  naver_url.12[i] <- paste0(naver_url_1,section,naver_url_2,mydate[i])
}

head(naver_url.12)

temp <- list()
for (i in 1:length(naver_url.12)){
  html <- read_html(naver_url.12[i])
  temp[[i]] <- html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title')
}

sentence <- unlist(temp)
word_1 <- sapply(sentence, extractNoun, USE.NAMES = F)
word_2 <- unlist(word_1)
wordcount_2 <- table(word_2)
head(sort(wordcount_2, decreasing = T), 20) 

word_3 <- sapply(word_2, str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
word_3 <- sapply(unlist(word_3), paste, USE.NAMES = F)
word_3 <- Filter(function(x){x != ""}, word_3)
word_3 <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3)
word_3 <- word_3[ifelse(word_3 %in% as.character(0:9), F, T)]
word_3 

w <- table(word_3)
head(sort(w, decreasing = T), 100)

# 박근혜
p <- which(word_2 == '朴' | word_2 == '박근혜'|word_2 == '박근' | word_2 == '근혜')
plot(p)
boxplot(p)
hist(p,xlim = c(1, length(word_2)))
length(p)

# 안철수
a <- which(word_2 == '安' | word_2 == '안철수' | word_2 == '철수' | word_2 == '안철' )
plot(a)
boxplot(a)
hist(a,xlim = c(1, length(word_2)))
length(a)

# 문재인
j <- which(word_2 == '文' | word_2 == '문재인' | word_2 == '문재' | word_2 == '재인')
plot(j)
boxplot(j)
hist(j,xlim = c(1, length(word_2)))
length(j)


