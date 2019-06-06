# Naver News Title 크롤링 전에 패키지 실행!
library(rvest)
library(dplyr)
library(N2H4)
library(stringr)
library(KoNLP)

#########################################################################################################################################################
### 17대 대선 ###

# 1. 17대 대선 날짜 출력
fdate1718 <- function(year) {
  fdate <- NULL
  mydate <<- list()
  day <- 1:31
  for(i in 1:length(month.abb)) {
    if(i %in% c(1, 3, 5, 7, 8)) {
      fdate <- c(fdate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:31])))
    } else if(i == 10) {
      fdate <- c(fdate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:31])))
    } else if(i %in% c(4, 6, 9)) {
      fdate <- c(fdate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:30])))
    } else if(i == 11) {
      fdate <- c(fdate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:30])))
    } else if(i == 2) {
      fdate <- c(fdate, as.numeric(paste0(year, 0, i, 0, day[1:9])), as.numeric(paste0(year, 0, i, day[10:28])))
    } else {
      fdate <- c(fdate, as.numeric(paste0(year, i, 0, day[1:9])), as.numeric(paste0(year, i, day[10:18])))
    }
    mydate[[i]] <<- subset(fdate, as.numeric(substr(fdate, 5, 6)) == i)
  }
  names(mydate) <<- month.abb
}

fdate1718(2007)
mydate


# 2. Naver News URL 설정

# 초기 URL 부분 설정
naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='

# URL 공통부분
section <- 100  

# 정치섹션 100
naver_url_2 <- '&date='

# URL 설정
naver_url.07 <- vector(mode = "list", length = 12)
for (i in 1:length(mydate)){
  for (j in 1:length(mydate[[i]])){
    naver_url.07[[i]][j] <- paste0(naver_url_1,section,naver_url_2,mydate[[i]][j])
  }
  names(naver_url.07) <- month.abb
}
naver_url.07

# Naver News Title Crawling
temp <- vector(mode = "list", length = 12)
for (i in 1:length(naver_url.07)){
  for(j in 1:length(naver_url.07[[i]])) {
    html <- read_html(naver_url.07[[i]][j])
    temp[[i]] <- c(temp[[i]], html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title'))
  }
}

# Result Check
View(temp)


# 3. Text Mining(제목 데이터 전처리)
sentence <- vector(mode = "list")
word_1 <- vector(mode = "list")
word_2 <- vector(mode = "list")
word_3 <- vector(mode = "list")

for(i in 1:length(temp)) {
  sentence[[i]] <- unlist(temp[i])
  word_1[[i]] <- sapply(sentence[[i]], extractNoun, USE.NAMES = F)
  word_2[[i]] <- unlist(word_1[[i]])
  
  word_3[[i]] <- sapply(word_2[[i]], str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
  word_3[[i]] <- sapply(unlist(word_3[i]), paste, USE.NAMES = F)
  word_3[[i]] <- Filter(function(x){x != ""}, word_3[[i]])
  word_3[[i]] <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3[[i]])
  word_3[[i]] <- word_3[[i]][ifelse(word_3 %in% as.character(0:9), F, T)]
}

View(word_3)


# 4. Barplot 확인
# 이명박
m <- NULL
for(i in 1:12) {
  m[i] <- length(which(word_3[[i]] == '李' | word_3[[i]] == '이명박' | word_3[[i]] == '이명' | word_3[[i]] == '명박'))
}

barplot(m, names.arg = month.abb, main = "이명박")

#########################################################################################################################################################
### 18대 대선 ###

# 1. 18대 대선 날짜 출력

fdate1718(2012)
mydate

# 2. Naver News URL 설정
naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
section <- 100
naver_url_2 <- '&date='

# URL 설정
naver_url.12 <- vector(mode = "list", length = 12)
for (i in 1:length(mydate)){
  for (j in 1:length(mydate[[i]])){
    naver_url.12[[i]][j] <- paste0(naver_url_1, section, naver_url_2, mydate[[i]][j])
  }
  names(naver_url.12) <- month.abb
}
naver_url.12

# Naver News Title Crawling
temp <- vector(mode = "list", length = 12)
for (i in 1:length(naver_url.12)){
  for(j in 1:length(naver_url.12[[i]])) {
    html <- read_html(naver_url.12[[i]][j])
    temp[[i]] <- c(temp[[i]], html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title'))
  }
}

# Result Check
View(temp)


# 3. Text Mining(제목 데이터 전처리)
sentence <- vector(mode = "list")
word_1 <- vector(mode = "list")
word_2 <- vector(mode = "list")
word_3 <- vector(mode = "list")

for(i in 1:length(temp)) {
  sentence[[i]] <- unlist(temp[i])
  word_1[[i]] <- sapply(sentence[[i]], extractNoun, USE.NAMES = F)
  word_2[[i]] <- unlist(word_1[[i]])
  
  word_3[[i]] <- sapply(word_2[[i]], str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
  word_3[[i]] <- sapply(unlist(word_3[i]), paste, USE.NAMES = F)
  word_3[[i]] <- Filter(function(x){x != ""}, word_3[[i]])
  word_3[[i]] <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3[[i]])
  word_3[[i]] <- word_3[[i]][ifelse(word_3 %in% as.character(0:9), F, T)]
}

View(word_3)


# 4. Barplot 확인
# 박근혜
p <- NULL
for(i in 1:12) {
  p[i] <- length(which(word_3[[i]] == '朴' | word_3[[i]] == '박근혜' | word_3[[i]] == '박근' | word_3[[i]] == '근혜'))
}

barplot(p, names.arg = month.abb, main = "박근혜")

#########################################################################################################################################################
### 19대 대선 ###

# 1. 19대 대선 날짜 출력
spdate <- function(year1 = 2016, year2 = 2017) {
  mydate <<- list()
  day <- 1:31
  for(i in c(12, 1:5)) {
    if(i == 12) {
      mydate[[1]] <<- c(as.numeric(paste0(year1, i, 0, day[9])), as.numeric(paste0(year1, i, day[10:31])))
    } else if(i == 1) {
      mydate[[2]] <<- c(as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:31])))
    } else if(i == 2) {
      mydate[[3]] <<- c(as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:28])))
    } else if(i == 3) {
      mydate[[4]] <<- c(as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:31])))
    } else if(i == 4) {
      mydate[[5]] <<- c(as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:30])))
    } else {
      mydate[[6]] <<- as.numeric(paste0(year2, 0, i, 0, day[1:9]))
    }
  }
  names(mydate) <<- c(month.abb[12], month.abb[1:5])
}

# Result Check
spdate()
mydate

# 2. Naver News URL 설정
naver_url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
section <- 100
naver_url_2 <- '&date='

# URL 설정
naver_url.17 <- vector(mode = "list", length = 6)
for (i in 1:length(mydate)){
  for (j in 1:length(mydate[[i]])){
    naver_url.17[[i]][j] <- paste0(naver_url_1, section, naver_url_2, mydate[[i]][j])
  }
  names(naver_url.17) <- c(month.abb[12], month.abb[1:5])
}
naver_url.17

# Naver News Title Crawling
temp <- vector(mode = "list", length = 6)
for (i in 1:length(naver_url.17)){
  for(j in 1:length(naver_url.17[[i]])) {
    html <- read_html(naver_url.17[[i]][j])
    temp[[i]] <- c(temp[[i]], html %>% html_nodes(css='.ranking') %>% html_nodes(css='.ranking_list') %>% html_nodes(css='.ranking_text') %>% html_nodes(css='.ranking_headline') %>% html_nodes('a') %>% html_attr('title'))
  }
}

# Result Check
View(temp)


# 3. Text Mining(제목 데이터 전처리)
sentence <- vector(mode = "list")
word_1 <- vector(mode = "list")
word_2 <- vector(mode = "list")
word_3 <- vector(mode = "list")

for(i in 1:length(temp)) {
  sentence[[i]] <- unlist(temp[i])
  word_1[[i]] <- sapply(sentence[[i]], extractNoun, USE.NAMES = F)
  word_2[[i]] <- unlist(word_1[[i]])
  
  word_3[[i]] <- sapply(word_2[[i]], str_split, c(split = paste('[ " “ ” , - ↑ ↓ > ) '," ‘ ’ ( · < . ' ]")), USE.NAMES = FALSE)
  word_3[[i]] <- sapply(unlist(word_3[i]), paste, USE.NAMES = F)
  word_3[[i]] <- Filter(function(x){x != ""}, word_3[[i]])
  word_3[[i]] <- Filter(function(x){!(nchar(x)<2 & is.hangul(x))}, word_3[[i]])
  word_3[[i]] <- word_3[[i]][ifelse(word_3 %in% as.character(0:9), F, T)]
}

View(word_3)


# 4. Barplot 확인
# 문재인
j <- NULL
for(i in 1:6) {
  j[i] <- length(which(word_3[[i]] == '文' | word_3[[i]] == '문재인' | word_3[[i]] == '문재' | word_3[[i]] == '재인'))
}

barplot(j, names.arg = c(month.abb[12], month.abb[1:5]), main = "문재인")