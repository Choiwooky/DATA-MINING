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

    
###########################################################################################################################
    
## 박근혜 대통령 탄핵 가결일 2016/12/9 ~ 19대 대선 직전일 2017/5/9
spdate <- function(year1 = 2016, year2 = 2017) {
  mydate <<- NULL
  day <- 1:31
  for(i in c(12, 1:5)) {
    if(i == 12) {
      mydate <<- c(mydate, as.numeric(paste0(year1, i, 0, day[9])), as.numeric(paste0(year1, i, day[10:31])))
    } else if(i %in% c(1, 3)) {
      mydate <<- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:31])))
    } else if(i == 2) {
      mydate <<- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:28])))
    } else if(i == 4) {
      mydate <<- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])), as.numeric(paste0(year2, 0, i, day[10:30])))
    } else {
      mydate <<- c(mydate, as.numeric(paste0(year2, 0, i, 0, day[1:9])))
    }
  }
}

# 변수 mydate에 날짜를 저장하고 전역변수로 만들었음.
# 날짜 사용은 함수를 실행하고 mydate를 호출하면 됨.
  
## 결과
spdate()
mydate
