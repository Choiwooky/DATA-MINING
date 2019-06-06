## 17, 18대 대선 날짜 출력 함수
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

# 변수 mydate에 날짜를 저장하고 전역변수로 만들었음. list 형태임.
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

# 변수 mydate에 날짜를 저장하고 전역변수로 만들었음. list 형식임.
# 날짜 사용은 함수를 실행하고 mydate를 호출하면 됨.
  
## 결과
spdate()
mydate
