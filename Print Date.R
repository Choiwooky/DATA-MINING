# 17, 18대 대선 날짜 출력 함수
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

# 2007년 대선(17대 대선)
fdate1718(2007)
mydate

# 2012년 대선(18대 대선)
fdate1718(2012)
mydate
