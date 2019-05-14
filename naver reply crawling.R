if (!requireNamespace("N2H4")){
  source("https://install-github.me/forkonlp/N2H4")
}
if(!require(dplyr)){
  install.packages('dplyr')
}


library(N2H4)
library(dplyr)

# url <- "https://news.naver.com/main/read.nhn?m_view=1&includeAllCount=true&mode=LSD&mid=shm&sid1=101&oid=025&aid=0002894699"
# 임의의 url하나의 댓글을 뽑아 낼 때


k <- getAllComment(url)
# k에 N2H4패키지를 이용하여 기사의 댓글과 제목 등 다양한 정보 추출
k.con <- k %>% select(contents, sympathyCount)
# k.con에 k에 있는 데이터 중 contents(댓글), sympathyCount(공감수)만 추출
raw.data <- k.con %>% arrange(desc(sympathyCount))
# raw.data_age에 k.con을 공감순으로 내림차순 한 후 정렬

## setwd 이용 후 저장할 위치 확인 후 csv파일 작성
## write.csv(raw_data, file = "sample.csv", row.names = TRUE)
## 많은 양의 데이터를 한꺼번에 묶을 때는 bind_rows 이용 

### 패키지 출처 https://forkonlp.github.io/N2H4/
