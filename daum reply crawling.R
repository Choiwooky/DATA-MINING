if (!requireNamespace("DNH4")){
  source("https://install-github.me/forkonlp/DNH4")
}
# DNH4 : 다음 댓글 크롤링을 위한 패키지
if(!require(dplyr)){
  install.packages('dplyr')
}
# dplyr : 전처리를 위한 패키지

library(DNH4)
library(dplyr)

#######################################################################
### 하나의 다음기사 url에 따른 댓글 크롤링

url <- "https://news.v.daum.net/v/20190404074531633"
# url : 임의의 url입력

k <- DNH4::getComment(url, limit = "all")
# k : url의 모든 댓글과 다양한 정보를 추출
k1 <- k %>% select(content, likeCount)
# k1 : content(댓글), likeCount(공감수)를 추출하여 저장
k2 <- k1 %>% arrange(desc(likeCount))
# k2 : likeCount(공감수)를 내림차순으로 정렬
write.csv(k2, "sample.csv", row.names = TRUE)

#######################################################################
### dplyr 패키지를 이용해 간편하게 합쳐서 표현

k <- DNH4::getComment(url, limit = "all")
k.all <- k %>% select(content, likeCount) %>% arrange(desc(likeCount))
write.csv(k.all, "sample.csv", row.name = TRUE)
