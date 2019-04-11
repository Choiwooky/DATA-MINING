if (!requireNamespace("N2H4")){
  source("https://install-github.me/forkonlp/N2H4")
}

library(N2H4)

url <- "https://news.naver.com/main/read.nhn?m_view=1&includeAllCount=true&mode=LSD&mid=shm&sid1=101&oid=025&aid=0002894699"


k1 <- getComment(url, pageSize = 100, page = 1)
k2 <- getComment(url, pageSize = 100, page = 2)

k <- getAllComment(url)

