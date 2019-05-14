if (!requireNamespace("DNH4")){
  source("https://install-github.me/forkonlp/DNH4")
}

library(DNH4)

# url <- "http://v.media.daum.net/v/20180513202105651"
url <- "https://news.v.daum.net/v/20190404074531633"
k <- DNH4::getComment(url, limit = "all")
