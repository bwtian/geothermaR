ge.urlTable  <- function(url, table = 1 ) {
        ## scrape the table to dataframe from a url website
      if(!require(XML)){
              installed.packages("XML")
      } else {
              url.doc <- htmlParse(url)
              url.l <- readHTMLTable(url.doc)
              url.d <- url.l[[table]]
      }
      return(url.d)
}
##  # url <- "http://ja.wikipedia.org/wiki/%E5%9C%B0%E7%86%B1%E7%99%BA%E9%9B%BB"
### ge.urlTable(url,3)
