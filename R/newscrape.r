library(rvest, quietly = TRUE)
symbol <- "VWIUX"
url <- paste0("https://finance.yahoo.com/quote/",symbol,"?p=",symbol)
webpage <- read_html(url)
result <- html_nodes(webpage, "#quote-summary")

#Top Holdings
url2 <- paste0("https://finance.yahoo.com/quote/",symbol,"/holdings?p=",symbol)
webpage2 <- read_html(url2)
result2 <- html_nodes(webpage2, "#YDC-Col1")
result2a <- html_nodes(result2, "table") %>% html_table()
result2a

url2 <- paste0("https://finance.yahoo.com/quote/",symbol,"/holdings?p=",symbol)
webpage2 <- read_html(url2)
result2b <- html_nodes(webpage2,"span")
result2bb <- html_nodes(result2b, "table") %>% html_table()
result2bb

url3 <- paste0("https://finance.yahoo.com/quote/",symbol,"/holdings?p=",symbol)
webpage3 <- read_html(url3)
result3 <- webpage3 %>% html_nodes(xpath='//*[(@id = "#Main")]') %>% xml_attr("value")

# //*[(@id = "Main")]
result5<-sapply(result3,html_text)

holdings <- paste0("https://finance.yahoo.com/quote/",symbol,"/holdings?p=",symbol) %>%
    read_html() %>% 
    html_nodes("span") %>%
    sapply(html_text)

