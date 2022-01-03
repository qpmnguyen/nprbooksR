## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(rvest)
library(xml2)
library(robotstxt)
library(RSelenium)
library(glue)
library(stringr)

get_book_detail <- function(yr, index){
    driver$navigate(glue("https://apps.npr.org/best-books/#year={yr}&book={index}", yr = yr, index = index))
    html <- driver$getPageSource()[[1]]
    book_detail <- read_html(html) %>% html_nodes(".book-detail")
    tags <- book_detail %>% html_nodes(".tags") %>% html_nodes("a") %>% html_text()
    img <- book_detail %>% html_nodes(".cover") %>% rvest::html_attr("src") %>% str_replace(".", "https://apps.npr.org")
    author <- book_detail %>% html_nodes(".author") %>% html_text()
    extra <- book_detail %>% html_nodes(".text") %>% html_text() %>% stringr::str_trim() %>% strsplit(split = "—")
    text <- extra[[1]][1]
    blurb_auth <- extra[[1]][2] %>% str_trim()
    return(tibble(author = author, tags = list(tags), blurb = text, blurb_auth = blurb_auth, img = img))
    gc()
}


r_text <- get_robotstxt("https://apps.npr.org/best-books")
r_parsed <- parse_robotstxt(r_text)
delay <- r_parsed$crawl_delay
if (nrow(delay) == 0) {
    delay <- 5
}
# only local selenium works
rev <- rsDriver(port = 4444L, browser = "firefox")
driver <- rev$client

yrs <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

yrs_list <- vector(mode = "list", length = length(yrs))

for (i in seq_len(length(yrs))){
    print(glue("Current in year {yr}", yr = yrs[i]))
    driver$navigate(glue("https://apps.npr.org/best-books/#view=list&year={yr}", yr = yrs[i]))
    webElem <- driver$findElement(using = "class", "book-count")
    n_texts <- webElem$getElementText()[[1]]
    books <- vector(mode = "list", length = n_texts)
    for (j in seq_len(n_texts)){
        if (j %% 10 == 0){
            print(glue("Currently at {index} out of {total}", index = j, total = n_texts))
        }
        books[[j]] <- get_book_detail(yr = yrs[i], index = j)
    }
    books <- do.call(dplyr::bind_rows, books)
    yrs_list[[i]] <- books
}


data <- do.call(dplyr::bind_rows, yrs_list)
# TODO: Reviewer is nested under the section in text
data$blurb_auth

#usethis::use_data(DATASET, overwrite = TRUE)
