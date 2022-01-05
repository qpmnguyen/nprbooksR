## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(rvest)
library(xml2)
library(robotstxt)
library(RSelenium)
library(glue)
library(stringr)
library(httr)
library(lubridate)

get_book_detail <- function(yr, index){
    driver$navigate(glue("https://apps.npr.org/best-books/#year={yr}&book={index}", yr = yr, index = index))
    html <- driver$getPageSource()[[1]]
    book_detail <- read_html(html) %>% html_nodes(".book-detail")
    tags <- book_detail %>% html_nodes(".tags") %>% html_nodes("a") %>% html_text()
    img_url <- book_detail %>% html_nodes(".cover") %>% rvest::html_attr("src") %>% str_replace(".", "https://apps.npr.org")
    title <- book_detail %>% html_nodes(".title") %>% html_text() %>% stringr::str_trim() %>% stringr::str_to_title()
    authors <- book_detail %>% html_nodes(".author") %>% html_text()
    extra <- book_detail %>% html_nodes(".text") %>% html_text() %>% stringr::str_trim() %>%
        strsplit(split = "—")
    b_auth <- extra[[1]][length(extra[[1]])] %>% str_trim() %>%
        str_split(pattern = ",") %>% .[[1]]
    blurb_auth <- b_auth[1] %>% str_trim()
    blurb_auth_role <- str_trim(paste(b_auth[2], b_auth[3], sep = ", "))
    isbn10 <- str_split(img_url, pattern = "/") %>% .[[1]] %>% .[length(.)] %>%
        str_split(pattern = ".jpg") %>% .[[1]] %>% .[1]

    text <- extra[[1]][-length(extra[[1]])] %>% str_trim() %>% str_c(collapse = "-")

    return(tibble(title = title,
                  authors = authors, tags = list(tags),
                  isbn10 = isbn10,
                  blurb = text,
                  blurb_auth = blurb_auth,
                  blurb_auth_role = blurb_auth_role,
                  img_url = img_url,
                  yr_list = yr))
    gc()
}
#' @title Given a row of books as tibble, return corresponding google data
#' @description All queries are done via isbn10 so hopefully nothing will be wrong
#' @param tibble_info The row of book information as a tibble row.
#'     Will return error if tibble_row has more than one row.
get_google_info <- function(isbn10, delay = 2){
    r <- httr::GET(glue("https://www.googleapis.com/books/v1/volumes?q={a}", a = isbn10))
    if (status_code(r) != 200 | content(r)$totalItems == 0){
        res <- tibble(
            publishers = NA_character_,
            published_date = NA_character_,
            gbooks_desc = NA_character_,
            page_count = NA_integer_,
            categories = list(NA_character_),
            average_rating = NA_real_,
            ratings_count = NA_integer_,
            maturity_rating = NA_character_,
            language = NA_character_
        )
    } else {
        search_content <- content(r)[[3]][[1]][["volumeInfo"]]
        res <- tibble(
            publisher = search_content[["publisher"]],
            published_date = search_content[["publishedDate"]],
            gbooks_desc = str_trim(search_content[["description"]]),
            page_count = search_content[["pageCount"]],
            categories = search_content[["categories"]],
            average_rating = search_content[["averageRating"]],
            ratings_count = search_content[["ratingsCount"]],
            maturity_rating = search_content[["maturityRating"]],
            language = search_content[["language"]],
            price_USD = content(r)[[3]][[1]][["saleInfo"]][["retailPrice"]][["amount"]]
        )
    }
    Sys.sleep(delay)
    return(res)
}

r_text <- get_robotstxt("https://apps.npr.org/")
r_parsed <- parse_robotstxt(r_text)
delay <- r_parsed$crawl_delay
if (nrow(delay) == 0) {
    delay <- 5
}
# only local selenium works
rev <- rsDriver(port = 4444L, browser = "firefox")
driver <- rev$client

yrs <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

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


nprbooks <- do.call(dplyr::bind_rows, yrs_list)

usethis::use_data(nprbooks, overwrite = TRUE)
