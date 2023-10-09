# Load
library(pacman)
pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  rvest,
  httr,
  polite,
  data.table,
  glue
)

### Make url string


make_url_date <- function(year) {
  ## Output: f=01%2F01%2F2023&to=31%2F12%2F2023

  url_date <- glue("f=01%2F01%2F{year}&to=31%2F12%2F{year}")

  return(url_date)
}

make_url_term <- function(search_term) {
  ## Output: term+term

  url_term <- str_replace(search_term, " ", "+")

  return(url_term)
}

make_url_title <- function(hansard_title) {
  hansard_title <- if (hansard_title == TRUE) {
    hto <- "hto=1"
  } else {
    hto <- ""
  }
  return(hto)
}


make_url <- function(year,
                     search_term,
                     hansard_title = FALSE) {
  url_date <- make_url_date(year)
  url_term <- make_url_term(search_term)
  url_hto <- make_url_title(hansard_title)

  glue("https://www.aph.gov.au/Parliamentary_Business/Hansard/Search?__VIEWSTATEGENERATOR=20B6B7A5&ind=0&st=1&sr=0&q=%22{url_term}%22&expand=False&drvH=0&pnuH=0&{url_date}&pi=0&pv=&chi=0&coi=0&ps=10")
}

scrape_hansard_results <- function(year, search_term) {
  url <- make_url(year, search_term)

  url_bow <- polite::bow(url)

  url_content <-
    polite::scrape(url_bow) %>%
    html_element(css = "#main_0_content_0_pTotalResults") %>%
    html_text()

  results <- gsub("[^0-9.-]", "", url_content)

  results_df <- data.frame(
    "year" = year,
    "total_results" = results
  )

  return(results_df)
}


search_map <- map_dfr(2022:2023, scrape_hansard_results, search_term = "John Howard")
