#test retrieve data https://stackoverflow.com/questions/46224378/how-to-submit-a-form-that-seems-to-be-handled-by-javascript-using-httr-or-rvest/46756323#46756323 


library(rvest)

BASE_URL = 'https://anrweb.vermont.gov/dec/_dec/LongTermMonitoringLakes.aspx'
PARAMS = list(drpdwnSite='02 - South Lake B', 
              drpdwnMinYear=1992, 
              drpdwnMinYear=2017, 
              drpdwnStatus='All',
              submit='View Selected Data')

library(httr)
library(rvest)
library(tidyverse)

pre_pg <- read_html("https://anrweb.vermont.gov/dec/_dec/LongTermMonitoringLakes.aspx")

setNames(
  html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("value"),
  html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("name")
) -> hidden

str(hidden)
## Named chr [1:3] "x62pLbphYWUDXsdoNdBBNrxqyHHI+K06BzjFwdP3Uooafgey2uG1gLWxzh07djRxiQR724uplZFAI8klbq6HCSkmrp8jP15EMwvkDM/biUEuQrf"| __truncated__ ...
## - attr(*, "names")= chr [1:3] "__VIEWSTATE" "__VIEWSTATEGENERATOR" "__EVENTVALIDATION"

POST(
  url = "https://anrweb.vermont.gov/dec/_dec/LongTermMonitoringLakes.aspx", 
  add_headers(
    Origin = "https://anrweb.vermont.gov/dec", 
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.52 Safari/537.36", 
    Referer = "https://anrweb.vermont.gov/dec/_dec/LongTermMonitoringLakes.aspx"
  ), 
  body = list(
    `__EVENTTARGET` = "", 
    `__EVENTARGUMENT` = "", 
    `__VIEWSTATE` = hidden["__VIEWSTATE"],
    `__VIEWSTATEGENERATOR` = hidden["__VIEWSTATEGENERATOR"],
    `__EVENTVALIDATION` = hidden["__EVENTVALIDATION"],
    drpdwnSite='02 - South Lake B', 
    drpdwnMinYear=1992, 
    drpdwnMinYear=2017, 
    drpdwnStatus='All',
    submit='View Selected Data'

  ), 
  encode = "form"
) -> res


mcga <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  make.unique(x, sep = "_")
}

pg <- content(res, as="parsed")
col_names <- html_nodes(pg, "a.headings") %>% html_text(trim=TRUE) %>% mcga()


records <- html_nodes(pg, "div.offenderRow")

map(sprintf(".//div[@class='span1 searchCol%s']", 1:12), ~{
  html_nodes(records, xpath=.x) %>% html_text(trim=TRUE)
}) %>% 
  set_names(col_names) %>% 
  bind_cols() %>% 
  readr::type_convert() -> xdf

xdf
glimpse(xdf)

xml_integer(html_nodes(pg, "span#lblPgCurrent"))
xml_integer(html_nodes(pg, "span#lblTotalPgs"))

html_nodes(pg, "input[type='hidden']")


### Original code below
library(rvest)

BASE_URL = 'https://mdocweb.state.mi.us/otis2/otis2.aspx'
PARAMS = list(txtboxLName='Smith', 
              drpdwnGender='Either', 
              drpdwnRace='All', 
              drpdwnStatus='All',
              submit='btnSearch')

library(httr)
library(rvest)
library(tidyverse)

pre_pg <- read_html("https://mdocweb.state.mi.us/otis2/otis2.aspx")

setNames(
  html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("value"),
  html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("name")
) -> hidden

str(hidden)
## Named chr [1:3] "x62pLbphYWUDXsdoNdBBNrxqyHHI+K06BzjFwdP3Uooafgey2uG1gLWxzh07djRxiQR724uplZFAI8klbq6HCSkmrp8jP15EMwvkDM/biUEuQrf"| __truncated__ ...
## - attr(*, "names")= chr [1:3] "__VIEWSTATE" "__VIEWSTATEGENERATOR" "__EVENTVALIDATION"

POST(
  url = "https://mdocweb.state.mi.us/otis2/otis2.aspx", 
  add_headers(
    Origin = "https://mdocweb.state.mi.us", 
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.52 Safari/537.36", 
    Referer = "https://mdocweb.state.mi.us/otis2/otis2.aspx"
  ), 
  body = list(
    `__EVENTTARGET` = "", 
    `__EVENTARGUMENT` = "", 
    `__VIEWSTATE` = hidden["__VIEWSTATE"],
    `__VIEWSTATEGENERATOR` = hidden["__VIEWSTATEGENERATOR"],
    `__EVENTVALIDATION` = hidden["__EVENTVALIDATION"],
    txtboxLName = "Smith", 
    txtboxFName = "", 
    txtboxMDOCNum = "", 
    drpdwnGender = "Either", 
    drpdwnRace = "All", 
    txtboxAge = "", 
    drpdwnStatus = "All", 
    txtboxMarks = "", 
    btnSearch = "Search"
  ), 
  encode = "form"
) -> res


mcga <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  make.unique(x, sep = "_")
}

pg <- content(res, as="parsed")
col_names <- html_nodes(pg, "a.headings") %>% html_text(trim=TRUE) %>% mcga()


records <- html_nodes(pg, "div.offenderRow")

map(sprintf(".//div[@class='span1 searchCol%s']", 1:12), ~{
  html_nodes(records, xpath=.x) %>% html_text(trim=TRUE)
}) %>% 
  set_names(col_names) %>% 
  bind_cols() %>% 
  readr::type_convert() -> xdf

xdf
glimpse(xdf)

xml_integer(html_nodes(pg, "span#lblPgCurrent"))
xml_integer(html_nodes(pg, "span#lblTotalPgs"))

html_nodes(pg, "input[type='hidden']")
