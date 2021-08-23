pacman::p_load(RSelenium, rvest, googlesheets4, tidyverse, tibble, glue)

sheet_url <- "<SHEET URL>" #edit this after you make a google sheet... sheet needs to have two tabs (diffcounter and data I called them), would add the column names too (page_links etc. in the order of the tibble at the bottom)

driver <- rsDriver(browser=c("firefox"), port= 4833L, extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless')))) #remove this extra capability if you want to see the remote browser during the scrape
remote_driver <- driver[["client"]]
remote_driver$open()

month2020 <- month.name #use the keywords you want, with the years and months you want, it's like this because the bot needs to click "load more" for each query, and if you don't do it bins like this (key word, month, year) it will crash as it loads way too many items. This is using a combination of keyterm, province, month, year, and only adding the non-duplicates to the google sheet
month2021 <- month.name
month2021 <- month2021[month2021 != c("July", "August", "September", "October", "November", "December")]

base_urls1 <- paste("https://www.cbc.ca/search?q=covid%20", 2020, "%20", month2020, "%20", "Ontario", "&section=news&sortOrder=date&media=all", sep = "")
base_urls2 <- paste("https://www.cbc.ca/search?q=coronavirus%20", 2021, "%20", month2021, "%20", "Ontario", "&section=news&sortOrder=date&media=all", sep = "")
base_urls3 <- paste("https://www.cbc.ca/search?q=covid%20", 2021, "%20", month2021, "%20", "Ontario", "&section=news&sortOrder=date&media=all", sep = "")
base_urls4 <- paste("https://www.cbc.ca/search?q=coronavirus%20", 2020, "%20", month2020, "%20", "Ontario", "&section=news&sortOrder=date&media=all", sep = "")

base_urls <- c(base_urls1, base_urls2, base_urls3, base_urls4)

for(b in base_urls) {
  
  print("Reading id_sheet...")
  
  id_sheet <- suppressMessages(read_sheet(sheet_url, sheet = 'diffcounter'))
  
  id_sheet$page_links <- as.character(id_sheet$page_links)
  
  print("Opening remote driver...")
  
  remote_driver$open()
  
  print(glue("Navigating to {b}..."))
  
  remote_driver$navigate(b)
  
  Sys.sleep(5)
  
  total_results <- remote_driver$findElement(using = "css", value = "strong:nth-child(2)")$getElementText() %>%
    as.numeric()
  
  total_clicks <- round(total_results/10, 0)
  
  print(glue("Loading all pages in {b}..."))
  
  replicate(total_clicks,
            {
              tryCatch({
                webElem <- remote_driver$findElement(using = "css", value = "body")
                
                webElem$sendKeysToElement(list(key = "end"))
                
                Sys.sleep(15) #incredibly slow rn, probably want to change this
                
                load_more <- remote_driver$findElement(using = "class", value = 'loadMore')
                
                load_more$clickElement()
                
                Sys.sleep(15) #incredibly slow rn, probably want to change this
              },
              error = function(e){
                print(glue("Failed to find button on {b}"))
              }
              )
            })
  
  print(glue("Getting data in {b}..."))
  
  page <- read_html(remote_driver$getPageSource()[[1]])
  
  page_links <- page %>%
    html_nodes(".rightImage") %>%
    html_attr('href') %>%
    url_absolute("https://www.cbc.ca/search?q=crime&section=news&sortOrder=relevance&media=all")
  
  image_links <- page %>%
    html_nodes("#content .placeholder img") %>%
    html_attr('src') %>%
    url_absolute("https://www.cbc.ca/search?q=crime&section=news&sortOrder=relevance&media=all")
  
  headlines <- page %>%
    html_nodes("#h-card-") %>%
    html_text(trim = TRUE)
  
  abstract <- page %>%
    html_nodes("#d-card-") %>%
    html_text(trim = TRUE)
  
  date <- page %>%
    html_nodes(".timeStamp") %>%
    html_text(trim = TRUE)
  
  data_to_append <- tibble(
    page_links = page_links,
    image_links = image_links,
    headlines = headlines,
    abstract = abstract,
    date = date
  ) %>%
    anti_join(id_sheet, by = 'page_links')
  
  ids_to_append <- data_to_append %>%
    select(page_links)
  
  print(glue("Saving data to {sheet_url}..."))
  
  sheet_append(sheet_url, data_to_append, sheet = 'data')
  
  sheet_append(sheet_url, ids_to_append, sheet = 'diffcounter')
}
