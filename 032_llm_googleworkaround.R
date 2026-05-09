library(httr)
library(xml2)

get_live_news <- function(ticker, days) {
  # Clean ticker to avoid symbols that confuse Google Search
  clean_ticker <- str_split(ticker, "\\.")[[1]][1]
  url <- paste0("https://news.google.com/rss/search?q=", clean_ticker, 
                "+stock+when:", days, "d&hl=en-US&gl=US&ceid=US:en")
  
  # User-Agent makes you look like a real browser, not a script
  u_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  
  success <- FALSE
  while(!success) {
    res <- GET(url, u_agent)
    stat <- status_code(res)
    
    if(stat == 200) {
      success <- TRUE
    } else if (stat %in% c(429, 403)) {
      cat(sprintf("\n[!] Blocked (Status %s). Waiting 30s to retry...", stat))
      Sys.sleep(30) # Wait 30 seconds before trying again
    } else {
      return("News source error.") # Other errors (404, etc) shouldn't loop forever
    }
  }
  
  # Once successful, parse the XML
  rss_data <- read_xml(content(res, "raw"))
  titles <- xml_find_all(rss_data, ".//title") %>% xml_text()
  
  recent_headlines <- paste(head(titles[-1], 15), collapse = " | ")
  
  return(if(nchar(recent_headlines) > 10) recent_headlines else paste0("No major news in last ", days, " days."))
}
