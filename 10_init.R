get_live_news <- function(ticker, days) {
  # Construct the Google News RSS URL for the ticker
  # 'when:1d' limits results to the last 24 hours
  url <- paste0("https://news.google.com/rss/search?q=", ticker, "+stock+when:", days,"d&hl=en-US&gl=US&ceid=US:en")
  
  tryCatch({
    rss_data <- read_xml(url)
    titles <- xml_find_all(rss_data, ".//title") %>% xml_text()
    
    # We only take the top 5 headlines to keep the prompt small
    recent_headlines <- paste(head(titles[-1], 15), collapse = " | ")
    return(if(nchar(recent_headlines) > 10) recent_headlines else "No major news in last", days, "days.")
  }, error = function(e) return("News source unavailable."))
}

get_global_macro_news <- function(days) {
  # We search for broad terms that affect all asset classes (FX, Equities, Commodities)
  query <- paste0("geopolitics+OR+macroeconomics+OR+inflation+OR+central+banks+when:",days,"d")
  url <- paste0("https://news.google.com/rss/search?q=", query, "&hl=en-US&gl=US&ceid=US:en")
  
  tryCatch({
    rss_data <- read_xml(url)
    titles <- xml_find_all(rss_data, ".//title") %>% xml_text()
    
    # We take a larger sample (top 20) because these are global events
    macro_headlines <- paste(head(titles[-1], 20), collapse = " | ")
    return(macro_headlines)
  }, error = function(e) return("Global macro news unavailable."))
}