model_name <- "llama3"

cat("\n[1/4] Fetching Global Macro backdrop (14 days)...")

global_macro_context <- get_global_macro_news(days=14)

cat("\n[2/4] Calculating 14-day & 30-day performance metrics...")

context_data <- returns_xts %>%
  tk_tbl() %>%
  pivot_longer(-index, names_to = "Ticker", values_to = "Return") %>%
  group_by(Ticker) %>%
  summarise(
    # Last 30 Days (Existing)
    perf_30d = round(sum(tail(Return, 30)) * 100, 2),
    vol_30d  = round(sd(tail(Return, 30)) * sqrt(252) * 100, 2),
    
    # Last 14 Days (New)
    perf_14d = round(sum(tail(Return, 14)) * 100, 2),
    vol_14d  = round(sd(tail(Return, 14)) * sqrt(252) * 100, 2),
    
    momentum = ifelse(sum(tail(Return, 14)) > 0, "Rising", "Falling"),
    .groups = "drop"
  )


#get news for one day
news_save <- data.frame(Ticker = character(0), NewsText = character(0), stringsAsFactors = FALSE)

for(i in 1:nrow(assets)) {
  tk_orig <- assets$Ticker[i]
  
  #map tickers to google searches for accurate responses
  tk_search <- case_when(
    tk_orig == "DBXN.DE" ~ "Eurozone interest rates ECB policy",
    tk_orig == "GC=F"   ~ "Gold price",
    tk_orig == "SI=F"   ~ "Silver price",
    tk_orig == "CL=F"   ~ "Crude Oil WTI",
    tk_orig == "BZ=F"   ~ "Brent Crude Oil",
    tk_orig == "NG=F"   ~ "Natural Gas price",
    tk_orig == "HG=F"   ~ "Copper price",
    tk_orig == "TIO=F"  ~ "Iron Ore",
    tk_orig == "ZC=F"   ~ "Corn price",
    tk_orig == "ZW=F"   ~ "Wheat price",
    tk_orig == "ZS=F"   ~ "Soybeans price",
    tk_orig == "KC=F"   ~ "Coffee price",
    tk_orig == "SB=F"   ~ "Sugar price",
    tk_orig == "PL=F"   ~ "Platinum price",
    tk_orig == "PA=F"   ~ "Palladium price",
    tk_orig == "CC=F"   ~ "Cocoa price",
    tk_orig == "CT=F"   ~ "Cotton price",
    tk_orig == "ALI=F"  ~ "Aluminum price",
    TRUE ~ tk_orig %>% str_replace_all("=X|\\.NS|\\.KS|\\.L", "")
  )
  
  tk_search <- URLencode(tk_search)
    ticker_news <- get_live_news(tk_search, days=14)
    news_save <- rbind(news_save, data.frame(
    Ticker = tk_orig, 
    NewsText = ticker_news, 
    stringsAsFactors = FALSE
  ))
  
  cat(sprintf("[%d/%d] Success: %s\n", i, nrow(assets), tk_orig))
}

