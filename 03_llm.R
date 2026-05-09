cat("\n[3/4] Running LLM sentiment analysis for each asset...")
llm_results_list <- list()

for(i in 1:nrow(assets)) {
  tk <- assets$Ticker[i]
  nm <- assets$Name[i]
  ctx <- context_data %>% filter(Ticker == tk)
  
  if(nrow(ctx) == 0) next
  
  # Fetch 14-day headlines for this specific ticker
  
  #already fetched in llminit
  ticker_news <- news_save$NewsText[i]
  
  #data containers for averaging
  temp_returns <- c()
  temp_confs <- c()
  
  cat(sprintf("\n Analyzing [%d/%d] %s... ", i, nrow(assets), tk))
  
  # Construction of the Comprehensive Prompt
  prompt <- paste0(
    "GLOBAL CONTEXT (14d): ", global_macro_context, "\n\n",
    "ASSET: ", nm, " (", tk, ")\n",
    "STATS: 14d Perf: ", ctx$perf_14d, "%, 30d Perf: ", ctx$perf_30d, "%, 14d Vol: ", ctx$vol_14d, "%\n",
    "ASSET HEADLINES (14d): ", ticker_news, "\n\n",
    "TASK: Synthesize the global backdrop, recent price momentum, and ticker news for a 3-month forecast. Predict the 3-month total return.\n",
    "MANDATORY: Use decimal format (0.02) NOT percentages (2%). NO '%' SYMBOLS.\n",
    "FORMAT: You MUST return ONLY the following format: [[RETURN, CONFIDENCE]]\n",
    "Example: [[0.04, 0.7]]"
  )
  
  for(j in 1:5) {
    # Query Ollama
    tryCatch({
      res <- rollama::query(prompt, model = model_name)
      # Extract numbers inside [[ ]]
      clean_val <- str_extract(res[[1]]$message$content, "\\[\\[.*?\\]\\]") %>% 
        str_replace_all("\\[|\\]", "")
      vals <- as.numeric(unlist(str_split(clean_val, ",")))
      
      #safety thingy
      if(!is.na(vals[1]) && abs(vals[1]) > 1) vals[1] <- NA
      if(!is.na(vals[2]) && abs(vals[2]) > 1) vals[1] <- NA
      
      #only store values that are fine
      if(!is.na(vals[1]) && !is.na(vals[2])) {
        temp_returns <- c(temp_returns, vals[1])
        temp_confs <- c(temp_confs, vals[2])
        cat(".") # Visual progress indicator for each successful run
      }
    }, error = function(e) {
      #silently fail, if everything fails its handled below
    })
  }
  if(length(temp_returns) > 0) {
    total_conf <- sum(temp_confs)
    
    if(total_conf > 0) {
      weighted_return <- sum(temp_returns * temp_confs) / total_conf
      avg_conf <- mean(temp_confs)
    } else {
      weighted_return <- mean(temp_returns)
      avg_conf <- 0.05
    }
  
    
    # Store annualized return (3m * 4) and confidence
    llm_results_list[[tk]] <- list(return = (weighted_return+1)^4-1, conf = avg_conf)
    cat("Done.")
  } else {
    # Neutral fallback if all 5 runs failed to return valid numbers
    llm_results_list[[tk]] <- list(return = 0, conf = 0.05)
    cat("Error/Skipped.")
  }
}


cat("\n[4/4] Updating Mu_BL with LLM views...")