llm_df <- map_dfr(llm_results_list, as.data.frame, .id = "Ticker") %>%
  mutate(linear_val = ((return+1)^(1/4)-1)*4,
         log_val = return,
         conf = conf) %>% 
  dplyr::select(Ticker, linear_val, log_val, conf)
