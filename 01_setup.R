file_path <- "asset_list.xlsx"
assets <- bind_rows(
  read_excel(file_path, sheet = "equities") %>% mutate(Class = "Equities"),
  read_excel(file_path, sheet = "fixed income") %>% mutate(Class = "Fixed Income"),
  read_excel(file_path, sheet = "foreign exchange") %>% mutate(Class = "FX"),
  read_excel(file_path, sheet = "commodities") %>% mutate(Class = "Commodities")
) %>% 
  distinct(Ticker, .keep_all = TRUE) %>%
  mutate(cost = case_when(
    Class == "Equities" & Region == "DM" ~ 0.0020,
    Class == "Equities" & Region == "EM" ~ 0.0050,
    Class == "Fixed Income" & Region == "G10" ~ 0.0025,
    Class == "Fixed Income" ~ 0.0060,
    Class == "FX" & Region == "G10" ~ 0.0005,
    Class == "FX" ~ 0.0030,
    Class == "Commodities" ~ 0.0020,
    TRUE ~ 0.0025
  ))

tickers <- unique(assets$Ticker)
returns_xts <- tq_get(tickers, from = Sys.Date() - (365*2), to = Sys.Date()) %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "log") %>%
  pivot_wider(names_from = symbol, values_from = daily.returns) %>%
  tk_xts(date_var = date, silent = TRUE)
returns_xts[is.na(returns_xts)] <- 0