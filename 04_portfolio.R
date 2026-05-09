# --- 4. THE WEALTH MAXIMIZER (Strict Constraints) ---
port_spec <- portfolio.spec(assets = colnames(returns_xts))

# CONSTRAINT A: TOTAL WEIGHT = 1 (No Leverage Disaster)
# This forces the sum of all absolute weights to be ~1.0
port_spec <- add.constraint(port_spec, type = "weight_sum", min_sum = 0.2, max_sum = 1)

# CONSTRAINT B: LONG-BIASED (Limit Shorting)
port_spec <- add.constraint(port_spec, type = "box", min = -0.30, max = 0.50)

#transaction costs
cost_vector <- assets$cost
names(cost_vector) <- assets$Ticker
port_spec <- add.constraint(port_spec, 
                            type = "transaction_cost", 
                            ptc = cost_vector)

# CONSTRAINT C: MANDATE GROUPS (+/- 5% or $25M Net)
asset_groups <- list(
  Equities = which(colnames(returns_xts) %in% assets$Ticker[assets$Class == "Equities"]),
  FixedIncome = which(colnames(returns_xts) %in% assets$Ticker[assets$Class == "Fixed Income"]),
  FX = which(colnames(returns_xts) %in% assets$Ticker[assets$Class == "FX"]),
  Commodities = which(colnames(returns_xts) %in% assets$Ticker[assets$Class == "Commodities"])
)
#port_spec <- add.constraint(port_spec, type = "group", groups = asset_groups,
 #                           group_min = rep(0.05, 4), group_max = rep(0.70, 4))

# OBJECTIVE: Maximize Return for a "Moderate" Risk (prevents -33% floor)
port_spec <- add.objective(port_spec, type = "return", name = "mean")
port_spec <- add.objective(port_spec, type = "risk", name = "ETL", 
                           arguments=list(p=0.95, clean="boudt"))


#net position constraint
mandate_penalty <- function(weights, groups) {
  penalty <- 0
  for(i in 1:length(groups)) {
    group_weight <- sum(weights[groups[[i]]])
    # If the net position is in the 'Dead Zone' (-5% to +5%), penalize it
    if(abs(group_weight) < 0.05) {
      penalty <- penalty + (0.05 - abs(group_weight))*100# Massive penalty to disqualify the particle
    }
  }
  return(penalty)
}
port_spec <- add.objective(port_spec, 
                           type = "function", 
                           name = "mandate_penalty", 
                           arguments = list(groups = asset_groups))

leverage_penalty <- function(weights, max_lev = 2.0) {
  current_lev <- sum(abs(weights))
  
  if (current_lev > max_lev) {
    # Quadratic penalty: The more you break it, the more it hurts
    return((current_lev - max_lev)^2 * 1000) 
  } else {
    return(0)
  }
}

port_spec <- add.objective(port_spec, 
                           type = "function", 
                           name = "leverage_penalty", 
                           arguments = list(max_lev = 2.0))


#leverage constraint, max 200%
port_spec <- add.constraint(port_spec, type = "leverage", leverage = 2)

parallel <- TRUE
if(parallel==TRUE) {
  p_load(foreach, doParallel)
  cores <- parallel::detectCores() - 1 #let one core live!!!!!!!, else youll have a funny time :)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  print(paste("Optimization running on", cores, "cores!"))
}

# --- 5. Execution & Wealth Analysis ---
method <- "pso"

#https://rdrr.io/rforge/PortfolioAnalytics/src/R/optimize.portfolio.R

if(method=="ROI") { #problem that it ignores the max_leverage argument
  opt_res <- optimize.portfolio(R = returns_xts, portfolio = port_spec, 
                                optimize_method = "ROI", mu = mu_bl, sigma = Sigma)
} else if(method=="pso") { #maxit for pso
  opt_res <- optimize.portfolio(
    R = returns_xts, 
    portfolio = port_spec, 
    optimize_method = "pso", 
    mu = mu_bl, 
    sigma = Sigma,
    moment_fun="set.portfolio.moments",
    search_size = 25000,
    maxit=500,
    max_leverage = 2.0,
    trace = TRUE,
    parallel= if(parallel==TRUE) {TRUE} else{FALSE},
    control = list(
      NP=50
    )
  )
} else if(method=="DEoptim") { #itermax for DEoptim
    opt_res <- optimize.portfolio(
      R = returns_xts, 
      portfolio = port_spec, 
      optimize_method = "DEoptim",
      search_size = 10000, # Increased for better discovery of alpha
      itermax=200,
      max_leverage = 2.0,
      trace = TRUE,
      mu = mu_bl,
      moment_fun="set.portfolio.moments",
      sigma = Sigma,
      control = list(
        NP=50,
        parallelType=if(parallel==TRUE) {1} else{0} #set to one for parallel optimization
      )
    )
}

if(parallel==TRUE){
  stopCluster(cl)
  registerDoSEQ()
}


weights <- extractWeights(opt_res)
capital <- 500000000

# Calculate Metrics
exp_return_3m <- sum(weights * mu_bl) * (63/252)
exp_vol_3m <- sqrt(as.numeric(t(weights) %*% Sigma %*% weights)) * sqrt(63/252)
certainty_95_return <- exp_return_3m - (0.8416 * exp_vol_3m) #95% 1.645, #40% 0.5244, 60%: 0.842

final_portfolio <- tibble(Ticker = names(weights), Weight = weights) %>%
  left_join(assets %>% dplyr::select(Ticker, Class, cost), by = "Ticker") %>%
  mutate(Allocation_USD = Weight * capital,
         TC_Cost_USD = abs(Allocation_USD) * cost)

total_tc <- sum(final_portfolio$TC_Cost_USD)
expected_wealth <- (capital - total_tc) * exp(exp_return_3m)
wealth_95_cert <- (capital - total_tc) * (1 + certainty_95_return)

