tau <- 0.05
Pi  <- mu_bl_hist
Q   <- llm_df$linear_val

#else the confidence is too low
llm_influence_weight <- 0.6
actual_weight <- llm_influence_weight * llm_df$conf
Omega_diag <- diag(Sigma) * (1 / (actual_weight + 1e-6))

#simplified version for diagonal matrix
H <- solve(tau * Sigma)
W <- diag(1 / Omega_diag)

mu_bl <- solve(H + W, H %*% Pi + W %*% Q)

mu_bl <- as.vector(mu_bl)
names(mu_bl) <- llm_df$Ticker


#thats for my check
bl_check <- tibble(
  Ticker     = llm_df$Ticker,
  Confidence = llm_df$conf,
  History_Pi = Pi,           # The 80% Prior
  AI_View_Q  = Q,            # The 20% AI View
  BL_Final   = mu_bl         # The Bayesian Result
) %>%
  mutate(
    # How much did the AI actually move the needle?
    Total_Shift = BL_Final - History_Pi,
    # Did it listen to the AI? (100% means BL_Final == AI_View_Q)
    AI_Influence_Pct = ifelse(AI_View_Q == History_Pi, 0, 
                              (BL_Final - History_Pi) / (AI_View_Q - History_Pi) * 100)
  )