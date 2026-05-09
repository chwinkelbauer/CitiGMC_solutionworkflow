llm_influence_weight <- 0.2

#for linearized gains
if(TRUE) {
  q_returns <- llm_df$linear_val
} else {q_returns <- llm_df$log_val}

q_conf <- llm_df$conf

mu_bl_hist <- colMeans(returns_xts) * 252
grand_mean <- mean(mu_bl_hist)

lambda <- 0.5 
mu_prior <- (1 - lambda) * mu_bl_hist + (lambda * grand_mean)

p_load(corpcor)
Sigma <- cov.shrink(returns_xts) * 252

actual_weight <- llm_influence_weight * q_conf
mu_bl <- ((1 - actual_weight) * mu_bl_hist) + (actual_weight * q_returns)

