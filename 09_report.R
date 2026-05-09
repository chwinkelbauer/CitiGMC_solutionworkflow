# --- 6. Final Report ---
cat("\n--- FINAL WEALTH MAXIMIZATION REPORT ---")
cat("\nGross Exposure (Leverage):    ", round(sum(abs(weights)) * 100, 1), "%")
cat("\nTotal Entry Costs:            $", formatC(total_tc, format="f", big.mark=",", digits=0))
cat("\n\nExpected 3M Return:           ", round(exp_return_3m * 100, 2), "%")
cat("\n95% Certainty Return:         ", round(certainty_95_return * 100, 2), "%")
cat("\n\nExpected Wealth:              $", formatC(expected_wealth, format="f", big.mark=",", digits=0))
cat("\nWealth (95% Certainty):       $", formatC(wealth_95_cert, format="f", big.mark=",", digits=0))
cat("\n\nNet Gain vs Doing Nothing:    $", formatC(wealth_95_cert - capital, format="f", big.mark=",", digits=0), " (95% Confidence)\n")




