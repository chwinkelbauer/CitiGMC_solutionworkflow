library(pacman)
p_load(readxl, tidyverse, tidyquant, PortfolioAnalytics, ROI, ROI.plugin.quadprog, timetk, purrr)

#init functions
source("10_init.R")
if(TRUE){
  source("032_llm_googleworkaround.R") #use this to get news, simulates mozilla user so ip is valid
}
#run analysis nd get data
source("01_setup.R")

source("031_llminit.R") #this is the llm part
#source("03_llm.R") #uncomment when running llm

source("06_datamani.R")
#use the jamesstein method to estimate that thing
if(TRUE){
  source("05_jamesstein.R")
} else {
  source("02_blm.R")
}

#TRUE performs black litterman
if(TRUE){source("07_blacklittermanoptim.R")}

source("04_portfolio.R")

#report
source("09_report.R")


  #source("11_runwhilenothere.R")

source("12_comparison.R")

#########################################
#instructions

#data was retrieved: DD/MM/YYYY (27/04/2026)
#dont change, so do not rerun 01_setup.R & 03_llminit.R
