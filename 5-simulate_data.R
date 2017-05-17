source("4-fit_real.R")

# Set simulated observation error correlation equal to the
# the correlation of the real time series
# cor_mat <- 
#   df_yt %>%
#   dplyr::select(Survey, Year, biomass) %>%
#   tidyr::spread(Survey, biomass) %>%
#   dplyr::select(DFO, Fall, Spring) %>%
#   cor
cor_mat <- matrix(data = 0,
                  nrow = 3,
                  ncol = 3)
  
  

## Simulate data ------------------------------------------
df_sims <- 
  run_sim(n_ages      = 10,
          n_surveys   = 3,
          sd_multiplier = 4,
          n_burn      = 100,
          n_sim       = 30,
          return_burn = FALSE,
          cor_mat     = cor_mat)



