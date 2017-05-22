source("3-clean.R")
# Simulate data

n_rep <- 1000
df_sims <-
  base::expand.grid(rep = 1:n_rep,
                    scenario_f = c("No fishing")) %>%
  dplyr::group_by(rep, scenario_f) %>%
  dplyr::do(
    run_sim(n_ages        = 10,
            n_surveys     = 3,
            sd_multiplier = 4,
            n_burn        = 100,
            n_sim         = 30,
            return_burn   = FALSE,
            cor_mat       = matrix(data = 0,
                                   nrow = 3,
                                   ncol = 3))
    )



