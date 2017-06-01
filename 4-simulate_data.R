source("3-clean.R")
# Simulate data

n_rep <- 1000
n_scenario <- 5 #Years in scenario (used again in plots)
terminal_year <- max(df_rfssb$Year) + n_scenario
df_sims <-
  base::expand.grid(rep = 1:n_rep,
                    driver = c("f", "r"),
                    scenario = c("no change", 
                                 "increasing biomass", 
                                 "decreasing biomass")) %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::do(
    run_sim(n_ages        = 10,
            n_surveys     = 3,
            sd_multiplier = 4,
            n_burn        = 100,
            n_sim         = length(df_r$year),
            n_scenario    = n_scenario,
            driver        = .$driver,
            scenario      = .$scenario,
            df_r          = df_r,
            df_f          = df_f,
            terminal_year = terminal_year,
            return_burn   = FALSE,
            cor_mat       = matrix(data = 0,
                                   nrow = 3,
                                   ncol = 3))
    )



