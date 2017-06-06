source("5-fit_models.R")

## Calculate error of each method in scenario years -------

df_errors <-
  df_sims_withfit %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::filter(variable %in% c("biomass_rw", 
                                "biomass_rw_3ymean",
                                "biomass_average",
                                "biomass_average_3ymean",
                                "biomass_tru"),
                year > (terminal_year - n_scenario)) %>%
  # Rename variables for plots
  dplyr::mutate(variable = ifelse(variable == "biomass_rw",
                                  "biomass_rw_terminal",
                                  variable),
                variable = ifelse(variable == "biomass_average",
                                  "biomass_average_terminal",
                                  variable)) %>%
  tidyr::spread(variable, value) %>%
  tidyr::gather(variable, value, 
                -rep, -driver, -scenario, 
                -year, -biomass_tru) %>%
  dplyr::mutate(abs_err = abs(value - biomass_tru)) %>%
  dplyr::group_by(driver, scenario, variable) %>%
  dplyr::summarise(mae = mean(abs_err),
                   mae_se = sd(abs_err)/length(abs_err)^0.5,
                   mae_ci = 1.96 * mae_se) %>%
  tidyr::separate(variable, into = c("variable_fit", 
                                     "method", 
                                     "smooth")) %>%
  dplyr::mutate(method = ifelse(method == "average",
                                "Average surveys",
                                "Random walk"),
                smooth = ifelse(smooth == "3ymean",
                                "Three-year mean",
                                "Terminal year"))

  
## Percent reduction in error going from Average to RW ----
df_perc_reduct <-
  df_errors %>%
  dplyr::filter(method %in% c("Average surveys", "Random walk"),
                smooth == "Terminal year") %>%
  dplyr::select(scenario, driver, method, mae) %>%
  tidyr::spread(method, mae) %>%
  dplyr::mutate(perc_reduct = 100*(`Average surveys` - `Random walk`)/`Average surveys`)

## Coverage performance of rw model in scenario years ----
df_coverage_decile <-
  df_sims_withfit %>%
  dplyr::filter(variable %in% c("biomass_tru",
                                "biomass_rw",
                                "biomass_rw_se"),
                year > (terminal_year - n_scenario)) %>%
  tidyr::spread(variable, value) %>%
  dplyr::mutate(decile = floor(10 * pnorm(q    = log(biomass_tru), 
                                          mean = log(biomass_rw), 
                                          sd   = log(biomass_rw_se))))

