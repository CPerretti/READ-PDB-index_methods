source("5-fit_models.R")

## Calculate error of each method in scenario years -------
df_error_by_rep <-
  df_sims_withfit %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::filter(variable %in% c("biomass_rw_1surv_term", 
                                "biomass_rw_1surv_3ymean",
                                "biomass_rw_2surv_term", 
                                "biomass_rw_2surv_3ymean",
                                "biomass_rw_3surv_term", 
                                "biomass_rw_3surv_3ymean",
                                "biomass_average_1surv_term",
                                "biomass_average_2surv_term",
                                "biomass_average_3surv_term",
                                "biomass_average_1surv_3ymean",
                                "biomass_average_2surv_3ymean",
                                "biomass_average_3surv_3ymean",
                                "biomass_tru",
                                "biomass_3ypc"),
                year > (terminal_year - n_scenario)) %>%
  tidyr::spread(variable, value) %>%
  tidyr::gather(variable, value, 
                -rep, -driver, -scenario, 
                -year, -biomass_tru, -biomass_3ypc) %>%
  dplyr::mutate(abs_err = abs(value - biomass_tru)) %>%
  tidyr::separate(variable, into = c("variable_fit", 
                                     "method",
                                     "number of surveys",
                                     "smooth")) %>%
  dplyr::mutate(method = ifelse(method == "average",
                                "Empirical",
                                "State-space"),
                smooth = ifelse(smooth == "3ymean",
                                "Three-year mean",
                                "Terminal year")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(driver = as.character(driver),
                scenario = as.character(scenario),
                driver = ifelse(driver == "f",
                                "Fishing driver",
                                driver),
                driver = ifelse(driver == "r",
                                "Recruitment driver",
                                driver),
                scenario = ifelse(scenario == "no change",
                                  "No change",
                                  scenario),
                scenario = ifelse(scenario == "increasing rapidly",
                                  "Increasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing rapidly",
                                  "Decreasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "increasing slowly",
                                  "Increasing slowly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing slowly",
                                  "Decreasing slowly",
                                  scenario))

df_errors <-
  df_error_by_rep %>%
  dplyr::group_by(driver, scenario, method, `number of surveys`, smooth) %>%
  dplyr::summarise(mae = mean(abs_err),
                   mae_se = sd(abs_err)/length(abs_err)^0.5,
                   mae_ci = 1.96 * mae_se,
                   mean_3ypc = mean(biomass_3ypc))
  
## Percent reduction in error between models and smooths ----
# Overall comparisons
pcbetter_termVSmean <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(pcbetter_term = mean(100*(`Three-year mean` - `Terminal year`)/
                                          `Three-year mean`))

pcbetter_emptermVSempmean <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(method == "Empirical") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(pcbetter_term = mean(100*(`Three-year mean` - `Terminal year`)/
                                          `Three-year mean`))

pcbetter_sstermVSssmean <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(method == "State-space") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(pcbetter_term = mean(100*(`Three-year mean` - `Terminal year`)/
                                          `Three-year mean`))

pcbetter_sstermVSempterm <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(smooth == "Terminal year") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(method, mae) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(pcbetter_term = mean(100*(Empirical - `State-space`)/Empirical))

pcbetter_ssmeanVSempmean <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(smooth == "Three-year mean") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(method, mae) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(pcbetter_term = mean(100*(Empirical - `State-space`)/Empirical))

# Comparisons within scenarios
pcbetter_sstermVSssmean_byscenario <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(method == "State-space") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::group_by(driver, scenario) %>%
  dplyr::summarise(pcbetter_term = mean(100*(`Three-year mean` - `Terminal year`)/
                                          `Three-year mean`))
pcbetter_emptermVSempmean_byscenario <-
  df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(method == "Empirical") %>%
  dplyr::select(-mae_se, -mae_ci) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::group_by(driver, scenario) %>%
  dplyr::summarise(pcbetter_term = mean(100*(`Three-year mean` - `Terminal year`)/
                                          `Three-year mean`))
# # Terminal year only
# df_perc_reduct <-
#   df_errors %>%
#   dplyr::filter(method %in% c("Average surveys", "Random walk"),
#                 smooth == "Terminal year") %>%
#   dplyr::select(scenario, driver, method, mae) %>%
#   tidyr::spread(method, mae) %>%
#   dplyr::mutate(perc_reduct = 100*(`Average surveys` - `Random walk`)/`Average surveys`)
# 
# ## Three-year mean vs terminal year
# df_perc_reduct_3yrvsterm_average <-
#   df_errors %>%
#   dplyr::filter(method %in% c("Average surveys")) %>%
#   dplyr::select(scenario, driver, method, smooth, mae) %>%
#   tidyr::spread(smooth, mae) %>%
#   dplyr::mutate(perc_reduct = 100*(`Three-year mean` - `Terminal year`)/
#                   `Three-year mean`)
# 
# df_perc_reduct_3yrvsterm_ss <-
#   df_errors %>%
#   dplyr::filter(method %in% c("Random walk")) %>%
#   dplyr::select(scenario, driver, method, smooth, mae) %>%
#   tidyr::spread(smooth, mae) %>%
#   dplyr::mutate(perc_reduct = 100*(`Three-year mean` - `Terminal year`)/
#                   `Three-year mean`)


## Coverage performance of rw model in scenario years ----
df_coverage_decile <-
  df_sims_withfit %>%
  dplyr::filter(variable %in% c("biomass_tru",
                                "biomass_rw_3surv_term",
                                "biomass_rw_3surv_term_se"),
                year > (terminal_year - n_scenario)) %>%
  tidyr::spread(variable, value) %>%
  dplyr::mutate(decile = floor(10 * pnorm(q    = log(biomass_tru), 
                                          mean = log(biomass_rw_3surv_term), 
                                          sd   = log(biomass_rw_3surv_term_se))))

