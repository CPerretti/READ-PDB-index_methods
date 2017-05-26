source("5-fit_models.R")

## Calculate error of each method -------------------------

df_errors <-
  df_sims_withfit %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::filter(variable %in% c("biomass_rw", 
                                "biomass_rw_3ymean",
                                "biomass_average",
                                "biomass_average_3ymean",
                                "biomass_tru"),
                year == 2016) %>%
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


## Calculate coverage probability of random walk model ----
df_coverage <-
  df_sims_withfit %>%
  dplyr::filter(variable %in% c("biomass_tru",
                                "biomass_rw",
                                "biomass_rw_hi95",
                                "biomass_rw_lo95",
                                "biomass_rw_hi90",
                                "biomass_rw_lo90",
                                "biomass_rw_hi75",
                                "biomass_rw_lo75")) %>%
  tidyr::spread(variable, value) %>%
  dplyr::mutate(within_ci95 = ifelse(biomass_tru < biomass_rw_hi95 &
                                      biomass_tru > biomass_rw_lo95,
                                     1,
                                     0),
                within_ci90 = ifelse(biomass_tru < biomass_rw_hi90 &
                                       biomass_tru > biomass_rw_lo90,
                                     1,
                                     0),
                within_ci75 = ifelse(biomass_tru < biomass_rw_hi75 &
                                       biomass_tru > biomass_rw_lo75,
                                     1,
                                     0)) %>%
  dplyr::summarise(coverage_ci95 = sum(within_ci95)/
                                        length(within_ci95) * 100,
                   coverage_ci90 = sum(within_ci90)/
                                        length(within_ci90) * 100,
                   coverage_ci75 = sum(within_ci75)/
                                         length(within_ci75) * 100) %>%
  dplyr::group_by(driver, scenario) %>%
  dplyr::summarise(coverage_ci95_mean = mean(coverage_ci95),
                   coverage_ci90_mean = mean(coverage_ci90),
                   coverage_ci75_mean = mean(coverage_ci75),
                   coverage_ci95_ci = 1.96 * sd(coverage_ci95) /
                                          length(coverage_ci95)^0.5,
                   coverage_ci90_ci = 1.96 * sd(coverage_ci90) /
                                          length(coverage_ci90)^0.5,
                   coverage_ci75_ci = 1.96 * sd(coverage_ci75) /
                                          length(coverage_ci75)^0.5)
  
  
  