source("4-simulate_data.R")
# Fit models to simulated data

library(TMB)


# compile cpp code and load dll
compile("yt_rw.cpp")
dyn.load(dynlib("yt_rw"))


## Fit simulations ----------------------------------------
# Fit model
df_fit_sim <-
  df_sims %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::do(
    fit_models(year = .$year,
             log_biomass_survey1 = .$biomass_obs_log.survey1, 
             log_biomass_survey2 = .$biomass_obs_log.survey2, 
             log_biomass_survey3 = .$biomass_obs_log.survey3)) %>%
  dplyr::mutate(biomass_rw = biomass_rw_term2018,
                biomass_rw = ifelse(year == 2017,
                                    biomass_rw_term2017,
                                    biomass_rw),
                biomass_rw = ifelse(year == 2016,
                                    biomass_rw_term2016,
                                    biomass_rw),
                biomass_rw = ifelse(year == 2015,
                                    biomass_rw_term2015,
                                    biomass_rw),
                biomass_rw = ifelse(year == 2014,
                                    biomass_rw_term2014,
                                    biomass_rw),
                biomass_rw_se = biomass_rw_term2018_se,
                biomass_rw_se = ifelse(year == 2017,
                                    biomass_rw_term2017_se,
                                    biomass_rw_se),
                biomass_rw_se = ifelse(year == 2016,
                                    biomass_rw_term2016_se,
                                    biomass_rw_se),
                biomass_rw_se = ifelse(year == 2015,
                                    biomass_rw_term2015_se,
                                    biomass_rw_se),
                biomass_rw_se = ifelse(year == 2014,
                                    biomass_rw_term2014_se,
                                    biomass_rw_se),
                biomass_rw_3ymean = zoo::rollmean(biomass_rw, 
                                              k = 3,
                                              na.pad = TRUE,
                                              align = "right"),
                biomass_average_3ymean = zoo::rollmean(biomass_average, 
                                                   k = 3,
                                                   na.pad = TRUE,
                                                   align = "right"))

# Add fits to original dataframe and re-organize data into
# long format
df_sims_withfit <-
  df_sims %>%
  tidyr::gather(variable, value, 
                -year, -rep, -driver, -scenario) %>%
  dplyr::bind_rows({df_fit_sim %>%
                    tidyr::gather(variable, value, 
                                  -year, -rep, -driver, 
                                  -scenario)})


## Fit real data ------------------------------------------
# Fit model
df_fit_real <-
  df_real %>%
  fit_models(year = unique(.$year),
             log_biomass_survey1 = 
               dplyr::filter(., Survey == "DFO")$log_biomass,
             log_biomass_survey2 = 
               dplyr::filter(., Survey == "Spring")$log_biomass,
             log_biomass_survey3 = 
               dplyr::filter(., Survey == "Fall")$log_biomass) %>%
  dplyr::mutate(biomass_rw = biomass_rw_term2016,
                biomass_rw_se = biomass_rw_term2016_se,
                biomass_rw_3ymean = zoo::rollmean(biomass_rw, 
                                              k = 3,
                                              na.pad = TRUE,
                                              align = "right"),
                biomass_average_3ymean = zoo::rollmean(biomass_average, 
                                                   k = 3,
                                                   na.pad = TRUE,
                                                   align = "right"))

# Add fits to original dataframe and re-organize data into
# long format
df_real_withfit <-
  df_real %>%
  # add rw fit to original dataframe
  dplyr::bind_rows({df_fit_real %>%
      tidyr::gather(Survey, biomass, -year)}) %>%
  # rename variables
  dplyr::select(year, Survey, biomass) %>%
  dplyr::rename(variable = Survey,
                value = biomass)
