source("4-simulate_data.R")
# Fit models to simulated data

library(TMB)


# compile cpp code
compile("yt_rw_1surv.cpp")
compile("yt_rw_2surv.cpp")
compile("yt_rw_3surv.cpp")
dyn.load(dynlib("yt_rw_1surv"))
dyn.load(dynlib("yt_rw_2surv"))
dyn.load(dynlib("yt_rw_3surv"))


## Fit simulations ----------------------------------------
# Fit model
df_fit_sim <-
  df_sims %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::do(
    fit_models(year = .$year,
               log_biomass_survey1 = .$biomass_obs_log.survey1, 
               log_biomass_survey2 = .$biomass_obs_log.survey2, 
               log_biomass_survey3 = .$biomass_obs_log.survey3,
               biomass_tru = .$biomass_tru)) %>%
  dplyr::mutate(biomass_rw_1surv_term = biomass_rw_1surv_term2018,
                biomass_rw_1surv_term = ifelse(year == 2017,
                                               biomass_rw_1surv_term2017,
                                               biomass_rw_1surv_term),
                biomass_rw_1surv_term = ifelse(year == 2016,
                                               biomass_rw_1surv_term2016,
                                               biomass_rw_1surv_term),
                biomass_rw_1surv_term = ifelse(year == 2015,
                                               biomass_rw_1surv_term2015,
                                               biomass_rw_1surv_term),
                biomass_rw_1surv_term = ifelse(year == 2014,
                                               biomass_rw_1surv_term2014,
                                               biomass_rw_1surv_term),
                biomass_rw_2surv_term = biomass_rw_2surv_term2018,
                biomass_rw_2surv_term = ifelse(year == 2017,
                                               biomass_rw_2surv_term2017,
                                               biomass_rw_2surv_term),
                biomass_rw_2surv_term = ifelse(year == 2016,
                                               biomass_rw_2surv_term2016,
                                               biomass_rw_2surv_term),
                biomass_rw_2surv_term = ifelse(year == 2015,
                                               biomass_rw_2surv_term2015,
                                               biomass_rw_2surv_term),
                biomass_rw_2surv_term = ifelse(year == 2014,
                                               biomass_rw_2surv_term2014,
                                               biomass_rw_2surv_term),
                biomass_rw_3surv_term = biomass_rw_3surv_term2018,
                biomass_rw_3surv_term = ifelse(year == 2017,
                                    biomass_rw_3surv_term2017,
                                    biomass_rw_3surv_term),
                biomass_rw_3surv_term = ifelse(year == 2016,
                                    biomass_rw_3surv_term2016,
                                    biomass_rw_3surv_term),
                biomass_rw_3surv_term = ifelse(year == 2015,
                                    biomass_rw_3surv_term2015,
                                    biomass_rw_3surv_term),
                biomass_rw_3surv_term = ifelse(year == 2014,
                                    biomass_rw_3surv_term2014,
                                    biomass_rw_3surv_term),
                biomass_rw_1surv_term_se = biomass_rw_1surv_term2018_se,
                biomass_rw_1surv_term_se = ifelse(year == 2017,
                                                  biomass_rw_1surv_term2017_se,
                                                  biomass_rw_1surv_term_se),
                biomass_rw_1surv_term_se = ifelse(year == 2016,
                                                  biomass_rw_1surv_term2016_se,
                                                  biomass_rw_1surv_term_se),
                biomass_rw_1surv_term_se = ifelse(year == 2015,
                                                  biomass_rw_1surv_term2015_se,
                                                  biomass_rw_1surv_term_se),
                biomass_rw_1surv_term_se = ifelse(year == 2014,
                                                  biomass_rw_1surv_term2014_se,
                                                  biomass_rw_1surv_term_se),
                biomass_rw_2surv_term_se = biomass_rw_2surv_term2018_se,
                biomass_rw_2surv_term_se = ifelse(year == 2017,
                                                  biomass_rw_2surv_term2017_se,
                                                  biomass_rw_2surv_term_se),
                biomass_rw_2surv_term_se = ifelse(year == 2016,
                                                  biomass_rw_2surv_term2016_se,
                                                  biomass_rw_2surv_term_se),
                biomass_rw_2surv_term_se = ifelse(year == 2015,
                                                  biomass_rw_2surv_term2015_se,
                                                  biomass_rw_2surv_term_se),
                biomass_rw_2surv_term_se = ifelse(year == 2014,
                                                  biomass_rw_2surv_term2014_se,
                                                  biomass_rw_2surv_term_se),
                biomass_rw_3surv_term_se = biomass_rw_3surv_term2018_se,
                biomass_rw_3surv_term_se = ifelse(year == 2017,
                                    biomass_rw_3surv_term2017_se,
                                    biomass_rw_3surv_term_se),
                biomass_rw_3surv_term_se = ifelse(year == 2016,
                                    biomass_rw_3surv_term2016_se,
                                    biomass_rw_3surv_term_se),
                biomass_rw_3surv_term_se = ifelse(year == 2015,
                                    biomass_rw_3surv_term2015_se,
                                    biomass_rw_3surv_term_se),
                biomass_rw_3surv_term_se = ifelse(year == 2014,
                                    biomass_rw_3surv_term2014_se,
                                    biomass_rw_3surv_term_se),
                biomass_rw_1surv_3ymean = zoo::rollmean(biomass_rw_1surv_term, 
                                                        k = 3,
                                                        na.pad = TRUE,
                                                        align = "right"),
                biomass_rw_2surv_3ymean = zoo::rollmean(biomass_rw_2surv_term, 
                                                        k = 3,
                                                        na.pad = TRUE,
                                                        align = "right"),
                biomass_rw_3surv_3ymean = zoo::rollmean(biomass_rw_3surv_term, 
                                                        k = 3,
                                                        na.pad = TRUE,
                                                        align = "right"),
                biomass_average_1surv_3ymean = zoo::rollmean(biomass_average_1surv_term, 
                                                             k = 3,
                                                             na.pad = TRUE,
                                                             align = "right"),
                biomass_average_2surv_3ymean = zoo::rollmean(biomass_average_2surv_term, 
                                                             k = 3,
                                                             na.pad = TRUE,
                                                             align = "right"),
                biomass_average_3surv_3ymean = zoo::rollmean(biomass_average_3surv_term, 
                                                             k = 3,
                                                             na.pad = TRUE,
                                                             align = "right"),
                biomass_3ypc = zoo::rollapply(biomass_tru,
                                              width = 3,
                                              na.pad = TRUE,
                                              align = "right",
                                              FUN = function(x) 100*(x[3]-x[1])/x[1])
                ) %>%
  dplyr::select(-biomass_tru)

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
               dplyr::filter(., Survey == "Fall")$log_biomass,
             biomass_tru = NA) %>%
  dplyr::mutate(biomass_rw_3surv_term = biomass_rw_3surv_term2016,
                biomass_rw_3surv_term_se = biomass_rw_3surv_term2016_se,
                biomass_rw_3surv_3ymean = zoo::rollmean(biomass_rw_3surv_term, 
                                              k = 3,
                                              na.pad = TRUE,
                                              align = "right"),
                biomass_average_3surv_3ymean = zoo::rollmean(biomass_average_3surv_term, 
                                                   k = 3,
                                                   na.pad = TRUE,
                                                   align = "right")) %>%
  dplyr::select(-biomass_tru)

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
