# Fit models to simulated data

source("5-simulate_data.R")

# set up data and parameters
dat <- 
  list(log_obs_DFO    = df_sims$biomass_obs_log.survey1,
       log_obs_spring = df_sims$biomass_obs_log.survey2,
       log_obs_fall   = df_sims$biomass_obs_log.survey3)

# Initialize parameters
parameters <- 
  list(logB = rep(0, length(dat$log_obs_DFO)),
       log_process_error = 0,
       log_obs_error_DFO = 0,
       log_obs_error_spring = 0,
       log_obs_error_fall = 0)

# Fit model
obj <- MakeADFun(dat,
                 parameters,
                 DLL = "yt_rw",
                 random = c("logB"),
                 silent = TRUE)

opt <- nlminb(obj$par, obj$fn, obj$gr,
              control = list(iter.max = 1000,
                             eval.max = 1000))

srep <- summary(sdreport(obj))


# Extract parameter estimates from TMB output
ests <- return_ests(srep = srep)

# Organize fit
df_fit_sim  <-
  data.frame(year = df_sims$year %>% unique,
             `biomass_rw` = ests$est.log.pop[,"Estimate"] %>% exp,
             `biomass_rw_hi` = (ests$est.log.pop[,"Estimate"] +
                          2*ests$est.log.pop[,"Std. Error"]) %>%
               exp,
             `biomass_rw_lo` = (ests$est.log.pop[,"Estimate"] -
                          2*ests$est.log.pop[,"Std. Error"]) %>%
               exp,
             biomass_average = rowMeans(
                                  cbind(df_sims$biomass_obs.survey1,
                                        df_sims$biomass_obs.survey2,
                                        df_sims$biomass_obs.survey3)),
                                          
             check.names = F)


