# Fit models to real data.
source("3-clean.R")

library(TMB)
library(Hmisc)

# compile cpp code and load dll
compile("yt_rw.cpp")
dyn.load(dynlib("yt_rw"))

# set up data and parameters
dat <- 
  list(log_obs_DFO    = dplyr::filter(df_yt, Survey == "DFO")$log_biomass,
       log_obs_spring = dplyr::filter(df_yt, Survey == "Spring")$log_biomass,
       log_obs_fall   = dplyr::filter(df_yt, Survey == "Fall")$log_biomass)

# Initialize parameters
parameters <- 
  list(logB = rep(0, nrow(dplyr::filter(df_yt, Survey == "DFO"))),
       log_process_error    = 0,
       log_obs_error_DFO    = 0,
       log_obs_error_spring = 0,
       log_obs_error_fall   = 0)

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
df_fit_real  <-
  data.frame(year = df_yt$year %>% unique,
             `RW` = ests$est.log.pop[,"Estimate"] %>% exp,
             `RW hi` = (ests$est.log.pop[,"Estimate"] +
                        2*ests$est.log.pop[,"Std. Error"]) %>%
                        exp,
             `RW lo` = (ests$est.log.pop[,"Estimate"] -
                        2*ests$est.log.pop[,"Std. Error"]) %>%
                        exp,
             check.names = F)



