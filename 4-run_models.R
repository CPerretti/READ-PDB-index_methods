# Setup models and fit them to yellowtail data.
source("3-clean.R")

library(TMB)
library(Hmisc)

# compile cpp code and load dll
compile("yt_rw.cpp")
dyn.load(dynlib("yt_rw"))

n.obs <- dplyr::filter(df_yt, Survey == "DFO") %>% 
  nrow # assumes data in all years for all three surveys


# set up data and parameters
dat_wfloors <- list(
  log_obs_DFO = dplyr::filter(df_yt, Survey == "DFO")$log_biomass,
  log_obs_spring = dplyr::filter(df_yt, Survey == "Spring")$log_biomass,
  log_obs_fall = dplyr::filter(df_yt, Survey == "Fall")$log_biomass,
  DFO_sd = (dplyr::filter(df_yt, Survey == "DFO")$CV.B.^2 + 1) %>% 
    log %>% sqrt,
  spring_sd = (dplyr::filter(df_yt, Survey == "Spring")$CV.B.^2 + 1) %>% 
    log %>% sqrt,
  fall_sd = (dplyr::filter(df_yt, Survey == "Fall")$CV.B.^2 + 1) %>% 
    log %>% sqrt
)

# Setup data for the no floors model (set all obs error to zero)
dat_nofloors <-
  dat_wfloors
dat_nofloors$DFO_sd <- 
  dat_nofloors$spring_sd <- 
  dat_nofloors$fall_sd <- 
  rep(0,length(dat_wfloors$fall_sd))

parameters <- list(
  logB = rep(0,n.obs),
  log_process_error = 0,
  log_obs_error_DFO = 0,
  log_obs_error_spring = 0,
  log_obs_error_fall = 0
)

# Fit with floors model
obj_wfloors <- MakeADFun(dat_wfloors,
                 parameters,
                 DLL = "yt_rw", 
                 random = c("logB"), 
                 silent = TRUE)
opt_wfloors <- nlminb(obj_wfloors$par, 
                      obj_wfloors$fn, 
                      obj_wfloors$gr, 
                      control = list(iter.max = 1000,
                                     eval.max = 1000))
srep_wfloors <- summary(sdreport(obj_wfloors))

# Fit no floors model
obj_nofloors <- MakeADFun(dat_nofloors,
                          parameters,
                          DLL = "yt_rw", 
                          random = c("logB"), 
                          silent = TRUE)
opt_nofloors <- nlminb(obj_nofloors$par, 
                       obj_nofloors$fn, 
                       obj_nofloors$gr, 
                       control = list(iter.max = 1000,
                                      eval.max = 1000))
srep_nofloors <- summary(sdreport(obj_nofloors))


# Extract parameter estimates from TMB output
ests_wfloors <- return_ests(srep = srep_wfloors)
ests_nofloors <- return_ests(srep = srep_nofloors)



fits_wfloors  <-
  data.frame(Year = df_yt$Year %>% unique,
             `RW with obs floor` = ests_wfloors$est.log.pop[,"Estimate"] %>% exp,
             `RW with obs floor hi` = (ests_wfloors$est.log.pop[,"Estimate"] + 
                                       2*ests_wfloors$est.log.pop[,"Std. Error"]) %>%
                                       exp,
             `RW with obs floor lo` = (ests_wfloors$est.log.pop[,"Estimate"] - 
                                       2*ests_wfloors$est.log.pop[,"Std. Error"]) %>%
                                       exp,
             check.names = F)

fits_nofloors <-
  data.frame(Year = df_yt$Year %>% unique,
             `RW no obs floor` = ests_nofloors$est.log.pop[,"Estimate"] %>% exp,
             `RW no obs floor hi` = (ests_nofloors$est.log.pop[,"Estimate"] + 
                                     2*ests_nofloors$est.log.pop[,"Std. Error"]) %>%
                                     exp,
             `RW no obs floor lo` = (ests_nofloors$est.log.pop[,"Estimate"] - 
                                     2*ests_nofloors$est.log.pop[,"Std. Error"]) %>%
                                     exp,
             check.names = F)
