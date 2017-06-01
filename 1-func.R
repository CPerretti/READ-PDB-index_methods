# Load custom functions
library(magrittr) # for pipe operators (e.g., %>%)
library(ggplot2)

## Adjust ggplot function ---------------------------------
ggplot <- function(...) ggplot2::ggplot(...) + theme_bw()



## Extract estimates from TMB output ----------------------
return_ests <- function(srep) {
  est.log.pop <- srep[rownames(srep) == "logB",]
  est.process.error <- as.vector(round(srep[rownames(srep) ==
                                              "process_error",],3))
  est.obs.error.DFO <- as.vector(round(srep[rownames(srep) ==
                                              "obs_error_DFO",],3))
  est.obs.error.spring <- as.vector(round(srep[rownames(srep) ==
                                                 "obs_error_spring",],3))
  est.obs.error.fall <- as.vector(round(srep[rownames(srep) ==
                                               "obs_error_fall",],3))
  
  ests <- list(est.log.pop = est.log.pop,
               est.process.error = est.process.error,
               est.obs.error.DFO = est.obs.error.DFO,
               est.obs.error.fall = est.obs.error.fall,
               est.obs.error.spring = est.obs.error.spring)
}


## Run the population simulation --------------------------
run_sim <- function(n_ages,
                    n_surveys,
                    sd_multiplier,
                    n_burn,
                    n_sim,
                    n_scenario,
                    driver,
                    scenario,
                    df_r,
                    df_f,
                    terminal_year,
                    return_burn = FALSE,
                    cor_mat){
    
    n_t <- n_burn + n_sim + n_scenario
      
    # Pre-allocate matrices:
   abund_tru <- matrix(data = NA, # Abundance
                       nrow = n_t,
                       ncol = n_ages,
                       dimnames = list(NULL, 
                                       paste0("age", 1:n_ages)))
    C      <- matrix(data = NA, # Catch-at-age
                     nrow = n_t,
                     ncol = n_ages,
                     dimnames = list(NULL, 
                                     paste0("age", 1:n_ages)))
    SSB    <- matrix(data = NA, # Spawning stock biomass
                     nrow = n_t,
                     ncol = 1)
    
    # Model parameters:
    w_a    <- c(0.148, 0.317, 0.453, 0.588, 0.724, 
                rep(0.921, times = n_ages - 5)) # Weights
    mat_a  <- c(0, 0.462, 0.967, # Maturity
                 rep(1, times = n_ages - 3)) 
    m      <- 0.4 # Natural mortality
    phi    <- 0.4167 # Fraction of year before spawning
    
    ssb_ind <- 2:n_ages        # Ages that spawn
    sdR     <- 0.1             # Recruitment variability
    sdO     <- 0.2             # Observation error
    
    
    # Setup R time series
    r0 <- c(rep(df_r$R[1], times = n_burn), df_r$R)
    r <- c(r0, rep(df_r$R[nrow(df_r)], times = n_scenario))  
    
    # Set R for scenario years
    if (driver == "r"){
      if (scenario == "increasing biomass") {
        # increase by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          1.5^(1:n_scenario)
      } else if (scenario == "decreasing biomass") {
        # decrease by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          0.5^(1:n_scenario)
      } else if (scenario == "no change") {
        r <- r
      } else stop("scenario must be 'increasing biomass', 'decreasing biomass', or 'no change'")
    }
    
    
      
    # Set F time series
    f0 <- c(rep(df_f$Fmax[1], times = n_burn), df_f$Fmax)
    f <- c(f0, rep(df_f$Fmax[nrow(df_f)], times = n_scenario))
    
    # Set F for scenario years
    if (driver == "f") {
      if (scenario == "increasing biomass") {
        # decrease f by x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          0.5^(1:n_scenario)
      } else if (scenario == "decreasing biomass"){
        # increase f by x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          1.5^(1:n_scenario)
      } else if (scenario == "no change") {
        f <- f
      } else stop("scenario must be 'increasing', 'decreasing', or 'no change'")
    }

    fsel_a  <- c(0.01, 0.2, 0.6, # Fishing selectivity
                rep(1.0, times = n_ages - 3))
    
    
    # Initial abundance (in ten thousands):
    abund_tru[1,]  <- seq(from = 50000*1000, 
                          to = 50000, 
                          length.out = n_ages)   
    
    # Simulate population with fishery:
    for(t in 1:(n_t - 1)){
      Z_a <- f[t]*fsel_a + m
      # Spawning occurs before fishing
      SSB[t]  <- sum(abund_tru[t,] * w_a * mat_a * exp(-Z_a * phi))
      abund_tru[t+1,1] <- rlnorm(n = 1,       # Realized recruitment.
                                 m = log(r[t]) - 0.5 * sdR^2,
                                 s = sdR)
      
      abund_tru[t+1,-1] <- exp(-Z_a[-n_ages]) * abund_tru[t,-n_ages] # Transition
      # Plus group
      abund_tru[t+1,n_ages] <- abund_tru[t+1,n_ages] + exp(-Z_a[n_ages]) * abund_tru[t,n_ages]
      C[t,]   <- abund_tru[t,] * exp(-m) * (1 - exp(-f[t] * fsel_a)) * w_a # Catch (biomass).
    }
    
    # Simulate multiple surveys with different selectivities,
    # independent log-normal error and an outlier in one year.
    abund_obs <- array(data = NA,
                       dim = c(n_t, n_ages, n_surveys),
                       dimnames = list(NULL,
                                       paste0("age", 1:n_ages),
                                       paste0("survey", 1:n_surveys)))
    # Set obs error cor to cor of real data
    obs_cov <- 0.5*cor_mat*sdO*sdO
    diag(obs_cov) <- sdO^2
    
    outlier_ind <- sample((n_burn + 1):n_t, size = 1)
    outlier_year <- c((terminal_year - n_t + 1):terminal_year)[outlier_ind]
    sd_multiplier_vect <- rep(1, times = n_t)
    sd_multiplier_vect[outlier_ind] <- sd_multiplier
    
    # Fall survey selectivity
    fall_select <- c(0.1, 0.5, rep(1, times = n_ages - 2))
    # Spring survey selectivity
    spring_select <- c(0.01, 0.25, rep(1, times = n_ages - 2))
    # DFO survey selectivity
    dfo_select <- spring_select
    survey_select <- array(data = c(fall_select, 
                                    spring_select,
                                    dfo_select),
                           dim = c(length(spring_select), 1, n_surveys))
    
    for (i in 1:n_t) {
      obs_error <-  exp(sd_multiplier_vect[i] *
                        MASS::mvrnorm(mu = rep(0, 
                                               times = n_surveys), 
                                      Sigma = obs_cov))
      for (j in 1:n_ages) {
        abund_obs[i,j,] <- abund_tru[i,j] * 
                           survey_select[j,1,] * 
                           obs_error *
                           # Take this out if you want numbers caught:
                           1/survey_select[j,1,] # Expand abundance
      }
    }
    
    # Calculate biomass:
    biomass_tru <- abund_tru %*% w_a / 1000 / 1000 # convert kg to 1000mt
    biomass_obs <- matrix(nrow = n_t, ncol = n_surveys)
    colnames(biomass_obs) <- paste0("survey", 1:n_surveys)
    
    for (i in 1:n_surveys) {
      biomass_obs[,i] <- abund_obs[,,i] %*% w_a / 1000 / 1000 # convert kg to 1000mt
    }
    
    df2return <- 
      data.frame(year = (2013 + n_scenario - n_t + 1):(2013 + n_scenario),
                 outlier_year = outlier_year,
                 biomass_obs = biomass_obs,
                 biomass_obs_log = log(biomass_obs),
                 biomass_tru = biomass_tru,
                 abund_tru = abund_tru,
                 abund_obs = abund_obs)
    
    if (return_burn == FALSE) {
      df2return %<>% dplyr::filter(year >= (terminal_year - n_sim + 1)) 
    }
    
    return(df2return)
}

## Fit TMB and average model ------------------------------
fit_models <- function(...,
                       year,
                       log_biomass_survey1, 
                       log_biomass_survey2, 
                       log_biomass_survey3) {
  df2fit <- 
    data.frame(year,
               log_biomass_survey1, 
               log_biomass_survey2, 
               log_biomass_survey3)
  
  # Set up data and parameters
  dat <- 
    list(log_obs_DFO    = df2fit$log_biomass_survey1,
         log_obs_spring = df2fit$log_biomass_survey2,
         log_obs_fall   = df2fit$log_biomass_survey3)
  
  # Initialize parameters
  parameters <- 
    list(logB = rep(0, length(dat$log_obs_DFO)),
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
  df_fit  <-
    data.frame(year = df2fit$year %>% unique,
               `biomass_rw` = ests$est.log.pop[,"Estimate"] %>% exp,
               `biomass_rw_hi95` = (ests$est.log.pop[,"Estimate"] +
                                   1.96*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               `biomass_rw_lo95` = (ests$est.log.pop[,"Estimate"] -
                                   1.96*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               `biomass_rw_hi90` = (ests$est.log.pop[,"Estimate"] +
                                      1.645*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               `biomass_rw_lo90` = (ests$est.log.pop[,"Estimate"] -
                                      1.645*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               `biomass_rw_hi75` = (ests$est.log.pop[,"Estimate"] +
                                      1.15*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               `biomass_rw_lo75` = (ests$est.log.pop[,"Estimate"] -
                                      1.15*ests$est.log.pop[,"Std. Error"]) %>%
                 exp,
               biomass_average = 
                 rowMeans(cbind(df2fit$log_biomass_survey1 %>% exp, 
                                df2fit$log_biomass_survey2 %>% exp, 
                                df2fit$log_biomass_survey3 %>% exp)),
               
               check.names = F)
}
