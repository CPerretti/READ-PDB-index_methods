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
                    return_burn = FALSE){
    
    n_t <- n_burn + n_sim
      
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
    
    ssb_ind <- 2:n_ages       # Ages that spawn
    a      <- 5.0              # S-R parameter
    b      <- 0.1              # S-R parameter
    sdR    <- 0.6              # Recruitment variability
    sdO    <- 0.2              # Observation error
    
    f      <- c(rep(0, times = n_burn),
                seq(from = 0.0,   # Fishing mort rate
                    to   = 0.0,
                    length.out = ceiling(n_sim/2)),
                seq(from = 0.0,
                    to   = 0.0,
                    length.out = floor(n_sim/2)))
    
    fsel_a  <- c(0.01, 0.2, 0.6, # Fishing selectivity
                rep(1.0, times = n_ages - 3))
    
    
    # Initial abundance:
    abund_tru[1,]  <- seq(from = 20, to = 1, length.out = n_ages)   
    
    # Simulate population with fishery:
    for(t in 1:(n_t - 1)){
      Z_a <- f[t]*fsel_a + m
      # Spawning occurs before fishing
      SSB[t]  <- sum(abund_tru[t,] * w_a * mat_a * exp(-Z_a * phi))
      bh       <- a * SSB[t]/(1 + b * SSB[t])  # Mean recruitment
      abund_tru[t+1,1] <- rlnorm(n = 1,       # Realized recruitment.
                                 m = log(bh) - 0.5 * sdR^2,
                                 s = sdR)
      
      abund_tru[t+1,-1] <- exp(-Z_a[-n_ages]) * abund_tru[t,-n_ages] # Transition
      # I think the following line creates a plus group
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
    
    obs_cov <- matrix(data = sdO^2/2, 
                      nrow = n_surveys, 
                      ncol = n_surveys)
    diag(obs_cov) <- rep(sdO^2, times = n_surveys)
    
    outlier_ind <- sample((n_burn + 1):n_t, size = 1)
    outlier_year <- c((2016 - n_t + 1):2016)[outlier_ind]
    sd_multiplier_vect <- rep(1, times = n_t)
    sd_multiplier_vect[outlier_ind] <- sd_multiplier
    
    # Fall survey selectivity
    fall_select <- c(0.1, 0.5, rep(1, times = n_ages - 2))
    # Spring survey selectivity
    spring_select <- c(0.01, 0.25, rep(1, times = n_ages - 2))
    survey_select <- array(data = c(fall_select, spring_select),
                           dim = c(length(spring_select), 1, n_surveys))
    
    for (i in 1:n_t) {
      obs_error <-  exp(sd_multiplier_vect[i] *
                        MASS::mvrnorm(mu = rep(0, 
                                               times = n_surveys), 
                                      Sigma = obs_cov))
      for (j in 1:n_ages) {
        abund_obs[i,j,] <- abund_tru[i,j] * survey_select[j,1,] * obs_error
      }
    }
    
    # Calculate biomass:
    biomass_tru <- abund_tru %*% w_a
    biomass_obs <- matrix(nrow = n_t, ncol = n_surveys)
    colnames(biomass_obs) <- paste0("survey", 1:n_surveys)
    
    for (i in 1:n_surveys) {
      biomass_obs[,i] <- abund_obs[,,i] %*% w_a
    }
    
    df2return <- 
      data.frame(year = (2016 - n_t + 1):2016,
                 outlier_year = outlier_year,
                 biomass_obs = biomass_obs,
                 biomass_tru = biomass_tru,
                 abund_tru = abund_tru,
                 abund_obs = abund_obs)
    
    if (return_burn == FALSE) {
      df2return %<>% dplyr::filter(year >= (2016 - n_sim)) 
    }
    
    return(df2return)
}