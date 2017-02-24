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
run_sim <- function(maxT, 
                    n_seas, 
                    seas_ratio, 
                    noise){
    # Simulation parameters:
    n_ages <- 4                # Number of ages.
    
    year <- rep(1:maxT, each = n_seas) # Year index.
    # Pre-allocate matrices:
    N      <- matrix(data = NA, # Abundance.
                     nrow = maxT,
                     ncol = n_ages)
    C      <- matrix(data = NA, # Catch-at-age.
                     nrow = maxT,
                     ncol = n_ages)
    B      <- matrix(data = NA, # Biomass.
                     nrow = maxT,
                     ncol = 1)
    E      <- matrix(data = NA, # Eggs.
                     nrow = maxT,
                     ncol = 1)
    v_a    <- seq(from = 0,     # Vulnerabilities. 
                  to   = 1,
                  length.out = n_ages)
    w_a    <- seq(from = 10,    # Weights.
                  to   = 1000,
                  length.out = n_ages)  
    s_a    <- matrix(data = 0,  # Survivals.
                     nrow = n_ages-1, 
                     ncol = n_ages-1)
    
    # Model parameters:
    diag(s_a) <- (1 - 0.2)     # 1 - mortality.
    e_a    <- seq(from = 0,    # Fecundities. 
                  to   = 1000,
                  length.out = n_ages)
    a      <- 5.0              # S-R parameter.
    b      <- 0.1              # S-R parameter.
    sdR    <- 0.1              # SD of the process error.
    sdO    <- 0.2
    #sdO_high <- 1.02           # SD of high observation error.
 
    #if(noise=="low"){          # SD of observation error.
    #  sdO  <- 0.25 * sdO_high            
    #} else if(noise=="med"){
    #  sdO <- 0.50 * sdO_high
    #} else if(noise=="high"){
    #  sdO <- sdO_high
    #}
    
      f      <- c(seq(from = 0.9,   # Trend scenario.
                      to   = 0.3,
                      length.out = ceiling(maxT/2)),
                  seq(from = 0.3,
                      to   = 0.9,
                      length.out = floor(maxT/2)))
    
    
    # Initial conditions:
    if(f[1]==0.2){                # Initial abundance.
      N[1,]  <- c(60, 40, 25, 20)
    } else if(f[1]==0.9){
      N[1,]  <- c(50, 25, 8, 0.1)   
    }
    B[1]   <- sum(N[1,]*w_a)      # Initial biomass.
    E[1]   <- sum(N[1,]*e_a)      # Initial eggs.
    
    # Simulate fishery:
    for(t in 2:maxT){
      
      bh      <- a*E[t-1]/(1 + b*E[t-1])
      N[t,1]  <- rlnorm(n = 1,             # Recruitment.
                        m = log(bh) - 
                          0.5*sdR^2,
                        s = sdR)
      N[t,-1] <- s_a%*%N[t-1,-n_ages]      # Survival.        
      C[t,]   <- N[t,]*f[t]*v_a*w_a        # Catch (biomass).
      N[t,]   <- N[t,] - C[t,]/w_a         # Abundance.
      B[t]    <- sum(N[t,]*w_a)            # Total biomass.
      E[t]    <- sum(N[t,]*e_a)            # Total eggs produced.
      
    }
    
    # Add seasonality:
    min2max_ratio <- seas_ratio %>% sample(1)
    spr <- c(min2max_ratio, 1) %>% sample(1) # Spring availability
    
    if(spr == 1) {
      fal <- min2max_ratio  # Fall availability
    } else { fal <- 1 }
    
    N_sea_tru <- N[rep(1:nrow(N), # True seasonal abundance. 
                       each = n_seas),] *
      rep(x    = c(spr, fal),
          leng = nrow(N)*n_seas)
    seas <- rep(c("Spring","Fall"), leng = n_seas*maxT)
    
    
    # Add observation error:
    # Randomly an observation to make outlier by increasing
    # its variance:
    sdO_outlier <- rep(sdO, times = maxT*n_seas)
    sdO_outlier[sample(1:c(maxT*n_seas), size = 1)] <- 10*sdO
      
    N_sea_obs_vect <- rlnorm(n = prod(dim(N_sea_tru)),
                             m = log(N_sea_tru) - 
                               0.5*sdO_outlier^2,
                             s = sdO_outlier)
    # Convert to matrix:
    N_sea_obs <- matrix(data = N_sea_obs_vect, 
                        nrow = maxT*n_seas,
                        ncol = n_ages)
    
    # Calculate biomass:
    Observed <- as.vector(N_sea_obs%*%w_a)
    True     <- as.vector(N_sea_tru%*%w_a)
    
    return(data.frame(year = year,
                      seas = seas,
                      Observed = Observed,
                      True  = True))
}