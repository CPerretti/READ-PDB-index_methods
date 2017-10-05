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
    
    # Model parameters:
    w_a   <- c(0.148, 0.317, 0.453, 0.588, 0.724, 
              rep(0.921, times = n_ages - 5)) # Weights
    m     <- 0.4 # Natural mortality
    
    sdR   <- 0.1             # Recruitment variability
    sdO   <- 0.2             # Observation error
    
    
    # Setup R time series
    r0 <- c(rep(df_r$R[1], times = n_burn), df_r$R)
    r <- c(r0, rep(df_r$R[nrow(df_r)], times = n_scenario))  
    
    # Set R for scenario years
    if (driver == "r"){
      if (scenario == "increasing slowly") {
        # increase by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          seq(from = 1.39, to = 1.39, length.out = n_scenario)
      } else if (scenario == "increasing rapidly") {
        # increase by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          seq(from = 1.7, to = 1.7, length.out = n_scenario)
      } else if (scenario == "decreasing slowly") {
        # decrease by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          seq(from = 0.8, to = 0.8, length.out = n_scenario)
      } else if (scenario == "decreasing rapidly") {
        # decrease by r x% per year
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          seq(from = 0.5, to = 0.5, length.out = n_scenario)
      } else if (scenario == "no change") {
        r[(length(r)-n_scenario + 1):length(r)] <-
          r[(length(r)-n_scenario + 1):length(r)] *
          seq(from = 1.1, to = 1.1, length.out = n_scenario)
      } else stop("scenario must be 'increasing rapidly', 
                  'increasing slowly', 'decreasing rapidly', 
                  'decreasing slowly', 'decreasing biomass', 
                  or 'no change'")
    }
    
    
      
    # Set F time series
    f0 <- c(rep(df_f$Fmax[1], times = n_burn), df_f$Fmax)
    f <- c(f0, rep(df_f$Fmax[nrow(df_f)], times = n_scenario))
    
    # Set F for scenario years
    if (driver == "f"){
      if (scenario == "increasing slowly") {
        # increase by f x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          seq(from = 0.25, to = 0.25, length.out = n_scenario)
      } else if (scenario == "increasing rapidly") {
        # increase by f x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          seq(from = 0.0, to = 0.0, length.out = n_scenario)
      } else if (scenario == "decreasing slowly") {
        # decrease by f x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          seq(from = 2.15, to = 2.15, length.out = n_scenario)
      } else if (scenario == "decreasing rapidly") {
        # decrease by f x% per year
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          seq(from = 11.9, to = 11.9, length.out = n_scenario)
      } else if (scenario == "no change") {
        f[(length(f)-n_scenario + 1):length(f)] <-
          f[(length(f)-n_scenario + 1):length(f)] *
          seq(from = 0.72, to = 0.72, length.out = n_scenario)
      } else stop("scenario must be 'increasing rapidly', 
                  'increasing slowly', 'decreasing rapidly', 
                  'decreasing slowly', 'decreasing biomass', 
                  or 'no change'")
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
      abund_tru[t+1,1] <- rlnorm(n = 1,       # Realized recruitment.
                                 m = log(r[t]) - 0.5 * sdR^2,
                                 s = sdR)
      
      abund_tru[t+1,-1] <- exp(-Z_a[-n_ages]) * abund_tru[t,-n_ages] # Transition
      # Plus group
      abund_tru[t+1,n_ages] <- abund_tru[t+1,n_ages] + exp(-Z_a[n_ages]) * abund_tru[t,n_ages]
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
      df2return %<>% dplyr::filter(year >= (terminal_year - n_sim - n_scenario + 1)) 
    }
    
    return(df2return)
}

## Fit TMB and average model ------------------------------
fit_models <- function(...,
                       year,
                       log_biomass_survey1, 
                       log_biomass_survey2, 
                       log_biomass_survey3,
                       biomass_tru) {
  df2fit <- 
    data.frame(year,
               log_biomass_survey1, 
               log_biomass_survey2, 
               log_biomass_survey3)

  
  # Fit model once for each year in the scenario
  biomass_rw_all <- data.frame()
  
  for (i in 1:(n_scenario)) {
    # Setup data
    ind <- 1:(nrow(df2fit) - n_scenario + i)

    dat <- 
      list(log_obs_DFO    = df2fit$log_biomass_survey1[ind],
           log_obs_spring = df2fit$log_biomass_survey2[ind],
           log_obs_fall   = df2fit$log_biomass_survey3[ind]
           )
    
    # Initialize parameters
    parameters <- 
      list(logB = rep(0, length(dat$log_obs_DFO)),
           log_process_error    = 0,
           log_obs_error_DFO    = 0,
           log_obs_error_spring = 0,
           log_obs_error_fall   = 0)
    
    obj1 <- MakeADFun(dat[1],
                      parameters[1:3],
                      DLL = "yt_rw_1surv",
                      random = c("logB"),
                      silent = TRUE)
    
    obj2 <- MakeADFun(dat[1:2],
                      parameters[1:4],
                      DLL = "yt_rw_2surv",
                      random = c("logB"),
                      silent = TRUE)
    
    obj3 <- MakeADFun(dat,
                      parameters,
                      DLL = "yt_rw_3surv",
                      random = c("logB"),
                      silent = TRUE)
    
    opt1 <- nlminb(obj1$par, obj1$fn, obj1$gr,
                   control = list(iter.max = 1000,
                                  eval.max = 1000))
    
    opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr,
                   control = list(iter.max = 1000,
                                  eval.max = 1000))
    
    opt3 <- nlminb(obj3$par, obj3$fn, obj3$gr,
                  control = list(iter.max = 1000,
                                 eval.max = 1000))
    
    srep1 <- summary(sdreport(obj1))
    
    srep2 <- summary(sdreport(obj2))
    
    srep3 <- summary(sdreport(obj3))
    
    
    # Extract parameter estimates from TMB output
    ests1 <- return_ests(srep = srep1)
    
    ests2 <- return_ests(srep = srep2)
    
    ests3 <- return_ests(srep = srep3)
    
    biomass_rw_all <-
      dplyr::bind_rows(biomass_rw_all,
          data.frame(year = rep(unique(df2fit$year[ind]), times = 3),
                     model = rep(c(paste0("biomass_rw_1surv_term",
                                          df2fit$year[ind] %>% max),
                                   paste0("biomass_rw_2surv_term",
                                          df2fit$year[ind] %>% max),
                                   paste0("biomass_rw_3surv_term",
                                    df2fit$year[ind] %>% max)),
                                 each = length(ind)),
                     fit = c(ests1$est.log.pop[,"Estimate"] %>% exp,
                             ests2$est.log.pop[,"Estimate"] %>% exp,
                             ests3$est.log.pop[,"Estimate"] %>% exp),
                     se = c(ests1$est.log.pop[,"Std. Error"] %>% exp,
                            ests2$est.log.pop[,"Std. Error"] %>% exp,
                            ests3$est.log.pop[,"Std. Error"] %>% exp))) %>%
      dplyr::mutate(model = as.character(model))
      
  }
  
  # Organize fit
  biomass_rw_term <-
    biomass_rw_all %>%
    dplyr::select(-se) %>%
    tidyr::spread(model, fit, fill = NA)
  biomass_rw_se <-
    biomass_rw_all %>%
    dplyr::select(-fit) %>%
    dplyr::mutate(model = paste0(model, "_se")) %>%
    tidyr::spread(model, se, fill = NA)
  
  
  df_fit  <-
    biomass_rw_term %>%
    dplyr::left_join(by = "year",
                     biomass_rw_se) %>%
    dplyr::left_join(by = "year",
    data.frame(year = df2fit$year %>% unique,
               biomass_average_1surv_term = 
                 rowMeans(cbind(df2fit$log_biomass_survey1 %>% exp#, 
                                #df2fit$log_biomass_survey2 %>% exp, 
                                #df2fit$log_biomass_survey3 %>% exp
                 )),
               biomass_average_2surv_term = 
                 rowMeans(cbind(df2fit$log_biomass_survey1 %>% exp, 
                                df2fit$log_biomass_survey2 %>% exp#, 
                                #df2fit$log_biomass_survey3 %>% exp
                 )),
               biomass_average_3surv_term = 
                 rowMeans(cbind(df2fit$log_biomass_survey1 %>% exp, 
                                df2fit$log_biomass_survey2 %>% exp, 
                                df2fit$log_biomass_survey3 %>% exp
                                )),
               biomass_tru = biomass_tru,
               check.names = F))
}

## Plot time series with model fits -----------------------
plot_fit <- function(df_sims, 
                     rep2p, 
                     scen2p, 
                     driver2p, 
                     df_sims_withfit,
                     titl,
                     filename) {
  
  outlier_year <- 
    df_sims %>%
    dplyr::filter(rep == rep2p,
                  scenario == scen2p,
                  driver == driver2p) %$%
    outlier_year %>%
    unique
  
  # Choose replicate to plot
  df_sims_withfit2p <-
    df_sims_withfit %>%
    dplyr::filter(rep == rep2p,
                  driver == driver2p,
                  scenario == scen2p)
  
  df_line <-
    df_sims_withfit2p %>% 
    dplyr::filter(variable %in% c("Empirical fit",
                                  "State-space fit",
                                  "True"))
  
  df_point <-
    df_sims_withfit2p %>% 
    dplyr::filter(variable %in% c("Survey 1",
                                  "Survey 2",
                                  "Survey 3"))
  
  df_ribbon <-
    df_sims_withfit2p %>% 
    dplyr::filter(variable %in% c("State-space fit", 
                                  "biomass_rw_se")) %>%
    dplyr::select(variable, year, value) %>%
    tidyr::spread(variable, value) %>%
    dplyr::mutate(biomass_rw_lo95 = (log(`State-space fit`) - 
                                       1.96 * log(biomass_rw_se)) %>%
                    exp,
                  biomass_rw_hi95 = (log(`State-space fit`) + 
                                       1.96 * log(biomass_rw_se)) %>%
                    exp)
  
  df_outlier <- 
    df_sims_withfit2p %>%
    dplyr::filter(variable %in% c("Survey 1",
                                  "Survey 2",
                                  "Survey 3")) %>%
    dplyr::filter(year == outlier_year) 
  
p <-
  ggplot(df_sims_withfit2p, aes(x = year)) +
    geom_point(data   = df_outlier, 
               colour = "black", 
               shape  = 1, 
               size   = 5,
               aes(y = value)) +
    geom_line(data = df_line,
              aes(y = value, color = variable)) +
    geom_point(data = df_point,
               aes(y = value, shape = variable)) +
    scale_color_manual(values = c("red",
                                  "blue", 
                                  "black")) +
    geom_ribbon(data = df_ribbon,
                aes(x = year, 
                    ymin = biomass_rw_lo95, 
                    ymax = biomass_rw_hi95),
                alpha = 0.3,
                fill = "blue",
                size = 0.1) +
    geom_vline(xintercept = max(df_sims_withfit2p$year) - n_scenario) +
    xlab("Year") +
    ylab("Biomass (1000 mt)") +
    theme(legend.title = element_blank()) + 
    ggtitle(titl)
  
  print(p)
  
  ggsave(filename, width = 6.5, height = 4.5)
}
