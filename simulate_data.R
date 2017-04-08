source("3-clean.R")


## Simulate data ------------------------------------------
df_sims <- 
  run_sim(n_ages      = 10,
          n_surveys   = 2,
          sd_multiplier = 4,
          n_burn      = 100,
          n_sim       = 30,
          return_burn = FALSE)

## Setup data for plots -----------------------------------
outlier_year <- df_sims$outlier_year %>% unique
df2p <-
  df_sims %>%
  tidyr::gather(variable, value, -year)


## Plot simulated age structure ---------------------------
ggplot(df2p %>% dplyr::filter(variable %in% c("abund_obs.age1.survey1",
                                               "abund_obs.age1.survey2",
                                               "abund_obs.age5.survey1",
                                               "abund_obs.age5.survey2",
                                               "abund_obs.age9.survey1",
                                               "abund_obs.age9.survey2")),
       aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 0.3) +
  geom_vline(xintercept = dat2p %>% 
                          dplyr::filter(variable == "outlier_year") %$%
                          value %>% unique) +
  xlab("Year") +
  ylab("Abundance") +
  theme(legend.title = element_blank())

## Plot simulated biomass ---------------------------------
df <- 
  df2p %>% dplyr::filter(variable %in% c("biomass_obs.survey1",
                                         "biomass_obs.survey2",
                                         "biomass_tru"))
df_outlier <- df %>% dplyr::filter(year == outlier_year) 
ggplot(df,
       aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point(data   = df_outlier, 
             colour = "black", 
             shape  = 1, 
             size   = 5) +
  xlab("Year") +
  ylab("Biomass") +
  theme(legend.title = element_blank())


