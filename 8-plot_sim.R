source("7-plot_real.R")

##### Figures for SIMULATIONS

## Setup data for plots -----------------------------------
outlier_year <- 
  df_sims$outlier_year %>% 
  unique

df2p_sims <-
  df_sims %>%
  tidyr::gather(variable, value, -year, -outlier_year) %>%
  dplyr::bind_rows({df_fit_sim %>%
        tidyr::gather(variable, value, -year)})


## Plot simulated age structure ---------------------------
ggplot(df2p_sims %>% 
         dplyr::filter(variable %in% c("abund_obs.age1.survey1",
                                       "abund_obs.age1.survey2",
                                       "abund_obs.age5.survey1",
                                       "abund_obs.age5.survey2",
                                       "abund_obs.age9.survey1",
                                       "abund_obs.age9.survey2")),
       aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 0.3) +
  geom_vline(xintercept = df2p %>% 
               dplyr::filter(variable == "outlier_year") %$%
               value %>% unique) +
  xlab("Year") +
  ylab("Abundance") +
  theme(legend.title = element_blank())

## Plot simulated and fit biomass ---------------------------------
df_line <-
  df2p_sims %>% 
  dplyr::filter(variable %in% c("biomass_average",
                                "biomass_rw",
                                "biomass_tru"))

df_point <-
  df2p_sims %>% 
  dplyr::filter(variable %in% c("biomass_obs.survey1",
                                "biomass_obs.survey2",
                                "biomass_obs.survey3"))

df_ribbon <-
  df2p_sims %>% 
  dplyr::filter(variable %in% c("biomass_rw_hi", 
                                "biomass_rw_lo")) %>%
  dplyr::select(variable, year, value) %>%
  tidyr::spread(variable, value)

df_outlier <- 
  df2p_sims %>%
  dplyr::filter(variable %in% c("biomass_obs.survey1",
                                "biomass_obs.survey2",
                                "biomass_obs.survey3")) %>%
  dplyr::filter(year == outlier_year) 

ggplot(df2p_sims, aes(x = year)) +
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
                  ymin = biomass_rw_lo, 
                  ymax = biomass_rw_hi),
              alpha = 0.3,
              fill = "blue",
              size = 0.1) +
  xlab("Year") +
  ylab("Biomass") +
  theme(legend.title = element_blank())
