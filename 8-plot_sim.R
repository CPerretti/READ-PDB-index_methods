source("7-plot_real.R")
# Make plots of simulation results

## Setup data for plots -----------------------------------
rep2p <- 2
driver2p <- "r"
scen2p <- "decreasing biomass"


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

## Plot simulated age structure ---------------------------
p_age <-
  ggplot(df_sims_withfit2p %>%
           dplyr::filter(variable %in% c("abund_obs.age1.survey1",
                                         "abund_obs.age1.survey2",
                                         "abund_obs.age5.survey1",
                                         "abund_obs.age5.survey2",
                                         "abund_obs.age9.survey1",
                                         "abund_obs.age9.survey2")),
         aes(x = year, y = value, color = variable)) +
    geom_line() +
    geom_point(size = 0.3) +
    geom_vline(xintercept = df_sims_withfit2p %>% 
                 dplyr::filter(variable == "outlier_year") %$%
                 value %>% unique) +
    xlab("Year") +
    ylab("Abundance") +
    theme(legend.title = element_blank())

p_age


## Plot simulated and fit biomass ---------------------------------
df_line <-
  df_sims_withfit2p %>% 
  dplyr::filter(variable %in% c("biomass_average",
                                "biomass_rw",
                                "biomass_tru"))

df_point <-
  df_sims_withfit2p %>% 
  dplyr::filter(variable %in% c("biomass_obs.survey1",
                                "biomass_obs.survey2",
                                "biomass_obs.survey3"))

df_ribbon <-
  df_sims_withfit2p %>% 
  dplyr::filter(variable %in% c("biomass_rw_hi95", 
                                "biomass_rw_lo95")) %>%
  dplyr::select(variable, year, value) %>%
  tidyr::spread(variable, value)

df_outlier <- 
  df_sims_withfit2p %>%
  dplyr::filter(variable %in% c("biomass_obs.survey1",
                                "biomass_obs.survey2",
                                "biomass_obs.survey3")) %>%
  dplyr::filter(year == outlier_year) 

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
  theme(legend.title = element_blank())


## Plot error comparison ----------------------------------
p_err <-
  ggplot(df_errors, aes(x = method, y = mae, fill = smooth)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mae - mae_ci, ymax = mae + mae_ci),
                width = .2,                 
                position = position_dodge(.9)) +
  facet_grid(driver~scenario) +
  ylab("Mean absolute error") +
  xlab("Model") +
  guides(fill = guide_legend(title = NULL))

p_err

## Print coverage performance ------------------------------
print(df_coverage)


## Plot coverage deciles for rw in 2016 -------------------
p_decile <-
  ggplot(df_coverage_decile, aes(x=decile)) +
  stat_count() +
  facet_grid(driver~scenario) +
  scale_x_discrete(limits = seq(0,9))

p_decile



## Plot terminal year fit vs observed for just rw ----------
df_fitvsobs <-
  df_sims_withfit %>%
  dplyr::filter(year > (terminal_year - n_scenario),
                variable %in% c("biomass_tru",
                                "biomass_rw",
                                "biomass_rw_3ymean",
                                "biomass_average",
                                "biomass_average_3ymean"
                                )) %>%
  tidyr::spread(variable, value) %>%
  tidyr::gather(method, value, 
                -rep, -driver, -scenario, -year, -biomass_tru)

# Just rw
ggplot(df_fitvsobs %>%
         dplyr::filter(method %in% c("biomass_tru",
                                     "biomass_rw")),
       aes(x = biomass_tru, y = value)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_abline() +
  facet_grid(driver~scenario, scales = "free") +
  xlab("True state in terminal year") +
  ylab("Estimated state in terminal year")

# All methods
ggplot(df_fitvsobs,
       aes(x = biomass_tru, y = value)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_abline() +
  facet_grid(method~driver*scenario, scales = "free") +
  xlab("True state in terminal year") +
  ylab("Estimated state in terminal year")
