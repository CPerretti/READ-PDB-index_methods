source("6-calc_error.R")
# Make plots of real data


# Plot on 1000 mt scale
df_line <-
  df_real_withfit %>% 
  dplyr::filter(variable %in% c("Average",
                                "biomass_rw"))
df_point <-
  df_real_withfit %>% 
  dplyr::filter(variable %in% c("DFO", 
                                "Spring", 
                                "Fall"))

df_ribbon <-
  df_real_withfit %>% 
  dplyr::filter(variable %in% c("biomass_rw_hi95", 
                                "biomass_rw_lo95")) %>%
  dplyr::select(variable, year, value) %>%
  tidyr::spread(variable, value)

ggplot(data = df_line, aes(x = year)) +
  geom_line(aes(y = value, color = variable)) +
  geom_point(data = df_point,
             aes(y = value, shape = variable)) +
  scale_color_manual(values = c("red", 
                                "blue")) +
  geom_ribbon(data = df_ribbon,
              aes(x = year, 
                  ymin = biomass_rw_lo95, 
                  ymax = biomass_rw_hi95),
              alpha = 0.3,
              fill = "blue",
              size = 0.1) +
  theme(legend.title = element_blank()) +
  ylab("Biomass (1000 mt)")



