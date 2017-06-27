source("6-calc_error.R")
# Make plots of real data

# Prep data for plot
df_real_withfit1 <-
  df_real_withfit %>%
  dplyr::mutate(variable = ifelse(variable == "Average",
                                  "Empirical fit",
                                  variable),
                variable = ifelse(variable == "biomass_rw",
                                  "State-space fit",
                                  variable))

# Plot on 1000 mt scale
df_line <-
  df_real_withfit1 %>% 
  dplyr::filter(variable %in% c("Empirical fit",
                                "State-space fit"))
df_point <-
  df_real_withfit1 %>% 
  dplyr::filter(variable %in% c("DFO", 
                                "Spring", 
                                "Fall"))

df_ribbon <-
  df_real_withfit1 %>% 
  dplyr::filter(variable %in% c("biomass_rw_se", "State-space fit")) %>%
  dplyr::select(variable, year, value) %>%
  tidyr::spread(variable, value) %>%
  dplyr::mutate(biomass_rw_lo95 = (log(`State-space fit`) - 
                                    1.96 * log(biomass_rw_se)) %>%
                                    exp,
                biomass_rw_hi95 = (log(`State-space fit`) + 
                                     1.96 * log(biomass_rw_se)) %>%
                                     exp)

p <-
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
  ylab("Biomass (1000 mt)") +
  xlab("Year") +
  ggtitle("GBYT survey time series")

print(p)

ggsave("fig_ts_real.pdf", width = 5.5, height = 3.5)




