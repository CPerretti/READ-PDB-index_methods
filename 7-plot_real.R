source("6-fit_sim.R")

## Set up data for plots ----------------------------------
df2p_yt <-
  df_yt %>%
  # add rw fit to original dataframe
  dplyr::bind_rows({df_fit_real %>%
                    tidyr::gather(Survey, biomass, -Year)}) %>%
  # rename variables
  dplyr::select(Year, Survey, biomass) %>%
  dplyr::rename(variable = Survey,
                value = biomass,
                year = Year)


## Plot on 1000 mt scale ----
df_line <-
  df2p_yt %>% 
  dplyr::filter(variable %in% c("Average",
                                "RW"))
df_point <-
  df2p_yt %>% 
  dplyr::filter(variable %in% c("DFO", 
                                "Spring", 
                                "Fall"))

df_ribbon <-
  df2p_yt %>% 
  dplyr::filter(variable %in% c("RW hi", 
                                "RW lo")) %>%
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
                  ymin = `RW lo`, 
                  ymax = `RW hi`),
              alpha = 0.3,
              fill = "blue",
              size = 0.1) +
  theme(legend.title = element_blank()) +
  ylab("Biomass (1000 mt)")



