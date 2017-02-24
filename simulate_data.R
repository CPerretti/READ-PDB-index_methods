source("1-func.R")

df_sims <- 
  run_sim(n_seas     = 2,
          seas_ratio = 0.92,
          maxT       = 30)


## Plot simulated data
dat2p <-
  df_sims %>% 
  dplyr::mutate(t = ifelse(seas == "Fall", 
                           year + 0.5, 
                           year),
                year = 1986 + t) %>%
  dplyr::select(-t) %>%
  tidyr::gather(variable, value, -year, -seas)


ggplot(dat2p,
       aes(x = year, y = value, color = variable)) +
  geom_line() +
  xlab("Year") +
  ylab("Biomass") +
  theme(legend.title = element_blank())




