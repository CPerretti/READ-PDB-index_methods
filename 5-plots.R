# Make plots of model fits:
source("4-run_models.R")



## Fit MARSS model ----------------------------------------
# dat_yt2marss <-
#   df_yt %>% 
#   dplyr::filter(Survey != "Average") %>%
#   dplyr::select(Year, Survey, log_biomass) %>%
#   tidyr::spread(Survey, log_biomass) %>%
#   dplyr::select(-Year) %>%
#   t %>%
#   as.matrix
# 
# 
# # Set up model parameters
# marss_params <- 
#   list(B = matrix(1),
#        U = matrix(0),
#        Q = matrix("q"),
#        Z = matrix(c(1, 1, 1), 3, 1),
#        A = matrix(c(0, 0, 0), 3, 1),
#        R = "diagonal and unequal")
# 
# marss_out <- MARSS::MARSS(y = dat_yt2marss, 
#                           model = marss_params)
# 
# marss_fits <- 
#   data.frame(Year = df_yt$Year %>% unique,
#              `RW in MARSS` = marss_out$states %>%
#                            exp %>% t %>% as.vector,
#              `RW in MARSS lo` = (marss_out$states - 
#                                2*marss_out$states.se) %>%
#                                exp %>% t %>% as.vector,
#              `RW in MARSS hi` = (marss_out$states + 
#                                2*marss_out$states.se) %>%
#                                exp %>% t %>% as.vector,
#              check.names = F)

## Add model fits to to original data frame ---------------
df_yt %<>%
  # Add MARSS fit to original dataframe
  dplyr::bind_rows(#{marss_fits %>%
                  # tidyr::gather(Survey, biomass, -Year)},
                   {fits_wfloors %>%
                    tidyr::gather(Survey, biomass, -Year)},
                   {fits_nofloors %>%
                    tidyr::gather(Survey, biomass, -Year)})


## Plot on 1000 mt scale ----
dat2line <-
  df_yt %>% 
  dplyr::filter(Survey %in% c("Average",
                              "RW with obs floor",
                              "RW no obs floor"))
dat2point <-
  df_yt %>% 
  dplyr::filter(Survey %in% c("DFO", 
                              "Spring", 
                              "Fall"))

dat2ribbon_wfloor <-
  df_yt %>% 
  dplyr::filter(Survey %in% c("RW with obs floor hi", 
                              "RW with obs floor lo")) %>%
  dplyr::select(Survey, Year, biomass) %>%
  tidyr::spread(Survey, biomass)
  
dat2ribbon_nofloor <-
  df_yt %>% 
  dplyr::filter(Survey %in% c("RW no obs floor hi", 
                              "RW no obs floor lo")) %>%
  dplyr::select(Survey, Year, biomass) %>%
  tidyr::spread(Survey, biomass)

ggplot(data = dat2line, aes(x = Year)) +
  geom_line(aes(y = biomass, color = Survey)) +
  geom_point(data = dat2point,
             aes(y = biomass, shape = Survey)) +
  scale_color_manual(values = c("black", 
                                "red", "blue")) +
  geom_ribbon(data = dat2ribbon_wfloor,
              aes(x = Year, 
                  ymin = `RW with obs floor lo`, 
                  ymax = `RW with obs floor hi`),
              alpha = 0.2,
              fill = "blue",
              size = 0.1) +
  geom_ribbon(data = dat2ribbon_nofloor,
              aes(x = Year, 
                  ymin = `RW no obs floor lo`, 
                  ymax = `RW no obs floor hi`),
              alpha = 0.2,
              fill = "red",
              size = 0.1) +
  theme(legend.title = element_blank()) +
  ylab("Biomass (1000 mt)")



