source("7-plot_real.R")

df2cor <- 
  data.frame(
   Year = dplyr::filter(df_yt, Survey == "DFO")$Year,
   DFO = dplyr::filter(df_yt, Survey == "DFO")$biomass,
   Spring = dplyr::filter(df_yt, Survey == "Spring")$biomass,
   Fall = dplyr::filter(df_yt, Survey == "Fall")$biomass)

df_loess <-
  data.frame(Year = df2cor$Year,
             DFO_fit = fitted(loess(DFO ~ Year, df2cor)),
             Spring_fit = fitted(loess(Spring ~ Year, df2cor)),
             Fall_fit = fitted(loess(Fall ~ Year, df2cor)),
             DFO_res = resid(loess(DFO ~ Year, df2cor)),
             Spring_res = resid(loess(Spring ~ Year, df2cor)),
             Fall_res = resid(loess(Fall ~ Year, df2cor)))

cor(df_loess %>% dplyr::select(DFO_res,
                               Spring_res,
                               Fall_res)) %>%
  round(digits = 3)

df_all <-
  df2cor %>%
  dplyr::left_join(df_loess)


df2p <-
  df_all %>%
  tidyr::gather(variable, value, -Year)
  
# Plot surveys with loess fits
ggplot(df2p, aes(x = Year, y = value)) +
  geom_point(data =
               df2p %>% 
               dplyr::filter(variable %in% c("DFO",
                                             "Spring",
                                             "Fall")),
             aes(shape = variable)) +
  geom_line(data =
              df2p %>% 
              dplyr::filter(variable %in% c("DFO_fit",
                                            "Spring_fit",
                                            "Fall_fit")),
            aes(color = variable)) +
  ylab("Biomass (1000 mt)")


# Plot residuals vs year

ggplot(df2p, aes(x = Year, y = value)) +
  geom_line(data =
              df2p %>% 
              dplyr::filter(variable %in% c("DFO_res",
                                            "Spring_res",
                                            "Fall_res")),
            aes(color = variable)) +
  ylab("Residual of loess fit")