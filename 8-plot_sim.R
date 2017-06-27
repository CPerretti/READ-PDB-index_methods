source("7-plot_real.R")
# Make plots of simulation results



## Plot simulated and fit biomass ---------------------------------
# Setup data for plots
oldvar_newvar <-
  data.frame(variable = c("biomass_obs.survey1",
                          "biomass_obs.survey2",
                          "biomass_obs.survey3",
                          "biomass_average",
                          "biomass_rw",
                          "biomass_tru"),
             variable_new = c("Survey 1",
                              "Survey 2",
                              "Survey 3",
                              "Empirical fit",
                              "State-space fit",
                              "True")) %>%
  dplyr::mutate(variable = as.character(variable),
                variable_new = as.character(variable_new))

df_sims_withfit1 <-
  df_sims_withfit %>%
  dplyr::left_join(oldvar_newvar) %>%
  dplyr::mutate(variable = ifelse(!is.na(variable_new),
                                  variable_new,
                                  variable))

# 1. No change
plot_fit(df_sims = df_sims,
         rep2p = 1,
         driver2p = "r",
         scen2p = "no change",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated no change scenario",
         filename = "fig_ts_nochange.pdf")

# 2. Increasing by recruitment
plot_fit(df_sims = df_sims,
         rep2p = 1,
         driver2p = "r",
         scen2p = "increasing biomass",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated increasing by recruitment scenario",
         filename = "fig_ts_increasingbyr.pdf")

# 3. Increasing by fishing
plot_fit(df_sims = df_sims,
         rep2p = 1,
         driver2p = "f",
         scen2p = "increasing biomass",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated increasing by fishing scenario",
         filename = "fig_ts_increasingbyf.pdf")

# 4. Decreasing by recruitment
plot_fit(df_sims = df_sims,
         rep2p = 1,
         driver2p = "r",
         scen2p = "decreasing biomass",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated decreasing by recruitment scenario",
         filename = "fig_ts_decreasingbyr.pdf")

# 5. Decreasing by fishing
plot_fit(df_sims = df_sims,
         rep2p = 1,
         driver2p = "f",
         scen2p = "decreasing biomass",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated decreasing by fishing scenario",
         filename = "fig_ts_decreasingbyf.pdf")


## Plot simulated age structure ---------------------------
# p_age <-
#   ggplot(df_sims_withfit2p %>%
#            dplyr::filter(variable %in% c("abund_obs.age1.survey1",
#                                          "abund_obs.age1.survey2",
#                                          "abund_obs.age5.survey1",
#                                          "abund_obs.age5.survey2",
#                                          "abund_obs.age9.survey1",
#                                          "abund_obs.age9.survey2")),
#          aes(x = year, y = value, color = variable)) +
#     geom_line() +
#     geom_point(size = 0.3) +
#     geom_vline(xintercept = df_sims_withfit2p %>% 
#                  dplyr::filter(variable == "outlier_year") %$%
#                  value %>% unique) +
#     xlab("Year") +
#     ylab("Abundance") +
#     theme(legend.title = element_blank())

#p_age

## Plot error comparison ----------------------------------
df_errors1 <-
  df_errors %>%
  dplyr::ungroup() %>%
  dplyr::mutate(method = ifelse(method == "Average surveys",
                                "Empirical",
                                method),
                method = ifelse(method == "Random walk",
                                "State-space",
                                method),
                driver = as.character(driver),
                scenario = as.character(scenario),
                driver = ifelse(driver == "f",
                                  "Fishing driver",
                                  driver),
                driver = ifelse(driver == "r",
                                  "Recruitment driver",
                                  driver),
                scenario = ifelse(scenario == "no change",
                                  "No change",
                                  scenario),
                scenario = ifelse(scenario == "increasing biomass",
                                  "Increasing biomass",
                                  scenario),
                scenario = ifelse(scenario == "decreasing biomass",
                                  "Decreasing biomass",
                                  scenario))

p_err <-
  ggplot(df_errors1, aes(x = method, y = mae, fill = smooth)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mae - mae_ci, ymax = mae + mae_ci),
                width = .2,                 
                position = position_dodge(.9)) +
  facet_grid(driver~scenario) +
  ylab("Mean absolute error") +
  xlab("Model") +
  guides(fill = guide_legend(title = NULL))

p_err

ggsave("fig_err.pdf", width = 8.5, height = 4.5)


## Plot coverage deciles for rw in 2016 -------------------
df_coverage_decile1<-
  df_coverage_decile %>%
  dplyr::ungroup() %>%
  dplyr::mutate(driver = as.character(driver),
                scenario = as.character(scenario),
                driver = ifelse(driver == "f",
                                "Fishing driver",
                                driver),
                driver = ifelse(driver == "r",
                                "Recruitment driver",
                                driver),
                scenario = ifelse(scenario == "no change",
                                  "No change",
                                  scenario),
                scenario = ifelse(scenario == "increasing biomass",
                                  "Increasing biomass",
                                  scenario),
                scenario = ifelse(scenario == "decreasing biomass",
                                  "Decreasing biomass",
                                  scenario))

p_decile <-
  ggplot(df_coverage_decile1, aes(x=decile)) +
  geom_hline(yintercept = 500, color = "black") +
  geom_bar() +
  facet_grid(driver~scenario) +
  scale_x_discrete(limits = seq(0,9)) +
  xlab("Decile") +
  ylab("Number of observations in decile")

p_decile

ggsave("fig_decile.pdf", width = 7.5, height = 4.5)

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

## Calculate Mohn's rho for random walk estimates --------

# collect biomass estiamtes from random walk
mr <- dplyr::filter(df_sims_withfit, variable %in% c("biomass_rw_term2014",
                                                     "biomass_rw_term2015",
                                                     "biomass_rw_term2016",
                                                     "biomass_rw_term2017",
                                                     "biomass_rw_term2018"),
                    year >= 2014) %>%
  dplyr::group_by(rep, driver, scenario)

# get the tip and terminal assessment values for each year, rep, driver, and scenario
# I'm sure there are better ways to do this!
# note the years are hardwired for these calculations - should be generalized
for (iyear in 2014:2017){
  temp1 <- mr %>%
    dplyr::filter(variable == paste0("biomass_rw_term",iyear),
                  year == iyear) %>%
    dplyr::mutate(tip = value) %>%
    dplyr::select(-c(variable, value))
  
  temp2 <- mr %>%
    dplyr::filter(variable == "biomass_rw_term2018",
                  year == iyear) %>%
    dplyr::mutate(term = value) %>%
    dplyr::select(-c(variable, value))
  
  temp3 <- dplyr::inner_join(temp1, temp2) %>%
    dplyr::mutate(rho = (tip - term) / term)
  
  if (iyear == 2014){
    df_rho <- temp3
  }
  if (iyear >= 2015){
    df_rho <- rbind(df_rho, temp3)
  }
}

# calculate Mohn's rho using the four peels
df_mr <- 
  df_rho %>%
  dplyr::group_by(rep, driver, scenario) %>%
  dplyr::summarize(mohn_rho = mean(rho)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(driver = as.character(driver),
                scenario = as.character(scenario),
                driver = ifelse(driver == "f",
                                "Fishing driver",
                                driver),
                driver = ifelse(driver == "r",
                                "Recruitment driver",
                                driver),
                scenario = ifelse(scenario == "no change",
                                  "No change",
                                  scenario),
                scenario = ifelse(scenario == "increasing biomass",
                                  "Increasing biomass",
                                  scenario),
                scenario = ifelse(scenario == "decreasing biomass",
                                  "Decreasing biomass",
                                  scenario))

# make a somewhat pretty plot - I'm sure this can be improved as well
# bottom line is not much retrospective, but in the direction we discussed earlier
ggplot(df_mr, aes(x=scenario, y=mohn_rho, fill=driver)) +
  geom_violin() +
  xlab("Trend") +
  ylab("Mohn's rho") +
  guides(fill=guide_legend(title=NULL))

ggsave("fig_mohn.pdf", width = 6, height = 4)

# Calculate mean Mohn's rho for each scenario
df_mr_mean <-
  df_mr %>%
  dplyr::group_by(driver, scenario) %>%
  dplyr::summarise(mean_mohn = mean(mohn_rho))
