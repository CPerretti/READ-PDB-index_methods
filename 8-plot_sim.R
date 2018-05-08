source("7-plot_real.R")
# Make plots of simulation results



## Plot simulated and fit biomass ---------------------------------
# Setup data for plots
oldvar_newvar <-
  data.frame(variable = c("biomass_obs.survey1",
                          "biomass_obs.survey2",
                          "biomass_obs.survey3",
                          "biomass_average_3surv_term",
                          "biomass_rw_3surv_term",
                          "biomass_tru",
                          "biomass_rw_3surv_term_se"),
             variable_new = c("Survey 1",
                              "Survey 2",
                              "Survey 3",
                              "Empirical fit",
                              "State-space fit",
                              "True",
                              "biomass_rw_se")) %>%
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
         rep2p = 123,
         driver2p = "r",
         scen2p = "no change",
         df_sims_withfit = df_sims_withfit1,
         titl = "Simulated no change scenario",
         filename = "fig_ts_nochange.pdf")

# 2. Increasing by recruitment slowly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "r",
         scen2p = "increasing slowly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Slowly increasing by recruitment",
         filename = "fig_ts_incslowbyr.pdf")

# 3. Increasing by recruitment rapidly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "r",
         scen2p = "increasing rapidly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Rapidly increasing by recruitment",
         filename = "fig_ts_incrapidbyr.pdf")

# 4. Decreasing by recruitment slowly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "f",
         scen2p = "decreasing slowly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Slowly decreasing by recruitment",
         filename = "fig_ts_decslowbyr.pdf")

# 5. Decreasing by recruitment rapidly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "r",
         scen2p = "decreasing rapidly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Rapidly decreasing by recruitment",
         filename = "fig_ts_decrapidbyr.pdf")

# 6. Increasing by fishing slowly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "f",
         scen2p = "increasing slowly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Slowly increasing by fishing",
         filename = "fig_ts_incslowbyf.pdf")

# 7. Increasing by fishing rapidly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "f",
         scen2p = "increasing rapidly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Rapidly increasing by fishing",
         filename = "fig_ts_incrapidbyf.pdf")

# 8. Decreasing by fishing slowly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "f",
         scen2p = "decreasing slowly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Slowly decreasing by fishing",
         filename = "fig_ts_decslowbyf.pdf")

# 9. Decreasing by fishing rapidly
plot_fit(df_sims = df_sims,
         rep2p = 123,
         driver2p = "f",
         scen2p = "decreasing rapidly",
         df_sims_withfit = df_sims_withfit1,
         titl = "Rapidly decreasing by fishing",
         filename = "fig_ts_decrapidbyf.pdf")


## Plot a blow up the last 10 years with mean trajectories
df2plot <-
  df_sims %>%
  dplyr::filter(year >= max(year) - n_scenario + 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(driver, scenario, year, 
                biomass_obs.survey1, biomass_obs.survey2, 
                biomass_obs.survey3) %>%
  tidyr::gather(variable, biomass_obs, -year, -scenario, -driver) %>%
  dplyr::group_by(driver, scenario, year) %>%
  dplyr::summarise(median_obs = median(biomass_obs),
                   simint_high = quantile(biomass_obs, probs = .95),
                   simint_low  = quantile(biomass_obs, probs = .05)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(driver = as.character(driver),
                scenario = as.character(scenario),
                driver = ifelse(driver == "f",
                                "Fishing",
                                driver),
                driver = ifelse(driver == "r",
                                "Recruitment",
                                driver),
                scenario = ifelse(scenario == "no change",
                                  "No change",
                                  scenario),
                scenario = ifelse(scenario == "increasing rapidly",
                                  "Increasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing rapidly",
                                  "Decreasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "increasing slowly",
                                  "Increasing slowly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing slowly",
                                  "Decreasing slowly",
                                  scenario),
                scenario = factor(scenario, 
                                  levels = c("Decreasing rapidly",
                                             "Decreasing slowly",
                                             "No change",
                                             "Increasing slowly",
                                             "Increasing rapidly"))) %>%
  dplyr::rename(Scenario = scenario,
                Driver = driver)

p_scenarios <-
  ggplot(df2plot, aes(x = year, 
                    y = median_obs)) +
  geom_line() +
  geom_ribbon(aes(ymin = simint_low,
                  ymax = simint_high), alpha = 0.3) +
  ylim(0, NA) +
  xlab("Year") +
  ylab("Biomass (1000 mt)") +
  facet_grid(Driver ~ Scenario) +
  theme(axis.text = element_text(size = 7))

p_scenarios


ggsave("fig_scenarios.pdf", width = 9, height = 5)
  

## Plot simulated age structure in first year---------------------
df2plot <- 
  df_sims %>%
  dplyr::filter(year == 1973) %>%
  tidyr::gather(variable, value, 
                -year, -rep, -driver, -scenario) %>%
  dplyr::filter(variable %in% c("abund_tru.age1",
                                "abund_tru.age2",
                                "abund_tru.age3",
                                "abund_tru.age4",
                                "abund_tru.age5",
                                "abund_tru.age6",
                                "abund_tru.age7",
                                "abund_tru.age8",
                                "abund_tru.age9",
                                "abund_tru.age10")) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(mean_abund = mean(value)) %>%
  dplyr::mutate(age = substr(variable, start = 14, 15) %>%
                  as.numeric)
p_age <- 
  ggplot(df2plot,
         aes(x = age, y = mean_abund)) +
    geom_line() +
    geom_point(size = 0.3) +
    xlab("Age") +
    ylab("Abundance") +
    theme(legend.title = element_blank())

p_age

# Calculate % diff between VPA and simulated for 1973
df2plot <-
  df2plot %>%
  dplyr::arrange(age) %>%
  dplyr::mutate(mean_abund_tho = mean_abund/1000)
df2plot$mean_abund_tho[6] <- sum(df2plot$mean_abund_tho[6:10])
(df2plot$mean_abund_tho[1:6]  - c(46684, 34147, 37735, 21482, 8650, 3741)) / 
  df2plot$mean_abund_tho[1:6]

## Plot error comparison ----------------------------------
p_err <-
  ggplot(df_errors %>% dplyr::filter(`number of surveys` == "3surv"), 
         aes(x = method, y = mae, fill = smooth, group = smooth)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mae - mae_ci, ymax = mae + mae_ci),
                width = .2,                 
                position = position_dodge(.9)) +
  facet_grid(driver~scenario) +
  ylab("Estimation error (MAE)") +
  xlab("Model") +
  guides(fill = guide_legend(title = NULL))

p_err

ggsave("fig_err_mae.pdf", width = 10, height = 6.5)


## Plot coverage deciles for rw in 2016 -------------------
df_coverage_decile1 <-
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
                scenario = ifelse(scenario == "increasing rapidly",
                                  "Increasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing rapidly",
                                  "Decreasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "increasing slowly",
                                  "Increasing slowly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing slowly",
                                  "Decreasing slowly",
                                  scenario),
                scenario = factor(scenario, 
                                  levels = c("Decreasing rapidly",
                                             "Decreasing slowly",
                                             "No change",
                                             "Increasing slowly",
                                             "Increasing rapidly")))


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
# df_fitvsobs <-
#   df_sims_withfit %>%
#   dplyr::filter(year > (terminal_year - n_scenario),
#                 variable %in% c("biomass_tru",
#                                 "biomass_rw",
#                                 "biomass_rw_3ymean",
#                                 "biomass_average",
#                                 "biomass_average_3ymean"
#                                 )) %>%
#   tidyr::spread(variable, value) %>%
#   tidyr::gather(method, value, 
#                 -rep, -driver, -scenario, -year, -biomass_tru)
# 
# # Just rw
# ggplot(df_fitvsobs %>%
#          dplyr::filter(method %in% c("biomass_tru",
#                                      "biomass_rw")),
#        aes(x = biomass_tru, y = value)) +
#   geom_point(alpha = 0.5, size = 0.5) +
#   geom_abline() +
#   facet_grid(driver~scenario, scales = "free") +
#   xlab("True state in terminal year") +
#   ylab("Estimated state in terminal year")
# 
# # All methods
# ggplot(df_fitvsobs,
#        aes(x = biomass_tru, y = value)) +
#   geom_point(alpha = 0.2, size = 0.5) +
#   geom_abline() +
#   facet_grid(method~driver*scenario, scales = "free") +
#   xlab("True state in terminal year") +
#   ylab("Estimated state in terminal year")

## Calculate Mohn's rho for random walk estimates --------

# collect biomass estiamtes from random walk
mr <- dplyr::filter(df_sims_withfit, variable %in% c("biomass_rw_3surv_term2014",
                                                     "biomass_rw_3surv_term2015",
                                                     "biomass_rw_3surv_term2016",
                                                     "biomass_rw_3surv_term2017",
                                                     "biomass_rw_3surv_term2018"),
                    year >= 2014) %>%
  dplyr::group_by(rep, driver, scenario)

# get the tip and terminal assessment values for each year, rep, driver, and scenario
# I'm sure there are better ways to do this!
# note the years are hardwired for these calculations - should be generalized
for (iyear in 2014:2017){
  temp1 <- mr %>%
    dplyr::filter(variable == paste0("biomass_rw_3surv_term",iyear),
                  year == iyear) %>%
    dplyr::mutate(tip = value) %>%
    dplyr::select(-c(variable, value))
  
  temp2 <- mr %>%
    dplyr::filter(variable == "biomass_rw_3surv_term2018",
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
                scenario = ifelse(scenario == "increasing rapidly",
                                  "Increasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing rapidly",
                                  "Decreasing rapidly",
                                  scenario),
                scenario = ifelse(scenario == "increasing slowly",
                                  "Increasing slowly",
                                  scenario),
                scenario = ifelse(scenario == "decreasing slowly",
                                  "Decreasing slowly",
                                  scenario),
                scenario = factor(scenario, 
                                  levels = c("Decreasing rapidly",
                                             "Decreasing slowly",
                                             "No change",
                                             "Increasing slowly",
                                             "Increasing rapidly")))

# make a somewhat pretty plot - I'm sure this can be improved as well
# bottom line is not much retrospective, but in the direction we discussed earlier
ggplot(df_mr, aes(x = scenario, y = mohn_rho, fill = driver)) +
  geom_boxplot() +
  xlab("Trend") +
  ylab("Mohn's rho") +
  guides(fill=guide_legend(title=NULL))

ggsave("fig_mohn.pdf", width = 8.5, height = 4.5)

# Calculate mean Mohn's rho for each scenario
df_mr_mean <-
  df_mr %>%
  dplyr::group_by(driver, scenario) %>%
  dplyr::summarise(mean_mohn = mean(mohn_rho))


## Plot error vs three year change in biomass (all reps combined)-----
df2plot <-
  df_error_by_rep %>%
  dplyr::mutate(round_3ypc = round(biomass_3ypc, -1)) %>%
  dplyr::group_by(method, 
                  smooth, 
                  `number of surveys`,
                  round_3ypc) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 30) %>%
  dplyr::summarise(mean_ae = mean(abs_err),
                   se_ae   = sd(abs_err)/sqrt(n()),
                   se_high = mean_ae + se_ae,
                   se_low  = mean_ae - se_ae) %>%
  dplyr::filter(`number of surveys` == "3surv")

ggplot(df2plot, 
       aes(x = round_3ypc, y = mean_ae, 
           color = paste(method, smooth))) +
  geom_line() +
  geom_ribbon(aes(ymin = se_low, 
                  ymax = se_high,
                  fill = paste(method, smooth)),
              alpha = 0.3,
              size = 0.1) +
  xlab("Three year change in biomass (%)") +
  ylab("Absolute error") +
  theme(legend.title = element_blank()) 

ggsave("fig_changeVSerror_allruns.pdf", width = 8.5, height = 5.5)

## Plot difference in error for term vs 3yr against % change -----
df2plot <-
  df_error_by_rep %>%
  dplyr::mutate(round_3ypc = round(biomass_3ypc, -1)) %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::select(-value, -biomass_3ypc, -biomass_tru) %>%
  tidyr::spread(smooth, abs_err) %>%
  dplyr::mutate(abs_err_diff = `Smoothed estimate` - `Unsmoothed estimate`) %>%
  dplyr::group_by(method, 
                  scenario,
                  driver,
                  `number of surveys`,
                  round_3ypc) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 30) %>%
  dplyr::summarise(mean_ae_diff = mean(abs_err_diff),
                   se_ae_diff = sd(abs_err_diff)/sqrt(n()),
                   se_high = mean_ae_diff + se_ae_diff,
                   se_low  = mean_ae_diff - se_ae_diff,
                   prop    = n()/(n_rep*n_scenario))

ggplot(df2plot, 
       aes(x = round_3ypc, y = mean_ae_diff)) +
  geom_line(aes(color = method)) +
  #geom_line(aes(y = prop, color = prop), size = 3) +
  geom_ribbon(aes(ymin = se_low, 
                  ymax = se_high,
                  fill = method),
              alpha = 0.6,
              size = 0.1) +
  facet_grid(driver~scenario, scales = "free") +
  geom_hline(yintercept = 0, size = 0.3) +
  theme(legend.title = element_blank()) +
  xlab("Three year change in biomass (%)") +
  ylab("Smoothed estimate error - Unsmoothed estimate error")

ggsave("fig_changeVSerror_byscenario.pdf", width = 8.5, height = 5.5)

## Difference in error vs number of surveys ---------------
df2plot <-
  df_error_by_rep %>%
  dplyr::select(-value) %>%
  tidyr::spread(smooth, abs_err) %>%
  dplyr::mutate(abs_err_diff = `Smoothed estimate` - `Unsmoothed estimate`) %>%
  dplyr::group_by(driver, scenario, method, `number of surveys`) %>%
  dplyr::summarise(ae_diff_mean = mean(abs_err_diff),
                   ae_diff_se = sd(abs_err_diff)/length(abs_err_diff)^0.5,
                   ae_diff_ci = 1.96 * ae_diff_se) %>%
  dplyr::mutate(`number of surveys` = ifelse(`number of surveys` == "1surv",
                                             "1",
                                             `number of surveys`),
                `number of surveys` = ifelse(`number of surveys` == "2surv",
                                             "2",
                                             `number of surveys`),
                `number of surveys` = ifelse(`number of surveys` == "3surv",
                                             "3",
                                             `number of surveys`))

ggplot(df2plot,
       aes(x = `number of surveys`, y = ae_diff_mean, fill = method)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = ae_diff_mean - ae_diff_ci, 
                    ymax = ae_diff_mean + ae_diff_ci),
                width = .2,                 
                position = position_dodge(.9)) +
  facet_grid(driver~scenario) +
  ylab("Smoothed estimate error - Unsmoothed estimate error") +
  xlab("Number of surveys") +
  guides(fill = guide_legend(title = NULL))


ggsave("fig_numsurveyVSerror.pdf", width = 8.5, height = 5.5)


## Make tables of MAE and RMSE for each method
# MAE Table

df_errors %>% 
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::ungroup() %>%
  dplyr::select(scenario, driver, method, smooth, mae) %>%
  dplyr::mutate(method_smooth = paste(method, smooth)) %>%
  dplyr::select(scenario, driver, method_smooth, mae) %>%
  tidyr::spread(method_smooth, mae) %>%
  dplyr::select(scenario, driver,
                `Empirical Smoothed estimate`, `State-space Smoothed estimate`,
                `Empirical Unsmoothed estimate`, `State-space Unsmoothed estimate`)

# Comparison of smooth error vs model error

# model error
df_errors %>% 
  dplyr::ungroup() %>%
  dplyr::select(scenario, driver, method, smooth, mae) %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(mean_mae = mean(mae)) %>%
  tidyr::spread(method, mean_mae) %>%
  dplyr::mutate(pc_diff = (Empirical - `State-space`)/Empirical)
  
  
#smooth error
df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  dplyr::filter(method == "State-space") %>%
  dplyr::ungroup() %>%
  dplyr::select(scenario, driver, method, smooth, mae) %>%
  dplyr::group_by(scenario, driver, smooth) %>%
  dplyr::summarise(mean_mae = mean(mae)) %>%
  tidyr::spread(smooth, mean_mae) %>%
  dplyr::mutate(pc_diff = (`Smoothed estimate` - `Unsmoothed estimate`)/`Smoothed estimate`)


# PC diff between smooths within models
df_errors %>%
  dplyr::filter(`number of surveys` == "3surv") %>%
  #dplyr::filter(method == "State-space") %>%
  dplyr::filter(method == "Empirical") %>%
  dplyr::ungroup() %>%
  dplyr::select(scenario, driver, method, smooth, mae) %>%
  dplyr::group_by(scenario, driver, method, smooth) %>%
  dplyr::summarise(mae = mean(mae)) %>%
  tidyr::spread(smooth, mae) %>%
  dplyr::mutate(pc_diff = (`Smoothed estimate` - `Unsmoothed estimate`)/`Smoothed estimate`)



