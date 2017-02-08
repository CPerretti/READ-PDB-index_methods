# Use MARSS model to take several indices and estimate a
# single underlying trend.

# Advantages:
# 1) Handles seasonality
# 2) Incorporates multiple time series
# 3) Estimates process and observation error

library(magrittr)
library(ggplot2)
library(MARSS)

source("1-func.R")

set.seed(1)

N <- 60 # timeseries length
n <- vector(mode = "numeric")
n[1] <- 1000 # initial condition
u <- 0 # No trend

# Set the underlying population state (Gompertz model)
for(t in 2:N) {
  n[t] <-
    exp(u + rnorm(n = 1, mean = 0, sd = 0.5)) * n[t-1]
}

True_state <- log(n)

# Add seasonality and observation error
Index <- matrix(data = NA, nrow = 3, ncol = N)
seas <- rep(c(0, -1), length.out = N)
for(t in 1:N) {
  Index[,t] <- 
    True_state[t] +
    c(0, 0, 0) + #bias (A)
    seas[t] + #seasonality (D)
    MASS::mvrnorm(n = 1,   
                  mu = c(0, 0, 0), 
                  Sigma = matrix(data = c(.5,0,0,
                                          0,.5,0,
                                          0,0,.5), 
                                 nrow = 3, 
                                 ncol = 3))
}

## Plot un-normalized indices -----------------------------
dat2plot <-
  data.frame(YEAR = c(1:N)*0.5, 
             True_state = True_state, 
             Index = Index %>% t) %>%
  tidyr::gather(variable, value, -YEAR)

ggplot(dat2plot, 
       aes(x = YEAR, 
           y = value,
           color = variable)) +
  geom_line(data = dat2plot %>% 
              dplyr::filter(variable %in% c("Index.1",
                                            "Index.2",
                                            "Index.3")),
            alpha = 0.7) +
  geom_line(data = dat2plot %>% 
              dplyr::filter(variable %in% c("MARSS fit",
                                            "True_state")),
            size = 0.8) +
  xlab("Year") +
  ylab("Index value") +
  theme(legend.title = element_blank())


## Extract data to MARSS ----------------------------------
df <-
  data.frame(YEAR = c(1:N)*0.5, 
             True_state = True_state, 
             Index = Index %>% t) %>%
  tidyr::gather(variable, value, -YEAR) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(value_z = trawlr::calc_z(value)) %>%
  dplyr::select(-value)

dat2marss <-
  df %>%
  tidyr::spread(variable, value_z) %>%
  dplyr::select(-YEAR, -True_state) %>%
  t %>%
  as.matrix

# Need to incorporate seasonality into observation model
# This will be done through the D parameter.

# First set up an indicator variable which describes the
# season we are in.

# number of "seasons" (e.g., 12 months per year)
n_seas <- 2
# first "season" (e.g., Spring = 1, Fall = 2)
seas_first <- 1
# create factors for seasons
d_in <- diag(n_seas)
for(i in 2:(ceiling(N/n_seas))) {
  d_in <- cbind(d_in, diag(n_seas))
}

# trim d_in to correct start & length
d_in <- d_in[, (1:N) + (seas_first - 1)]
# better row names
rownames(d_in) <- c("Spring", "Fall")

# Now name the season parameters to fit
D <- matrix(c("Spring", "Fall"),
            nrow = 3,
            ncol = n_seas,
            byrow = TRUE)

## Fit model ----------------------------------------------
# Set up model parameters
mod.list.0 <- 
  list(B = matrix(1),
       U = matrix(0),
       Q = matrix("q"),
       Z = matrix(c(1, 1, 1), 3, 1),
       A = matrix(c(0, 0, 0), 3, 1),
       R = "diagonal and equal",
       # Allow for different seasonality across indices
       D = D, 
       d = d_in,
       x0 = matrix("mu"))

fit.0 <- MARSS(dat2marss, 
               model = mod.list.0)


## Line plot of true, fit, and the three indices ----------
df_wfit <-
  data.frame(YEAR = df$YEAR %>% unique, 
             variable = "MARSS fit", 
             value_z = (fit.0$states %>% t)) %>%
  dplyr::rename(value_z = X1) %>%
  dplyr::bind_rows(df, .)

ggplot(df_wfit, 
       aes(x = YEAR, 
           y = value_z,
           color = variable)) +
  geom_line(data = df_wfit %>% 
                    dplyr::filter(variable %in% c("Index.1",
                                                  "Index.2",
                                                  "Index.3")),
            alpha = 0.7) +
  geom_line(data = df_wfit %>% 
              dplyr::filter(variable %in% c("MARSS fit",
                                            "True_state")),
            size = 0.8) +
  xlab("Year") +
  ylab("Index value") +
  theme(legend.title = element_blank()) #+ 
  # scale_colour_manual(values=c("grey40", "grey50", "grey60", 
  #                              "blue", "black"))


## Scatter plots of true vs fit and the three indices -----
df2scatter <-
  df_wfit %>%
  tidyr::spread(variable, value_z) %>%
  tidyr::gather(variable, value_z, -YEAR, -True_state)

cor2plot <- 
  df2scatter %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(cor = cor(True_state, value_z))


ggplot(df2scatter,
       aes(x = True_state, 
           y = value_z, 
           color = variable)) +
  geom_point() +
  facet_wrap(~variable) +
  geom_text(data = cor2plot, 
            aes(x = -1, y = 1.5, 
                label = paste0("cor = ",
                               round(cor,2))),
            colour = "black") +
  ylab("Index value") +
  xlab("True state") +
  theme(legend.position = "none")

  