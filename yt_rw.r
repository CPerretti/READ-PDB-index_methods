# yt_rw
# state-space model for Georges Bank yellowtail flounder survey data
# want to compare population estimates with simple average and MARSS
# created 18 January 2017  Chris Legault
# last modified 19 January 2017

library(TMB)
library(Hmisc)

# compile cpp code and load dll
compile("yt_rw.cpp")
dyn.load(dynlib("yt_rw"))

# read data from file
input <- read.csv("yt.csv", header = TRUE)

log_obs_DFO <- log(input$DFO.B / 1000)  # log scale indices in 1000 mt
log_obs_spring <- log(input$Spring.B / 1000)
log_obs_fall <- log(input$Fall.B / 1000)
DFO_sd <- sqrt(log(input$DFO.CV^2 + 1)) # compute log scale survey sd from CV
spring_sd <- sqrt(log(input$Spring.CV^2 + 1))
fall_sd <- sqrt(log(input$Fall.CV^2 + 1))

n.obs <- length(log_obs_DFO) # assumes data in all years for all three surveys

# set up data and parameters
dat <- list(
  log_obs_DFO = log_obs_DFO,
  log_obs_spring = log_obs_spring,
  log_obs_fall = log_obs_fall,
  DFO_sd = DFO_sd,
  spring_sd = spring_sd,
  fall_sd = fall_sd
)

parameters <- list(
  logB = rep(0,n.obs),
  log_process_error = 0,
  log_obs_error_DFO = 0,
  log_obs_error_spring = 0,
  log_obs_error_fall = 0
)

# now estimate population, process error, and additional observation errors
obj <- MakeADFun(dat,
                 parameters,
                 DLL = "yt_rw", 
                 random = c("logB"), 
                 silent = TRUE)
opt <- nlminb(obj$par, 
              obj$fn, 
              obj$gr, 
              control = list(iter.max = 1000,
                             eval.max = 1000))
srep <- summary(sdreport(obj))
print(srep)

# get results in a format for plots
est.log.pop <- srep[rownames(srep) == "logB",]
est.process.error <- as.vector(round(srep[rownames(srep) == "process_error",],3))
est.obs.error.DFO <- as.vector(round(srep[rownames(srep) == "obs_error_DFO",],3))
est.obs.error.spring <- as.vector(round(srep[rownames(srep) == "obs_error_spring",],3))
est.obs.error.fall <- as.vector(round(srep[rownames(srep) == "obs_error_fall",],3))

TMB_fit_ev <- exp(est.log.pop[,1])
TMB_fit_hi <- exp(est.log.pop[,1] + 2*est.log.pop[,2])
TMB_fit_lo <- exp(est.log.pop[,1] - 2*est.log.pop[,2])
obs.DFO <- exp(log_obs_DFO)
obs.spring <- exp(log_obs_spring)
obs.fall <- exp(log_obs_fall)
avg.B <- (obs.DFO + obs.spring + obs.fall) / 3
my.range <- range(c(0, obs.DFO, obs.spring, obs.fall, hi, lo))

# make a pretty plot
plot(input$Year,obs.DFO,
     ylim = my.range,
     xlab = "Year of DFO and Spring Surveys",
     ylab = "Pop Biomass (1000 mt)", 
     pch = 0)
points(input$Year,obs.spring, pch=1)
points(input$Year,obs.fall, pch=2)
errbar(input$Year, ep, hi, lo, pch=3, col="red", errbar.col="red", add=TRUE)
lines(input$Year, avg.B, col = "blue")
legend('topleft', 
       legend = c("DFO", "Spring", "Fall", "est", "avg"),
       pch = c(0, 1, 2, 3, NA),
       lty = c(NA, NA, NA, NA, 1),
       col = c("black", "black", "black", "red", "blue"))
title(main = paste0("process error   = ",
                    est.process.error[1],
                    "\nadditional observation errors = ",
                    est.obs.error.DFO[1],
                    ", ",
                    est.obs.error.spring[1],
                    ", ",
                    est.obs.error.fall[1]), 
      outer=F)

cbind(input$Year, avg.B, ep)

