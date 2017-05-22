# convert_survey_kgtow_B
# convert GBYT survey data from kg/tow to population Biomass
# surveys are DFO, NEFSC Spring, and NEFSC Fall
# NEFSC data are in Albatross (1963-2008) and Bigelow (2009+) units
# 16 December 2016  Chris Legault
# last modified 27 January 2016 by Charles Perretti

# get survey data in kg/tow
data <- read.csv("data/GBYT_survey_data.csv")

# dfo <- data[data$Survey == "DFO", ]
# spring <- data[data$Survey == "Spring", ]
# fall <- data[data$Survey == "Fall", ]

# set up constants for conversions

# multiply Albatross by this factor to get Bigelow kg/tow
# differ by season based on Brooks et al. 2010 TRAC Ref Doc 2010/08
ALB2BIG.spring <- 2.244  
ALB2BIG.fall   <- 2.402  

# total area surveyed (square kilometers)
# differ by country due to different stratification and coverage of bank
total.area.dfo  <- 25453
total.area.nmfs <- 37286

# area swept by a single tow (square kilometers)
# doorspread used here, may be changed to wingspread at next TRAC)
# differ by country due to different nets used
tow.area.doors.dfo  <- 0.1214
tow.area.doors.nmfs <- 0.0606

# net catchability
# current value based on literature, may be changed to experiment estimate at next TRAC
catchability <- 0.37

# compute survey biomass (metric tons)
# B.dfo <- dfo[,3] * (total.area.dfo / (tow.area.doors.dfo * catchability)) / 1000
# 
# spring.BIG <- c(spring[spring$Year <= 2008, 3] * ALB2BIG.spring, spring[spring$Year >= 2009, 3])
# B.spring <- spring.BIG * (total.area.nmfs / (tow.area.doors.nmfs * catchability)) / 1000
# 
# fall.BIG <- c(fall[fall$Year <= 2008, 3] * ALB2BIG.fall, fall[fall$Year >= 2009, 3])
# B.fall <- fall.BIG * (total.area.nmfs / (tow.area.doors.nmfs * catchability)) / 1000

# create data frame for data
# note fall data from previous year
# start.year <- 1987
# end.year   <- 2016
# year2include <- seq(start.year, end.year)

# B <- data.frame("year"      = year2include,
#                 "DFO.B"     = B.dfo[dfo$Year %in% year2include],
#                 "Spring.B"  = B.spring[spring$Year %in% year2include],
#                 "Fall.B"    = B.fall[fall$Year %in% (year2include - 1)],
#                 "DFO.CV"    = dfo[dfo$Year %in% year2include, 4],
#                 "Spring.CV" = spring[spring$Year %in% year2include, 4],
#                 "Fall.CV"   = fall[fall$Year %in% (year2include - 1), 4])

#avg.B <- apply(B[,2:4], 1, mean) # average biomass from the three surveys

# make a plot
# plot(B$year, B$DFO.B, 
#      xlab="year", ylab="Biomass (mt)", 
#      ylim=c(0,max(B[,2:4])), type='l', col="red")
#  lines(B$year, B$Spring.B, col="dark green")
#  lines(B$year, B$Fall.B, col="blue")
#  lines(B$year, avg.B, col="black", lwd=2)
#  legend('topleft', 
#         legend=c("DFO","Spring","Fall","Average"), 
#         pch=NA, lty=1, lwd=c(1,1,1,2), 
#         col=c("red","dark green","blue","black"))


 # yt_data <- data.frame(YEAR = B$year, biomass = avg.B)
 # write.csv(yt_data, "../NEFMC_FEP/yt_data.csv")
 

