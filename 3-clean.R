source("2-load.R")
# Clean data

## Set up constants for conversions -----------------------

# multiply Albatross by this factor to get Bigelow kg/tow
# differ by season based on Brooks et al. 
# 2010 TRAC Ref Doc 2010/08
ALB2BIG.spring <- 2.244  
ALB2BIG.fall   <- 2.402  

# total area surveyed (square kilometers)
# differ by country due to different stratification 
# and coverage of bank
total.area.dfo  <- 25453
total.area.nmfs <- 37286

# area swept by a single tow (square kilometers)
# doorspread used here, may be changed to wingspread at next TRAC)
# differ by country due to different nets used
tow.area.doors.dfo  <- 0.1214
tow.area.doors.nmfs <- 0.0606

# net catchability
# current value based on literature, may be changed to 
# experiment estimate at next TRAC
catchability <- 0.37

## Setup data ---------------------------------------------
df_real <- 
  data %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(Survey = as.character(Survey)) %>%
  # Expand biomass per tow to total biomass adjusted 
  # for catchability
  dplyr::mutate(biomass = NA,
                # Expand DFO
                biomass = ifelse(Survey == "DFO",
                                 B.kg.tow. * 
                                   (total.area.dfo / 
                                      (tow.area.doors.dfo * 
                                         catchability)) / 
                                   1000,
                                 biomass),
                # Expand spring albatross
                biomass = ifelse(Survey == "Spring" &
                                   year <= 2008,
                                 B.kg.tow. * ALB2BIG.spring * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass),
                # Expand fall albatross
                biomass = ifelse(Survey == "Fall" &
                                   year <= 2008,
                                 B.kg.tow. * ALB2BIG.fall * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass),
                # Expand fall and spring bigelow
                biomass = ifelse(Survey %in% c("Spring", "Fall") &
                                   year >= 2009,
                                 B.kg.tow. * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass)) %>%
  # Lag fall survey biomass
  dplyr::mutate(year = ifelse(Survey == "Fall",
                              year + 1,
                              year)) %>%
  # Use start year and end year from convert_survey_kgtow_B.r
  dplyr::filter(year >= 1987,
                year <= 2016) %>%
  # Calculate survey average
  dplyr::bind_rows({dplyr::group_by(., year) %>%
                    dplyr::summarise(biomass = mean(biomass)) %>%
                    data.frame(., Survey = "Average")}) %>%
  # Scale down biomass 
  dplyr::mutate(biomass = biomass/1000) %>%
  # Calculate log for TMB model fitting
  dplyr::mutate(log_biomass = log(biomass))



