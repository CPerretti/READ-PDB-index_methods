# Clean data
source("2-load.R")

# Re-arrange yellowtail data for model fitting
df_yt <- 
  data %>%
  # Expand biomass per tow to total biomass adjusted for catchability
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
                                   Year <= 2008,
                                 B.kg.tow. * ALB2BIG.spring * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass),
                # Expand fall albatross
                biomass = ifelse(Survey == "Fall" &
                                   Year <= 2008,
                                 B.kg.tow. * ALB2BIG.fall * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass),
                # Expand fall and spring bigelow
                biomass = ifelse(Survey %in% c("Spring", "Fall") &
                                   Year >= 2009,
                                 B.kg.tow. * 
                                   (total.area.nmfs / 
                                      (tow.area.doors.nmfs * 
                                         catchability)) / 1000,
                                 biomass)) %>%
  # Lag fall survey biomass
  dplyr::mutate(Year = ifelse(Survey == "Fall",
                              Year + 1,
                              Year)) %>%
  # Use start year and end year from convert_survey_kgtow_B.r
  dplyr::filter(Year >= 1987,
                Year <= 2016) %>%
  # Calculate survey average
  dplyr::bind_rows({dplyr::group_by(., Year) %>%
      dplyr::summarise(biomass = mean(biomass)) %>%
      data.frame(., Survey = "Average")}) %>%
  # Scale down biomass 
  dplyr::mutate(biomass = biomass/1000) %>%
  # Calculate log for TMB model fitting
  dplyr::mutate(log_biomass = log(biomass))



