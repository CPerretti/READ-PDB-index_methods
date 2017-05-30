source("1-func.R")
# Load GB yellowtail survey data in kg/tow

data <- read.csv("data/GBYT_survey_data.csv")

#df_rfssb <- xlsx::read.xlsx("data/GBYTtimeseries.xlsx", 
#                            sheetName = "Sheet1")

df_rfssb <- read.csv("data/GBYTtimeseries.csv")
