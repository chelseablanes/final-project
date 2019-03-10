library("dplyr")
library("tidyr")
library("shiny")

# load unemployment csv
unemployment <- read.csv('data/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_10473697.csv', 
                         stringsAsFactors = FALSE)
unemployment <- unemployment %>% 
  select(Country.Name, Country.Code, X2016)
View(unemployment)

# load happiness csv
happiness <- read.csv('data/2016.csv', stringsAsFactors = FALSE)
