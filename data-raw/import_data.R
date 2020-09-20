### Data_Importing#######################################

library(Rssa)
library(lubridate)
library(tidyverse)
library(usethis)


#########################################################
# Generate Dataframes from CSV Files
#   -parkdata.csv : Park-specific PUD data
#   -flickr_popularity_forkimi.csv : global flickr popularity
#   -flickr_popularity_US.csv : US flickr popularity#
#########################################################

# Load Data from .csv files

parkdata <- read_csv("data-raw/parkdata.csv")
global_flickr_ud <- read_csv("data-raw/flickr_popularity_forkimi.csv")
us_flickr_ud <- read_csv("data-raw/flickr_popularity_US.csv")

# Park Specific PUD Data ------------------------

n <- length(parkdata[parkdata$park == "YELL",]$pud)

#Adding a single date variable
parkdata <- parkdata %>%
  mutate(
    date = make_date(year = year, month = monthnum)
  ) %>%
  arrange(park,date)


# Global flickr popularity ------------------------------

#Extracting Users Trend using SSA
flickr_global_userdays <- ts(global_flickr_ud$userdays,start = 2005, freq = 12)
flickr_us_userdays <- ts(us_flickr_ud$userdays, start = 2005, freq = 12)


## Creating Popular Park Dataset ------------------------


popular_parks <- c("GRSM","GRCA","YOSE","YELL","ROMO","DEVA","GRTE", "ZION",
                   "ARCH", "JOTR", "BRCA", "OLYM", "MORA", "EVER", "HAVO",
                   "GLAC", "HALE", "ACAD", "BIBE", "BADL")

parkdata_popular <- parkdata[parkdata$park %in% popular_parks,]

#Modify "BADL" to have strictly positive PUD counts (for log transformation)
parkdata_popular[parkdata_popular$park == "BADL" & parkdata_popular$pud == 0,]$pud <- 1
parkdata_popular[parkdata_popular$park == "BADL",]$pud

parkdata_popular <- parkdata_popular %>% select(date,park,pud)

#### Saving Datasets used in Package -------------------

flickr_userdays <- window(flickr_us_userdays, end = time(flickr_us_userdays)[156])
park_visitation <- parkdata_popular

# Save Data as .rda files.
#use_data(flickr_userdays, overwrite = TRUE)
#use_data(park_visitation)


