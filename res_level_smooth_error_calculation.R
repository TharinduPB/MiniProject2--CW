#finding the error of the smoothed values vs the WIMS data using RMSE

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelSmoothing)

####################################################################################################
# Data set 2021 #

# Read raw data and remove NA and 0 values filter 2021

res_2021 <- read_csv("rawdata.csv") %>%
  select(timestamp = Timestamp, level = Level1) %>%
  arrange(timestamp) %>%
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"),
                                                     as.Date("2021-12-31")))


# Read verified data
verified <- read_csv("406220.csv")  %>%
  select(timestamp = Datetime ,
         level = `Water Level` ,
         qc = QC ) %>%
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_2021 <- verified %>%
  arrange(timestamp) %>%
  dplyr::filter( between(as.Date(timestamp), as.Date("2021-01-01"),
                         as.Date("2021-12-31")))



#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_2021_1 <- phycon(res_2021,447.8,435.0,3.5)

res_2021_2 <- sgfilter(res_2021_1,6,99,0.03)

res_2021_3 <- raterise(res_2021_2,0.0023,288)




full_supply_level <- 447.8
min_level <- 435.0


# finding the anomalies and adding new columns 

malmsbury_21 <- malmsbury_21 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         MAD = abs(level - median(level)) / mad(level) >= 3.5
  )

malmsbury_21 <- malmsbury_21 %>%
  dplyr::filter (!FSL, !min, !MAD)

malmsbury_play <- malmsbury_21

#use ewma to further identify outliers

# smoothing with Exponentially Weighted Moving Average #

# using the zoo package to check EWMA #

malm_level <- malmsbury_play$level
malm_time <- malmsbury_play$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_cleaned<- malmsbury_play %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 288))



# use Savitzky-Golay method to draw a smoothed graph

smoothed <- sgolayfilt(malmsbury_21$level, p = 6, n = 99)
smoothed1 <- sgolayfilt(malmsbury_21$level, p = 4, n = 99)

#using Savitzky-golay to find outliers and remove them,

malmsbury_cleaned <- malmsbury_cleaned %>%
  mutate( sg_smooth = sg_smooth <- sgolayfilt(malmsbury_cleaned$level, p = 4, n = 99),
          sg_diff = sg_diff <- abs(sg_smooth - level),
          sg_diff_con = sg_diff >= 0.03)

malmsbury_cleaned1 <- malmsbury_cleaned %>%
  dplyr::filter(!sg_diff_con)





