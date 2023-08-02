# the purpose of this script is to index data points with spikes (or dh/dt)

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelSmoothing)
library(ggplot2)

####################################################################################################
# Data set 2021 #

# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>%
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




# using the rate of rise to find the anomalous points


malmsbury_rate_rise <- malmsbury_21


malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate(  dh = dh <- abs(malmsbury_rate_rise$level - lag(malmsbury_rate_rise$level)),
           dt = dt <- malmsbury_rate_rise$timestamp - lag(malmsbury_rate_rise$timestamp),
           rate = rate <- dh / as.numeric(dt),
           )

# find points where the rise is high 

malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate (rate_rise = rate_rise <- rate >= 0.0023)



ggplot(malmsbury_rate_rise, aes(timestamp, level)) + 
  ylim(439,450) +
  geom_line() + 
  geom_point(data = malmsbury_rate_rise, aes(timestamp, level, col = rate_rise),
             size = 0.1) 


#plot rate of rise with timestamp

ggplot(malmsbury_rate_rise, aes(timestamp, rate))





