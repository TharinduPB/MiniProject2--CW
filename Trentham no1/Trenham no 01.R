#this script uses the full functions of the level smooth package including the rate rise and anomal message

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)
library(base)

##################################################################################################
# Read raw data and remove NA and 0 values filter 2021

res_data <- read_csv("reslevels_raw.csv") %>% 
  select(timestamp = timestamp, level = level, reservoir = reservoir) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & reservoir == "Trentham No 1 Head Gauge" & 
                   between(as.Date(timestamp),
                           as.Date("2016-01-01"),
                           as.Date("2022-12-31")))

res_data_ori <- res_data

# Read verified data from WMIS 

verified <- read_csv("406282.csv")  %>% 
  select(timestamp = Datetime , 
         verified_level = `water Level` ,
         qc = QC ) %>% 
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_filtered <- verified %>% 
  arrange(timestamp) %>% 
  dplyr::filter( between(as.Date(timestamp), as.Date("2016-01-01"), 
                         as.Date("2022-12-31")))

#merge res_data and verified_data

res_data_merge <- merge(res_data,verified_filtered,by = "timestamp")


## remove physical, MAD, savitzky golay to remove anomalies ##

#inputs : Full supply level and Min level
res_data_1 <- phycon(res_data_merge,735,729)

#inputs : limit for MAD 
res_data_2 <- mad_outliers(res_data_1, 3.5)

#inputs : binomial number, number of points considered, limit to cut off values
res_data_3 <- sgfilter(res_data_1,0,105,0.03)


## use the rate rise functions to find anomalies ##

#inputs : limit for rate of rise, number-do not change
res_data_4 <- raterise(res_data_ori, 0.01, 100)

#inputs : number of points in which the value will go down, deduction rate
res_data_5 <- anomal_message(res_data_4,500,0.02)

#check the RMSE 

squared_diff <- (res_data_3$level - res_data_3$verified_level)^2
mean_squared_diff <- mean(squared_diff)
rmse <- sqrt(mean_squared_diff)

#visualize the data

p1 <- plot_ly() %>%
  add_trace(data=res_data_5, x=~timestamp, y=~thresh, mode = "lines",
            type = "scatter", name = "rate of rise", line = list(width=0.5))

p2 <- plot_ly() %>% 
  add_trace(data=res_data_ori, x=~timestamp, y=~level, mode="lines",
            type = "scatter", name = "original Data", line= list(width=0.5)) %>%
  add_trace(data=verified_filtered, x=~timestamp, y=~verified_level, mode="lines",
            type="scatter", name ="WMIS Data", line=list(width=0.4)) %>%
  add_trace(data=res_data_3, x=~timestamp, y=~level, mode = "lines",
            type = "scatter", name = "Smoothed Data", line = list(width = 0.6))


subplot(p1,p2, nrows = 2)
