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


#merge the two data frames malmsbury_21 and verified_2021

malmsbury_21_merge <- merge(malmsbury_21,verified_2021,by = "timestamp")

#adjust the column names 

colnames(malmsbury_21_merge) <- c("timestamp", "level", "verified_level", "qc" )


#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_2021_1 <- phycon(malmsbury_21_merge,447.8,435.0,3.5)

res_2021_2 <- sgfilter(res_2021_1,0,105,0.03)

res_2021_3 <- raterise(res_2021_2,0.0023,288)


res_2021_check <- res_2021_2[1:3]


squared_diff <- (res_2021_check$level - res_2021_check$verified_level)^2
mean_squared_diff <- mean(squared_diff)
rmse <- sqrt(mean_squared_diff)


res_2021_check_1 <- res_2021_3[1:3]

squared_diff_1 <- (res_2021_check_1$level - res_2021_check_1$verified_level)^2
mean_squared_diff_1 <- mean(squared_diff)
rmse_1 <- sqrt(mean_squared_diff)


#plot the comparison

plot_ly() %>%
  # add_trace(x=malmsbury_21$timestamp, y=malmsbury_21$level, type = "scatter",
  #           mode="lines", name="original", size = 0.1,
  #           line=(list(width=1)))%>%
  add_trace(x=verified_2021$timestamp, y=verified_2021$level, type="scatter",
            mode= "lines", name="verified",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_2021_2$timestamp, y=res_2021_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", size=0.8,
            line=(list(width = 1))) #%>%
  # add_trace(x= res_2021_3$timestamp, y=res_2021_3$level, type = "scatter",
  #           mode= "lines", name = "rate of rise", size = 0.1,
  #           line=(list(width=1)))


#############################################################################################################################
#data may 2021

# Read raw data and remove NA and 0 values filter 2021

malmsbury_may <- read_csv("rawdata.csv") %>%
  select(timestamp = Timestamp, level = Level1) %>%
  arrange(timestamp) %>%
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2017-01-01"),
                                                     as.Date("2023-01-31")))


# Read verified data
verified <- read_csv("406220.csv")  %>%
  select(timestamp = Datetime ,
         level = `Water Level` ,
         qc = QC ) %>%
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_may <- verified %>%
  arrange(timestamp) %>%
  dplyr::filter( between(as.Date(timestamp), as.Date("2017-01-01"),
                         as.Date("2023-01-31")))


#merge the two data frames malmsbury_21 and verified_2021

malmsbury_may21_merge <- merge(malmsbury_may,verified_may,by = "timestamp")

#adjust the column names 

colnames(malmsbury_may21_merge) <- c("timestamp", "level", "verified_level", "qc" )


#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_may_1 <- phycon(malmsbury_may21_merge,447.8,435.0,3.5)

res_may_2 <- sgfilter(res_may_1,0, 25001,0.03)

res_may_3 <- raterise(res_may_2,0.0023,288)


res_may21_check <- res_may_2[1:3]


squared_diff_may <- (res_may21_check$level - res_may21_check$verified_level)^2
mean_squared_diff_may <- mean(squared_diff_may)
rmse_may <- sqrt(mean_squared_diff_may)


res_may21_check_1 <- res_may_3[1:3]

squared_diff_may1 <- (res_may21_check_1$level - res_may21_check_1$verified_level)^2
mean_squared_diff_may1 <- mean(squared_diff_may1)
rmse_1 <- sqrt(mean_squared_diff_may1)


#plot the comparison

plot_ly() %>%
  add_trace(x=verified_may$timestamp, y=verified_may$level, type="scatter",
            mode= "lines", name="verified", color = "red",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_may_2$timestamp, y=res_may_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", color = "blue", size=0.8,
            line=(list(width = 1))) 






