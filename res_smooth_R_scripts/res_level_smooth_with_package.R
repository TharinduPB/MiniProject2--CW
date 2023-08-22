# smooth reservoir level data using physical constraints, Median Absolute Deviation, 
# savitzky- golay filter, and rate of rise indexing. 
# R package which was developed has been used in this code. 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)

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

res_2021_1 <- phycon(res_2021,447.8,435.0)

res_2021_2 <- mad_outliers(res_2021_1, 3.5)

res_2021_3 <- sgfilter(res_2021_2,0,105, 0.03)

res_2021_3 <- raterise(res_2021_3, 0.01, 250)


plot_ly() %>%
  add_trace(x=verified_2021$timestamp, y=verified_2021$level, type="scatter",
            mode= "lines", name="verified", color = "red",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_2021_2$timestamp, y=res_2021_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", color = "blue", size=0.8,
            line=(list(width = 1))) %>%
  add_trace(x=res_2021_3$timestamp, y=res_2021_3$level, type="scatter",
            mode= "lines", name="rate of rise cleaned", color = "green", size=0.9,
            line=(list(width = 1)))

plot_ly() %>%
  add_trace(x=verified_2021$timestamp, y=verified_2021$level, type="scatter",
            mode= "lines", name="verified",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_2021_2$timestamp, y=res_2021_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", size=0.8,
            line=(list(width = 1))) 
  



###########################################################################################################
# full data set #


res_full <- read_csv("rawdata.csv") %>%
  select(timestamp = Timestamp, level = Level1) %>%
  arrange(timestamp) %>%
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2017-01-01"),
                                                     as.Date("2022-12-31")))


# Read verified data
verified <- read_csv("406220.csv")  %>%
  select(timestamp = Datetime ,
         level = `Water Level` ,
         qc = QC ) %>%
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_full <- verified %>%
  arrange(timestamp) %>%
  dplyr::filter( between(as.Date(timestamp), as.Date("2017-01-01"),
                         as.Date("2022-12-31")))



#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_full_1 <- phycon(res_full,447.8,435.0,3.5)

res_full_2 <- sgfilter(res_full_1,6,99,0.03)

res_full_3 <- raterise(res_full_2,0.0023,250)


plot_ly() %>%
  # add_trace(x= res_full$timestamp, y=res_full$level, type="scatter",
  #           mode= "lines", name="original",size= 0.5, 
  #           line=(list(width = 1))) %>%
  add_trace(x=verified_full$timestamp, y=verified_full$level, type="scatter",
            mode= "lines", name="verified",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_full_2$timestamp, y=res_full_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", size=0.8,
            line=(list(width = 1))) #%>%
  # add_trace(x=res_full_3$timestamp, y=res_full_3$level, type="scatter",
  #           mode= "lines", name="rate of rise cleaned", color = "green", size=0.9,
  #           line=(list(width = 1)))


###########################################################################################################
# may_2017 #


res_may21 <- read_csv("rawdata.csv") %>%
  select(timestamp = Timestamp, level = Level1) %>%
  arrange(timestamp) %>%
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2017-05-01"),
                                                     as.Date("2017-05-31")))


# Read verified data
verified <- read_csv("406220.csv")  %>%
  select(timestamp = Datetime ,
         level = `Water Level` ,
         qc = QC ) %>%
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_may21 <- verified %>%
  arrange(timestamp) %>%
  dplyr::filter( between(as.Date(timestamp), as.Date("2017-05-01"),
                         as.Date("2017-05-31")))



#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_may_1 <- phycon(res_may21,447.8,435.0,3.5)

res_may_2 <- sgfilter(res_may_1,6,99,0.03)

res_may_3 <- raterise(res_may_2,0.0023,250)


plot_ly() %>%
  add_trace(x= res_may21$timestamp, y=res_may21$level, type="scatter",
            mode= "lines", name="original",size= 0.5, 
            line=(list(width = 1))) %>%
  add_trace(x=verified_may21$timestamp, y=verified_may21$level, type="scatter",
            mode= "lines", name="verified",size= 0.5, 
            line=(list(width = 1)))%>%
  add_trace(x=res_may_2$timestamp, y=res_may_2$level, type="scatter",
            mode= "lines", name="cleaned_sgfilter", size=0.8,
            line=(list(width = 1))) #%>%
# add_trace(x=res_full_3$timestamp, y=res_full_3$level, type="scatter",
#           mode= "lines", name="rate of rise cleaned", color = "green", size=0.9,
#           line=(list(width = 1)))


plot_ly() %>%
  add_trace(x=verified_may21$timestamp, y=verified_may21$level, type="scatter",
            mode= "lines", name="verified",size= 0.5, 
            line=(list(width = 1))) 
  









