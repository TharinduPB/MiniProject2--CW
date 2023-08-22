#Reservoir level smoothing and Anomaly Detection script

#Load libraries 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)
library(base)

############################################################################################
#Read and filter data

# Read raw data and remove NA and 0 values filter the dates

res_data <- read_csv("reslevels_raw.csv") %>% 
  select(timestamp = timestamp, level = level, reservoir = reservoir) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & reservoir == "Trentham No 1" &
                   between(as.Date(timestamp), 
                           as.Date("2015-01-01"), 
                           as.Date("2022-12-31")))

#remove repeating values in the res_data

# res_data <- res_data1 %>%
#   dplyr::distinct(timestamp, .keep_all = TRUE)


# Read verified data

verified <- read_csv("406222_1.csv")  %>% 
  select(timestamp = Datetime , 
         verified_level = `Water Level` ,
         qc = QC ) %>% 
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_filtered <- verified %>% 
  arrange(timestamp) %>% 
  dplyr::filter( between(as.Date(timestamp), as.Date("2015-01-01"), 
                         as.Date("2022-12-31")))

#Keep a data set which will not be modified, for reference

res_data_ori <- res_data

#plot the original 

plot_ly() %>%
  add_trace(data = res_data, x= ~timestamp, y=~level ,type = "scatter",
            mode = "lines" ,name = "Original Data", line=(list(width = 0.75)))

###########################################################################################
#Data smoothing section

#remove physical, MAD, savitzky golay and rate of rise to remove anomalies

res_data_1 <- phycon(res_data,735, 729)

res_data_2 <- mad_outliers(res_data_1, 3.5)

res_data_3 <- sgfilter(res_data_1,0,3,0.03)

############################################################################################
# using the rate of rise to find the anomalous points

res_rate_rise <- res_data_ori[1:2]

res_rate_rise <- res_rate_rise %>%
  mutate(  dh = dh <- abs(res_rate_rise$level - lag(res_rate_rise$level)),
           dt = dt <- res_rate_rise$timestamp - lag(res_rate_rise$timestamp),
           rate = rate <- dh / as.numeric(dt),
           rate_rise = rate_rise <- rate >= 0.023) %>%
  dplyr::filter(!is.na(dh))

true_index  <- which(res_rate_rise$rate_rise== TRUE)

#get a message when there are three or more true values in a specified range

for (index in true_index) {
  range_start <- index
  range_end <- index - 20
  
  # Count the number of TRUE values within the range
  true_count <- sum(res_rate_rise$rate_rise[seq(range_start, range_end)])
  
  if (true_count >= 3) {
    print(paste('check sensor', "level", res_rate_rise$level[index], 
                "timestamp", res_rate_rise$timestamp[index]))
  }
}

#get all the true values 

for(index in true_index){
  if(all(res_rate_rise$rate_rise[index]) == TRUE){
    print(paste('check sensor', "level", res_rate_rise$level[index], 
                "timestamp", res_rate_rise$timestamp[index]))
  }
}

#############################################################################################
#plot graphs to compare and analyse

#Plot the original and smoothed data

plot_ly() %>%
  add_trace(data = res_data, x= ~timestamp, y=~level ,type = "scatter",
            mode = "lines" ,name = "Original Data", line=(list(width = 0.75))) %>%
  # add_trace(data = verified_filtered, x=~timestamp, y=~verified_level, type = "scatter",
  #           mode = "lines", name = "verified data", line = list(width = 0.75)) %>%
  add_trace(data= res_data_3, x=~timestamp, y=~level,type = "scatter",
            mode = "lines" , name= "Smoothed Data", line=(list(width = 0.75))) %>%
  layout(title = 'trentham no1 original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 

#plot the rate of rise vs time

plot_ly() %>%
  add_trace(data = res_rate_rise, x = ~timestamp, y = ~rate, type = "scatter",
            mode = "lines",
            line=(list(width = 0.75))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'rate of rise')) 


