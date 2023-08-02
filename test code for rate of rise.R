#Plot rate of rise and use it to find problems and give warnings
# full 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)
library(base)

# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"), 
                                                     as.Date("2021-12-31")))


malmsbury_21_ori <- malmsbury_21

#remove physical, MAD, savitzky golay and rate of rise to remove anomalies


res_2021_1 <- phycon(malmsbury_21,447.8,435.0)

res_2021_2 <- mad_outliers(res_2021_1, 3.5)

res_2021_3 <- sgfilter(res_2021_2,0,105,0.03)



# Read verified data

verified <- read_csv("406220.csv")  %>% 
  select(timestamp = Datetime , 
         verified_level = `Water Level` ,
         qc = QC ) %>% 
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_2021 <- verified %>% 
  arrange(timestamp) %>% 
  dplyr::filter( between(as.Date(timestamp), as.Date("2021-01-01"), 
                         as.Date("2021-12-31")))


# using the rate of rise to find the anomalous points

malmsbury_rate_rise <- malmsbury_21_ori[1:2]

malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate(  dh = dh <- abs(malmsbury_rate_rise$level - lag(malmsbury_rate_rise$level)),
           dt = dt <- malmsbury_rate_rise$timestamp - lag(malmsbury_rate_rise$timestamp),
           rate = rate <- dh / as.numeric(dt),
           rate_rise = rate_rise <- rate >= 0.01) %>%
  dplyr::filter(!is.na(dh))


true_index  <- which(malmsbury_rate_rise$rate_rise== TRUE)


#statement to show a message when a point passes the threshold specified



for(index in true_index){
  if(all(malmsbury_rate_rise$rate_rise[(index) :(index- 2)]) == TRUE){
    print(paste('check sensor', "level", malmsbury_rate_rise$level[index], 
               "timestamp", malmsbury_rate_rise$timestamp[index]))
  }
}
  






#####################################################################################################################
#apply MAD filter to find statistical outliers


malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate( MAD = abs(malmsbury_rate_rise$rate - median(malmsbury_rate_rise$rate, na.rm = TRUE)) / 
            mad(rate , na.rm = TRUE) >= 2.5
    )

malmsbury_rate_rise1 <- malmsbury_rate_rise %>%
  dplyr::filter(MAD)


#plot the MAD outliers

plot_ly() %>%
  add_trace(data = malmsbury_rate_rise, x = ~timestamp, y = ~rate, type = "scatter",
            mode = "lines+markers", marker = list(size=5),
            line=(list(width = 0.75))) %>%
  # add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
  #           mode = "lines", name= "wims", line = (list(width = 1))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 


#filter the MAD outliers from the levels and plot time and level

malmsbury_rate_rise2 <- malmsbury_rate_rise %>%
  dplyr::filter(!MAD)

plot_ly() %>%
  add_trace(data = malmsbury_rate_rise , x= ~timestamp, y=~level ,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  add_trace(data= res_2021_3, x=~timestamp, y=~level,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 







