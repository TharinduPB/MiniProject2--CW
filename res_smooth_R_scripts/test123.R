#Plot rate of rise and use it to find problems and give warnings
# full 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)
library(base)


##################################################################################################
# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), 
                                                     as.Date("2021-01-01"), 
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

#########################################################################################
# using the rate of rise to find the anomalous points

malmsbury_rate_rise <- malmsbury_21_ori[1:2]

malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate(  dh = dh <- abs(malmsbury_rate_rise$level - lag(malmsbury_rate_rise$level)),
           dt = dt <- malmsbury_rate_rise$timestamp - lag(malmsbury_rate_rise$timestamp),
           rate = rate <- dh / as.numeric(dt),
           rate_rise = rate_rise <- rate >= 0.01,
           thresh = 0,) %>%
  dplyr::filter(!is.na(dh))

true_index  <- which(malmsbury_rate_rise$rate_rise== TRUE)

###########################################################################################
#get a message when there are three or more true values in a specified range

for (index in true_index) {
  range_start <- index
  range_end <- index - 20
  
  # Count the number of TRUE values within the range
  true_count <- sum(malmsbury_rate_rise$rate_rise[seq(range_start, range_end)])
  
  if (true_count >= 3) {
    print(paste('check sensor', "level", malmsbury_rate_rise$level[index], 
                "timestamp", malmsbury_rate_rise$timestamp[index]))
  }
}

#get all the true values 
for(index in true_index){
  if(all(malmsbury_rate_rise$rate_rise[index]) == TRUE){
    print(paste('check sensor', "level", malmsbury_rate_rise$level[index], 
                "timestamp", malmsbury_rate_rise$timestamp[index]))
  }
}

########################################################################################
#implementing the impulse pump curve

#add a function where the values which are in 1 will gradually come down to zero within 
#100 points

for (index in true_index) {
    if (malmsbury_rate_rise$rate_rise[index + 1] == FALSE) {
      malmsbury_rate_rise$thresh[index:(index + 500)] <- c(seq( from = 1, to = 0,
                                                                by = -0.002))
    } else if (malmsbury_rate_rise$rate_rise[index] == TRUE){
      malmsbury_rate_rise$thresh[index] <- 1
  }
}

#plot the graph to visualise

plot_ly() %>%
  add_trace(data=malmsbury_rate_rise, x=~timestamp, y=~thresh, type= "scatter",
            mode = "lines", line=(list(width=0.75)))

#define a function which can add 1 plus the value of y 

for (index in true_index) {
  if (malmsbury_rate_rise$rate_rise[index + 1] == FALSE) {
    malmsbury_rate_rise$thresh[(index + 1):(index + 500)] <- c(seq( from = 0.998, to = 0,
                                                              by = -0.002))
  } else if (malmsbury_rate_rise$rate_rise[index] == TRUE){
    malmsbury_rate_rise$thresh[index] <- (malmsbury_rate_rise$thresh[index-1]+ 1)
  }
}

malmsbury_rate_rise <- malmsbury_rate_rise %>% 
  mutate(thresh_bol = thresh_bol <- malmsbury_rate_rise$thresh > 1 )


#print a message

true_index2 <- which(malmsbury_rate_rise$thresh > 1)

for(index in true_index2){
  print(paste('check sensor', "level", malmsbury_rate_rise$level[index], 
                "timestamp", malmsbury_rate_rise$timestamp[index]))
}

#plot the rate of rise graph using the thresh values

plot_ly() %>%
  add_trace(data=malmsbury_rate_rise, x=~timestamp, y=~thresh, type= "scatter",
            mode = "lines", line=(list(width=0.75)))

###################################################################################################

#plot the values

plot_ly() %>%
  add_trace(data = malmsbury_rate_rise , x= ~timestamp, y=~level ,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  add_trace(data= res_2021_3, x=~timestamp, y=~sg_smooth,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  add_trace(data= res_2021_3, x=~timestamp, y=~level,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  add_trace(data= malmsbury_rate_rise1, x=~timestamp, y=~signal, type= "scatter",
            mode= "markers", marker = (list(size = 0.5)))%>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 



