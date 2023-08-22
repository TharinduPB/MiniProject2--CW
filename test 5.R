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

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), 
                                                     as.Date("2015-01-01"), 
                                                     as.Date("2022-12-31")))

malmsbury_21_ori <- malmsbury_21

## remove physical, MAD, savitzky golay to remove anomalies ##

#inputs : Full supply level and Min level
res_2021_1 <- phycon(malmsbury_21,447.8,435.0)

#inputs : limit for MAD 
res_2021_2 <- mad_outliers(res_2021_1, 3.5)

#inputs : binomial number, number of points considered, limit to cut off values
res_2021_3 <- sgfilter(res_2021_2,0,105,0.03)


## use the rate rise functions to find anomalies ##

#inputs : limit for rate of rise, number-donot change
res_2021_4 <- raterise(malmsbury_21, 0.01, 100)

#inputs : number of points in which the value will go down, deduction rate
res_2021_5 <- anomal_message(res_2021_4,500,0.02)


#visualize the data

p1 <- plot_ly() %>%
  add_trace(data=res_2021_5, x=~timestamp, y=~thresh, mode = "lines",
            type = "scatter", line = list(width=0.5))

p2 <- plot_ly() %>% 
  add_trace(data=malmsbury_21_ori, x=~timestamp, y=~level, mode="lines",
            type = "scatter", line= list(width=0.5)) 

  
subplot(p1,p2, nrows = 2)




