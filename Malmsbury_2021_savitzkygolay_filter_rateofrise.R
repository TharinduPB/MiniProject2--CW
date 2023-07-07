# Remove outliers using the FSL and min to remove obvious outliers, use Savitzky golay method
# to draw a fitting graph on top of the level graph and if there are huge differences + or - remove or
# impute

# full 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)


# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"), 
                                                     as.Date("2021-12-31")))


malmsbury_21_ori <- malmsbury_21

# Physical constraints

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




# visualise in plotly comparison between 

plot_ly() %>%
  # add_trace(x = malmsbury_21_or$timestamp , y = malmsbury_21_ori$level, type = "scatter",
  #            mode = "lines", name = "Original", line=(list(width =0.5))) %>%
  add_trace(x = malmsbury_21$timestamp, y = malmsbury_21$level, type = "scatter", 
            mode = "lines" , name = "FSL and Min removed", line=(list(width = 0.75))) %>%
  add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
            mode = "lines", name= "wims", line = (list(width = 1))) %>%
  add_trace(x=malmsbury_cleaned$timestamp, y=malmsbury_cleaned$level, type = "scatter",
            mode= "lines",name="ewma", line = (list(width = 1)))%>%
  add_trace(x = malmsbury_cleaned1$timestamp, y = malmsbury_cleaned1$level , type = "scatter",
             mode = "lines", name = "sg cleaned", line=(list(width = 1.5))) %>%
  # add_trace(x = malmsbury_21$timestamp, y = smoothed1 , type = "scatter",
  #           mode = "lines", name = "savitzky golay p4", line=(list(width = 1.5))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 

plot_ly() %>%
  add_trace(x=malmsbury_21$timestamp, y=malmsbury_21$level, type="scatter",
            mode= "lines", name="mad cleaned", line=list(widthe=1))


###################################################################################################################################


# using the rate of rise to find the anomalous points


malmsbury_rate_rise <- malmsbury_cleaned1[1:2]


malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate(  dh = dh <- abs(malmsbury_rate_rise$level - lag(malmsbury_rate_rise$level)),
         dt = dt <- malmsbury_rate_rise$timestamp - lag(malmsbury_rate_rise$timestamp),
         rate = rate <- dh / as.numeric(dt),
  
  )

# find points where the rise is high 

malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate (rate_rise = rate_rise <- rate >= 0.0023)

#filter values which are >= rate of rise threshold 
# 
# malmsbury_rate_rise1 <- malmsbury_rate_rise %>% 
#   dplyr::filter(!rate_rise)


# visualise in plotly comparison between 

plot_ly() %>%
  # add_trace(x = malmsbury_21_or$timestamp , y = malmsbury_21_ori$level, type = "scatter",
  #            mode = "lines", name = "Original", line=(list(width =0.5))) %>%
  add_trace(x = malmsbury_21$timestamp, y = malmsbury_21$level, type = "scatter", 
            mode = "lines" , name = "FSL and Min removed", line=(list(width = 0.75))) %>%
  add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
            mode = "lines", name= "wims", line = (list(width = 1))) %>%
  # add_trace(x=malmsbury_cleaned$timestamp, y=malmsbury_cleaned$level, type = "scatter",
  #           mode= "lines",name="ewma", line = (list(width = 1)))%>%
  add_trace(x = malmsbury_cleaned1$timestamp, y = malmsbury_cleaned1$level , type = "scatter",
            mode = "lines", name = "sg cleaned", line=(list(width = 1.5))) %>%
  add_trace(x = malmsbury_rate_rise1$timestamp, y = malmsbury_rate_rise1$level , type = "scatter",
             mode = "lines", name = "rate of rise", line=(list(width = 1.5))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 



############################################################################################################################

# finding the indexes of the rate of rise 
malmsbury_rate_rise1 <- malmsbury_rate_rise

rate_of_rise_indices <- which(malmsbury_rate_rise1$rate_rise== TRUE)



# # Iterate over the outlier indices
# for (index in rate_of_rise_indices) {
#   # remove 96 more data points
#   malmsbury_rate_rise1$level[index] <- malmsbury_rate_rise1$level[index + 96]
# }

# Iterate over the outlier indices
for (index in rate_of_rise_indices) {
  # Remove index + 250 points
  malmsbury_rate_rise1 <- malmsbury_rate_rise1[-c(index:(index + 250)) ,]
}




plot_ly() %>%
  # add_trace(x = malmsbury_21$timestamp, y = malmsbury_21$level, type = "scatter", 
  #           mode = "lines" , name = "FSL and Min removed", line=(list(width = 0.75))) %>%
  add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
            mode = "lines", name= "wims", line = (list(width = 1))) %>%
  add_trace(x=malmsbury_rate_rise1$timestamp, y=malmsbury_rate_rise1$level, type = "scatter",
             mode= "lines",name="rate of rise removed", line = (list(width = 1)))%>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 




