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


# use Savitzky-Golay method to draw a smoothed graph

smoothed <- sgolayfilt(malmsbury_21$level, p = 6, n = 99)
smoothed1 <- sgolayfilt(malmsbury_21$level, p = 4, n = 99)



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
  add_trace(x = malmsbury_21_ori$timestamp , y = malmsbury_21_ori$level, type = "scatter",
            mode = "lines", name = "Original", line=(list(width =0.5))) %>%
  add_trace(x = malmsbury_21$timestamp, y = malmsbury_21$level, type = "scatter", 
            mode = "lines" , name = "FSL and Min removed", line=(list(width = 0.75))) %>%
  add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
             mode = "lines", name= "wims", line = (list(width = 1))) %>%
  # add_trace(x = malmsbury_21$timestamp, y = smoothed , type = "scatter",
  #           mode = "lines", name = "savitzky golay", line=(list(width = 1.5))) %>%
  add_trace(x = malmsbury_21$timestamp, y = smoothed1 , type = "scatter",
            mode = "lines", name = "savitzky golay p4", line=(list(width = 1.5))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 







