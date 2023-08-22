# Reservoir level anomaly detection for MAY 2021

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)

# Read raw data and remove NA and 0 values filter 2021

malmsbury_2021 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"), 
                                             as.Date("2021-12-31")))

# Physical constraints
full_supply_level <- 447.8
min_level <- 435.0

# Define anomalies
malmsbury_full_2021 <- malmsbury_2021 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 2.5)

anomalies <- malmsbury_full_2021 %>% 
  select(1:6) %>% 
  pivot_longer(-1:-2, names_to = "anomaly_type", values_to = "anomaly") %>% 
  filter(anomaly)


# Clean data (have to use the FSL min & MAD)# malmsbury_cleaned_2021 is data cleaned after
# using FSL, MIN, MAD values

malmsbury_cleaned_2021 <- malmsbury_full_2021 %>% 
  filter(!MAD & !FSL& !min) %>% 
  mutate(level = round(level, 2))


#########################################################################################################

# smoothing with Exponentially Weighted Moving Average #

# using the zoo package to check EWMA #

malm_level21 <- malmsbury_cleaned_2021$level
malm_time21 <- malmsbury_cleaned_2021$timestamp

malmsbury_zoo21 <- zoo(malm_level21, malm_time21)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_cleaned_2021<- malmsbury_cleaned_2021 %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo21, n = 150)  )

# plot the ewma values and the MAD cleaned data to compare #

malmsbury_cleaned_2021 <- malmsbury_cleaned_2021 %>% mutate(timestamp = .POSIXct(timestamp))


#plot the comparison between ewma values and MAD cleaned data on plot_ly #

# Create first line trace
trace1 <- list(
  x= malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'MAD'
)

# Create second line trace
trace2 <- list(
  x = malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$ewma ,
  type = 'scatter',
  mode = 'lines',
  name = 'EWMA'
)

# Create the plot
plot_ly() %>%
  add_trace(x = trace1$x, y = trace1$y, type = trace1$type, 
            mode = trace1$mode, name = trace1$name) %>%
  add_trace(x = trace2$x, y = trace2$y, type = trace2$type, 
            mode = trace2$mode, name = trace2$name) %>%
  layout(title = 'EWMA and Data cleaned using MAD',
         xaxis = list(title = 'X-axis'),
         yaxis = list(title = 'Y-axis'))



##############################################################################################
# Lets clean the data using the EWMA # 

levels_1 <- malmsbury_cleaned_2021$level
ewma_1 <- malmsbury_cleaned_2021$ewma

# adding a new column as diff to check the difference between level and ewma # 

malmsbury_cleaned_2021 <- malmsbury_cleaned_2021 %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1))


malmsbury_cleaned_2021 <- malmsbury_cleaned_2021 %>% 
  mutate(ewma_outliers = diff >= 0.03)

# clean the data #

malmsbury_full_cleaned_2021 <- malmsbury_cleaned_2021 %>% 
  filter(!ewma_outliers) 


# compare the original data and the smoothed data #

# # using the plotly function to plot multiple graphs to compare original data, cleaned data #

# Create the trace for malmsbury_2021 -original data set #
trace21_1 <- list(
  x= malmsbury_2021$timestamp,
  y = malmsbury_2021$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Data'
)

# Create the trace for malmsbury_cleaned_2021 - data without MAD outliers # 
trace21_2 <- list(
  x = malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$level,
  type = 'scatter',
  mode = 'lines',
  name = 'Without MAD outliers'
)


#create the trace for malmsbury_full_cleaned_2021 - data full cleaned #

trace21_3 <- list(
  x = malmsbury_full_cleaned_2021$timestamp,
  y = malmsbury_full_cleaned_2021$level,
  type = 'scatter',
  mode = 'lines',
  name = 'Cleaned'
)


# Create the plot
plot_ly() %>%
  # add_trace(x = trace21_1$x, y = trace21_1$y, type = trace21_1$type, 
  #           mode = trace21_1$mode, name = trace21_1$name, line=(list(width =0.5))) %>%
  add_trace(x = trace21_2$x, y = trace21_2$y, type = trace21_2$type, 
            mode = trace21_2$mode , name = trace21_2$name, line=(list(width = 0.75))) %>%
  add_trace(x = trace21_3$x, y = trace21_3$y, type = trace21_3$type,
            mode = trace21_3$mode, name = trace21_3$name, line=(list(width = 1.5))) %>%
  layout(title = 'MAD= 2.5, n=150, diff >= 0.03',
       xaxis = list(title = 'timestamp'),
       yaxis = list(title = 'level'))





