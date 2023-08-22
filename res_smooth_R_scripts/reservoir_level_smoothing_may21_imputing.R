# Imputing values to make the curve smoother# 
# May 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)

# Read raw data and remove NA and 0 values filter 2021

malmsbury_may21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-05-01"), 
                                             as.Date("2021-05-31")))

# Physical constraints

full_supply_level <- 447.8
min_level <- 435.0

# Define anomalies
# smoothing with Exponentially Weighted Moving Average #
# using the zoo package to check EWMA #

malm_level <- malmsbury_may21$level
malm_time <- malmsbury_may21$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_may21<- malmsbury_may21 %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 15))

# adding a new column as diff to check the difference between level and ewma # 

levels_1 <- malmsbury_may21$level
ewma_1 <- malmsbury_may21$ewma

malmsbury_may21 <- malmsbury_may21 %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1))



# finding the anomalies and adding new columns 

malmsbury_may21 <- malmsbury_may21 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 2.5,
         ewma_outliers = diff >= 0.03)

# Visualise

ggplot()+
  # ylim(440, 450)+
  labs(title = "comparison original data set vs ewma") +
  theme_minimal()+
  geom_line(data = malmsbury_may21, aes(timestamp, level), col = "green", size = 0.5 )+
  geom_line(data = malmsbury_may21, aes(timestamp, ewma), col = "blue", size = 0.75)
  
malmsbury_play <- malmsbury_may21

# Identify the indices of outliers in the dataset
outlier_indices <- which(malmsbury_play$ewma_outliers == TRUE)

# Iterate over the outlier indices
for (index in outlier_indices) {
  # Replace the outlier value with the previous value in the dataset
  malmsbury_play$level[index] <- malmsbury_play$level[index - 1]
}

# Clean up the ewma_outliers column
malmsbury_play$ewma_outliers <- FALSE

# visualize the comparisons between original and new data set using plotly

ggplot()+
  # ylim(442, 443)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_may21, aes(timestamp, level), col = "green", size = 0.5 )+
  geom_line(data = malmsbury_play, aes(timestamp, level), col = "blue", size = 0.75)



# define trace1 

trace_1 <- list(
  x= malmsbury_may21$timestamp,
  y = malmsbury_may21$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Data'
)

# define trace2

trace_2 <- list( 
  x= malmsbury_play_1$timestamp,
  y= malmsbury_play_1$level,
  type = "scatter",
  mode = 'lines',
  name = 'cleaned'
  )

# define another trace for EWMA

trace_3 <- list(
  x= malmsbury_play$timestamp,
  y = malmsbury_play$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'aa'
)

#Create the plot

plot_ly() %>%
  add_trace(x = trace_3$x, y = trace_3$y, type = trace_3$type,
            mode = trace_3$mode, name = trace_3$name, line=(list(width =0.5))) %>%
  add_trace(x = trace_1$x, y = trace_1$y, type = trace_1$type, 
            mode = trace_1$mode , name = trace_1$name, line=(list(width = 0.75))) %>%
  add_trace(x = trace_2$x, y = trace_2$y, type = trace_2$type,
            mode = trace_2$mode, name = trace_2$name, line=(list(width = 1.5))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level'))

###############################################################################################################

# to try out the rate of change of level #

# remove all other columns except the timestamp and levels of malamsbury_play.R

malmsbury_play_1 <- malmsbury_play[1: 2]

 dh <- abs(malmsbury_play_1$level- lag(malmsbury_play_1$level))
 dt <- malmsbury_play_1$timestamp- lag(malmsbury_play_1$timestamp)



malmsbury_play_1 <- malmsbury_play_1 %>% 
  mutate ( dh = dh,
           dt = dt,
           rise = rise <- dh / as.numeric(dt),
           rise_outliers = rise_outliers <- rise >= 0.02
            )

# Identify the indices of outliers in the dataset
outlier_indices_rise <- which(malmsbury_play_1$rise_outliers == TRUE)

# Iterate over the outlier indices
for (index in outlier_indices_rise) {
  # Replace the outlier value with the previous value in the dataset
  malmsbury_play_1$level[index] <- malmsbury_play_1$level[index - 1]
}

# Clean up the rise_outliers column
malmsbury_play$rise_outliers <- FALSE



ggplot()+
  # ylim(442, 443)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_may21, aes(timestamp, level), col = "green", size = 0.5 )+
  geom_line(data = malmsbury_play, aes(timestamp, level), col = "blue", size = 0.75)+
  geom_line(data = malmsbury_play_1, aes(timestamp, level), col = "red", size = 1)





