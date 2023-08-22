# Reservoir level anomaly detection

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)

# Read raw data and remove NA and 0 values


malmsbury_full <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 )



plot_ly(malmsbury_full, x = ~timestamp, y = ~level, mode = "lines")


# Physical constraints
full_supply_level <- 447.8
min_level <- 435.0

# Define anomalies
malmsbury_full <- malmsbury_full %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 3)

anomalies <- malmsbury_full %>% 
  select(1:6) %>% 
  pivot_longer(-1:-2, names_to = "anomaly_type", values_to = "anomaly") %>% 
  filter(anomaly)

# Visualise
ggplot(malmsbury_full, aes(timestamp, level)) + 
  ylim(440,445) +
  labs(title = "comparison of methods of outlier removal") +
  geom_line() + 
  facet_wrap(~anomaly_type) + 
  geom_point(data = anomalies, aes(timestamp, level, col = anomaly_type),
             size = 3) 

# plot only the MAD outliers #

ggplot(malmsbury_full, aes(timestamp, level, col = MAD )) + 
  ylim(435,470) +
  labs(title = "MAD outliers") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "MAD") +
  theme_bw() +
  theme(legend.position = "bottom")


# Clean data
malmsbury_cleaned <- malmsbury_full %>% 
  filter(!MAD) %>% 
  mutate(level = round(level, 2)) #%>% 
#plot_ly(x = ~timestamp, y = ~level, mode = "lines")

ggplot(malmsbury_cleaned, aes(timestamp, level, col = MAD )) + 
  ylim(435,475) +
  labs(title = "Cleaned data using MAD") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")

#######################################################################################################

# smoothing with Exponentially Weighted Moving Average #


# using the zoo package to check EWMA #

malm_level <- malmsbury_cleaned$level
malm_time <- malmsbury_cleaned$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned as ewma #

malmsbury_cleaned_full <- malmsbury_cleaned %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 365)  )

levels_1 <- malmsbury_cleaned_full$level
ewma_1 <- malmsbury_cleaned_full$ewma

# adding a new column as diff to check the difference between level and ewma # 

malmsbury_cleaned_full <- malmsbury_cleaned_full %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1))


malmsbury_cleaned_full <- malmsbury_cleaned_full %>% 
  mutate(ewma_outliers = diff >= 0.03)


# plot malmsbury_cleaned_play with EWMA points  

ggplot(malmsbury_cleaned_full, aes(timestamp, level, col = ewma_outliers)) + 
  ylim(435,448) +
  labs(title = " With EWMA") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b - %d- %y" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")

# clean the data #

malmsbury_cleaned_full1 <- malmsbury_cleaned_full %>% 
  filter(!ewma_outliers) 

# plot without the ewma outliers

ggplot(malmsbury_cleaned_full1, aes(timestamp, level, col = ewma_outliers)) + 
  ylim(435,448) +
  labs(title = " Without outliers from ewma, n =60, >0.03") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d- %y" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")


# compare the original data and the smoothed data #

ggplot(NULL, aes( timestamp, level))+
  labs(title = "comparison original data set vs cleaned") +
  geom_line(data = malmsbury_cleaned_full1, col = "red", size = 1 )+
  geom_line(data = malmsbury_full, col = "blue")






