# Reservoir level anomaly detection

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)

# Read raw data and remove NA and 0 values filter May 2021
# malmsbury <- read_csv("rawdata.csv") %>% 
#   select(timestamp = Timestamp, level = Level1) %>% 
#   arrange(timestamp) %>% 
#   filter(!is.na(level) & level !=0 & between(timestamp, as.Date("2021-05-01"), as.Date("2021-05-31")))

malmsbury <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-05-01"), 
                                                          as.Date("2021-05-31")))



plot_ly(malmsbury, x = ~timestamp, y = ~level, mode = "lines")


# Physical constraints
full_supply_level <- 447.8
min_level <- 435.0

# Define anomalies
malmsbury <- malmsbury %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 2.0)

anomalies <- malmsbury %>% 
  select(1:6) %>% 
  pivot_longer(-1:-2, names_to = "anomaly_type", values_to = "anomaly") %>% 
  filter(anomaly)

# Visualise
ggplot(malmsbury, aes(timestamp, level)) + 
  ylim(440,445) +
  labs(title = "comparison of methods of outlier removal") +
  geom_line() + 
  facet_wrap(~anomaly_type) + 
  geom_point(data = anomalies, aes(timestamp, level, col = anomaly_type),
             size = 3) 

# plot only the MAD outliers #

ggplot(malmsbury, aes(timestamp, level, col = MAD )) + 
  ylim(435,470) +
  labs(title = "MAD outliers MAD>= 2.5") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "MAD") +
  theme_bw() +
  theme(legend.position = "bottom")


# Clean data
malmsbury_cleaned <- malmsbury %>% 
  filter(!MAD) %>% 
  mutate(level = round(level, 2)) #%>% 
  #plot_ly(x = ~timestamp, y = ~level, mode = "lines")

ggplot(malmsbury_cleaned, aes(timestamp, level, col = MAD )) + 
  ylim(441.75,442.5) +
  labs(title = "Cleaned data using MAD") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")

#########################################################################################################

# smoothing with Exponentially Weighted Moving Average #

# using the zoo package to check EWMA #

malm_level <- malmsbury_cleaned$level
malm_time <- malmsbury_cleaned$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned as ewma #

malmsbury_cleaned_play <- malmsbury_cleaned %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 200)  )

# plot the ewma values and the MAD cleaned data to compare #

malmsbury_cleaned_play <- malmsbury_cleaned_play %>% mutate(timestamp = .POSIXct(timestamp))

ggplot()+
  labs(title = "EWMA_values and Level") +
  geom_line(data = malmsbury_cleaned_play , aes(timestamp, ewma), col = "red", size = 1 )+
  geom_line(data = malmsbury_cleaned_play , aes( timestamp, level ), col = "green", size = 0.05 )


ggplot() +
  xlim(as.numeric(as.POSIXct("2021-05-03")), as.numeric(as.POSIXct("2021-05-10"))) +
  labs(title = "EWMA_values and Level") +
  geom_line(data = malmsbury_cleaned_play, aes(x = as.numeric(timestamp), y = ewma), col = "red", size = 0.5) +
  geom_line(data = malmsbury_cleaned_play, aes(x = as.numeric(timestamp), y = level), col = "blue", size = 0.1)



levels_1 <- malmsbury_cleaned_play$level
ewma_1 <- malmsbury_cleaned_play$ewma

# adding a new column as diff to check the difference between level and ewma # 

malmsbury_cleaned_play <- malmsbury_cleaned_play %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1))


malmsbury_cleaned_play <- malmsbury_cleaned_play %>% 
  mutate(ewma_outliers = diff >= 0.03)


# plot malmsbury_cleaned_play with EWMA points  

# ggplot(malmsbury_cleaned_play, aes(timestamp, level, col = ewma_outliers)) + 
#   ylim(441.75,442.5) +
#   labs(title = " With EWMA") +
#   geom_line(col = "gray") +
#   scale_x_datetime(date_labels = "%b- %d" ) + 
#   geom_point(size = 0.75) +
#   scale_color_grey(start = 0.8, end = 0.2) +
#   theme_bw() +
#   theme(legend.position = "bottom")

# clean the data #

malmsbury_cleaned2 <- malmsbury_cleaned_play %>% 
  filter(!ewma_outliers) 

# plot without the ewma outliers

ggplot(malmsbury_cleaned2, aes(timestamp, level, col = ewma_outliers)) + 
  ylim(442,442.5) +
  labs(title = " Without outliers from ewma, n =15, >0.03") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot()+
  labs(title = "EWMA_values and cleaned levels") +
  geom_line(data = malmsbury_cleaned2 , aes(timestamp, ewma), col = "red", size = 1 )+
  geom_line(data = malmsbury_cleaned2 , aes( timestamp, level ), col = "green", size = 0.05 )


ggplot() +
  xlim(as.numeric(as.POSIXct("2021-05-08")), as.numeric(as.POSIXct("2021-05-30"))) +
  labs(title = "EWMA_values and cleaned levels") +
  geom_line(data = malmsbury_cleaned2, aes(x = as.numeric(timestamp), y = ewma), col = "red", size = 0.5) +
  geom_line(data = malmsbury_cleaned2, aes(x = as.numeric(timestamp), y = level), col = "blue", size = 0.1)




# compare the original data and the smoothed data #

ggplot(NULL, aes( timestamp, level))+
  ylim(440, 445)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_cleaned2, col = "red", size = 1 )+
  geom_line(data = malmsbury, col = "blue", size = 0.1)+
  geom_line( data = malmsbury_cleaned, col = "green", size = 0.5)


# using the plotly function to plot multiple graphs #

graph_1 <- plot_ly(data = malmsbury_cleaned2,
                  x = ~timestamp, y = ~level, type = "scatter",
                  mode = "lines" , name = "final cleaned")
graph_2 <- plot_ly(data = malmsbury,
                   x = ~timestamp , y = ~level, type = "scatter",
                   mode = "lines", name = "original data set")
graph_3 <- plot_ly(data = malmsbury_cleaned,
                   x= ~timestamp , y = ~level, type = "scatter",
                   mode = "lines" , name = "without MAD outliers")

subplot <- subplot( graph_1 , graph_2 , graph_3 , nrows =3 )

subplot <- subplot %>% layout(title = "MAD >= 2.0")
  

subplot





# using the zoo package to check EWMA #

# malm_level <- malmsbury_cleaned$level
# malm_time <- malmsbury_cleaned$timestamp
# 
# malmsbury_zoo <- zoo(malm_level, malm_time)
# 
# # adding a new column to malmsbury_cleaned as ewma #
# 
# malmsbury_cleaned <- malmsbury_cleaned %>% 
#   mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 10)  )
# 
# levels_1 <- malmsbury_cleaned$level
# ewma_1 <- malmsbury_cleaned$ewma
# 
# # adding a new column as diff to check the difference between level and ewma # 
# 
# malmsbury_cleaned <- malmsbury_cleaned %>% 
#   mutate( diff = diff <- abs(levels_1 - ewma_1))
# 
# 
# malmsbury_cleaned <- malmsbury_cleaned %>% 
#   mutate(ewma_outliers = diff >= 0.03)
# 
# 
# # plot malmsbury_cleaned 
# 
# ggplot(malmsbury_cleaned, aes(timestamp, level, col = ewma_outliers)) + 
#   ylim(441.75,442.5) +
#   labs(title = " With EWMA") +
#   geom_line(col = "gray") +
#   scale_x_datetime(date_labels = "%b- %d" ) + 
#   geom_point(size = 0.75) +
#   scale_color_grey(start = 0.8, end = 0.2) +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# # clean the data #
# 
# malmsbury_cleaned1 <- malmsbury_cleaned %>% 
#   filter(!ewma_outliers) 
#   
# # plot without the ewma outliers
# 
# ggplot(malmsbury_cleaned1, aes(timestamp, level, col = ewma_outliers)) + 
#   ylim(441.75,442.5) +
#   labs(title = " Without outliers from ewma") +
#   geom_line(col = "gray") +
#   scale_x_datetime(date_labels = "%b- %d" ) + 
#   geom_point(size = 0.75) +
#   scale_color_grey(start = 0.8, end = 0.2) +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# 
# # compare the original data and the smoothed data #
# 
# ggplot(NULL, aes( timestamp, level))+
#   labs(title = "comparison original data set vs cleaned") +
#   geom_line(data = malmsbury_cleaned1, col = "red", size = 1 )+
#   geom_line(data = malmsbury, col = "blue")
# 
# 
# 
# 
# 
# 
