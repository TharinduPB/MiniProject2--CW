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



plot_ly(malmsbury_2021, x = ~timestamp, y = ~level, mode = "lines")


# Physical constraints
full_supply_level <- 447.8
min_level <- 435.0

# Define anomalies
malmsbury_2021 <- malmsbury_2021 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 2.5)

anomalies <- malmsbury_2021 %>% 
  select(1:6) %>% 
  pivot_longer(-1:-2, names_to = "anomaly_type", values_to = "anomaly") %>% 
  filter(anomaly)

# Visualise
ggplot(malmsbury_2021, aes(timestamp, level)) + 
   # ylim(440,445) +
  labs(title = "comparison of methods of outlier removal") +
  geom_line() + 
  facet_wrap(~anomaly_type) + 
  geom_point(data = anomalies, aes(timestamp, level, col = anomaly_type),
             size = 3) 

# plot only the MAD outliers #

ggplot(malmsbury_2021, aes(timestamp, level, col = MAD )) + 
  # ylim(435,470) +
  labs(title = "MAD outliers MAD>= 2.5") +
  geom_line(col = "blue") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.1) +
  theme(legend.position = "bottom")


# Clean data (have to use the FSL min & MAD)# malmsbury_cleaned_2021 is data cleaned after
# using FSL, MIN, MAD values

malmsbury_cleaned_2021 <- malmsbury_2021 %>% 
  filter(!MAD,!FSL,!min) %>% 
  mutate(level = round(level, 2))

plot_ly(malmsbury_cleaned_2021, x = ~timestamp, y = ~level, mode = "lines")



# ggplot(malmsbury_cleaned_2021, aes(timestamp, level )) + 
#   labs(title = "Cleaned data using MAD") +
#   geom_line(col = "blue") +
#   scale_x_datetime(date_labels = "%b- %d" ) + 
#   theme(legend.position = "bottom")

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

ggplot()+
  labs(title = "EWMA_values and Level") +
  geom_line(data = malmsbury_cleaned_2021 , aes(timestamp, ewma), col = "red", size = 1 )+
  geom_line(data = malmsbury_cleaned_2021 , aes( timestamp, level ), col = "green", size = 0.05 )


#plot the comparison between ewma values and MAD cleaned data on plot_ly #

# Create first line trace
trace1 <- list(
  x= malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Line 1'
)

# Create second line trace
trace2 <- list(
  x = malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$ewma ,
  type = 'scatter',
  mode = 'lines',
  name = 'Line 2'
)

# Create the plot
plot_ly() %>%
  add_trace(x = trace1$x, y = trace1$y, type = trace1$type, mode = trace1$mode, name = trace1$name) %>%
  add_trace(x = trace2$x, y = trace2$y, type = trace2$type, mode = trace2$mode, name = trace2$name) %>%
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

malmsbury_full_cleaned_2021 <- malmsbury_cleaned_2021 %>% 
  filter(!ewma_outliers) 

# plot without the full cleaned levels#

ggplot(malmsbury_full_cleaned_2021, aes(timestamp, level, col = ewma_outliers)) + 
  # ylim(442,442.5) +
  labs(title = " Without outliers from ewma, n =150, >0.03") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")


# ggplot()+
#   labs(title = "EWMA_values and cleaned levels") +
#   geom_line(data = malmsbury_cleaned21 , aes(timestamp, ewma), col = "red", size = 1 )+
#   geom_line(data = malmsbury_cleaned21 , aes( timestamp, level ), col = "green", size = 0.05 )
# 
# 
# ggplot() +
#   ylim(441.75,442.5)+
#   xlim(as.numeric(as.POSIXct("2021-05-08")), as.numeric(as.POSIXct("2021-05-30"))) +
#   labs(title = "EWMA_values and cleaned levels") +
#   geom_line(data = malmsbury_cleaned21, aes(x = as.numeric(timestamp), y = ewma), col = "red", size = 0.5) +
#   geom_line(data = malmsbury_cleaned21, aes(x = as.numeric(timestamp), y = level), col = "blue", size = 0.1)




# compare the original data and the smoothed data #

ggplot(NULL, aes( timestamp, level))+
  # ylim(440, 450)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_2021, col = "green", size = 0.5 )+
  geom_line(data = malmsbury_cleaned_2021, col = "blue", size = 0.75)+
  geom_line( data = malmsbury_full_cleaned_2021, col = "red", size = 1)


# # using the plotly function to plot multiple graphs to compare original data, cleaned data #

# Create the trace for malmsbury_2021 -original data set #
trace21_1 <- list(
  x= malmsbury_2021$timestamp,
  y = malmsbury_2021$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Levels'
)

# Create the trace for malmsbury_cleaned_2021 - data without MAD outliers # 
trace21_2 <- list(
  x = malmsbury_cleaned_2021$timestamp,
  y = malmsbury_cleaned_2021$level,
  type = 'scatter',
  mode = 'lines',
  name = 'Without MAD'
)


#create the trace for malmsbury_full_cleaned_2021 - data full cleaned #

trace21_3 <- list(
  x = malmsbury_full_cleaned_2021$timestamp,
  y = malmsbury_full_cleaned_2021$level,
  type = 'scatter',
  mode = 'lines',
  name = 'Cleaned Levels'
)


# Create the plot
plot_ly() %>%
  add_trace(x = trace21_1$x, y = trace21_1$y, type = trace21_1$type, 
             mode = trace21_1$mode, name = trace21_1$name, line=(list(width =0.5))) %>%
  add_trace(x = trace21_2$x, y = trace21_2$y, type = trace21_2$type, 
            mode = trace21_2$mode , name = trace21_2$name, line=(list(width = 0.75))) %>%
  add_trace(x = trace21_3$x, y = trace21_3$y, type = trace21_3$type,
            mode = trace21_3$mode, name = trace21_3$name, line=(list(width = 1)))
  layout(title = 'comparison Original data set vs Cleaned',
         xaxis = list(title = 'Timestamp'),
         yaxis = list(title = 'Level'))











# graph_1 <- plot_ly(data = malmsbury_cleaned2,
#                    x = ~timestamp, y = ~level, type = "scatter",
#                    mode = "lines" , name = "final cleaned")
# graph_2 <- plot_ly(data = malmsbury,
#                    x = ~timestamp , y = ~level, type = "scatter",
#                    mode = "lines", name = "original data set")
# graph_3 <- plot_ly(data = malmsbury_cleaned,
#                    x= ~timestamp , y = ~level, type = "scatter",
#                    mode = "lines" , name = "without MAD outliers")
# 
# subplot <- subplot( graph_1 , graph_2 , graph_3 , nrows =3 )
# 
# subplot <- subplot %>% layout(title = "MAD >= 2.0")
# 
# 
# subplot
# 
# 
# 


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
