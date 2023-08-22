# just to play with the smoothing values 

# smoothing with Exponentially Weighted Moving Average #


# using the zoo package to check EWMA #

malm_level <- malmsbury_cleaned$level
malm_time <- malmsbury_cleaned$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned as ewma #

malmsbury_cleaned_play <- malmsbury_cleaned %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 15)  )

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

ggplot(malmsbury_cleaned_play, aes(timestamp, level, col = ewma_outliers)) + 
  ylim(441.75,442.5) +
  labs(title = " With EWMA") +
  geom_line(col = "gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom")

# clean the data #

malmsbury_cleaned2 <- malmsbury_cleaned_play %>% 
  filter(!ewma_outliers) 

# plot without the ewma outliers

ggplot(malmsbury_cleaned2, aes(timestamp, level, col = ewma_outliers)) + 
  ylim(442,442.5) +
  labs(title = " Without outliers from ewma, n =60, >0.03") +
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
  xlim(as.numeric(as.POSIXct("2021-05-03")), as.numeric(as.POSIXct("2021-05-10"))) +
  labs(title = "EWMA_values and cleaned levels") +
  geom_line(data = malmsbury_cleaned2, aes(x = as.numeric(timestamp), y = ewma), col = "red", size = 0.5) +
  geom_line(data = malmsbury_cleaned2, aes(x = as.numeric(timestamp), y = level), col = "blue", size = 0.1)




# compare the original data and the smoothed data #

ggplot(NULL, aes( timestamp, level))+
  labs(title = "comparison original data set vs cleaned") +
  geom_line(data = malmsbury_cleaned2, col = "red", size = 1 )+
  geom_line(data = malmsbury, col = "blue")





