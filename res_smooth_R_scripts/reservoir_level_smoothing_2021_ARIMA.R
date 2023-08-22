# Remove outliers using the FSL MAD EWMA and replace them with the average of the previous value
#and the next value

# full 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)

# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"), 
                                             as.Date("2021-12-31")))

malmsbury_21_ori <- malmsbury_21

# Physical constraints

full_supply_level <- 447.8
min_level <- 435.0


# finding the anomalies and adding new columns 

malmsbury_21 <- malmsbury_21 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 1.5
  )

malmsbury_21 <- malmsbury_21 %>%
  filter (!FSL, !min)

# using the ARIMA method to predict correct values for the filtered values. 

# Identify the indices of outliers in the dataset

outlier_indices <- which(malmsbury_21$MAD == TRUE)

# Iterate over the outlier indices

for (index in outlier_indices) {
  # Replace the outlier value with the linear regression predicted value
  malmsbury_21$level[index] <- (malmsbury_21$level[index +1] + malmsbury_21$level[index-1])/2
}


# visualize the comparison between original and MAD cleaned and replaced

ggplot()+
  #ylim(442, 443)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_21, aes(timestamp, level), col = "green", size = 0.1 ) #+
  #geom_line(data = malmsbury_21_ori, aes(timestamp, level), col = "blue", size = 0.01)


# smoothing with Exponentially Weighted Moving Average #

# using the zoo package to check EWMA #

malm_level <- malmsbury_21$level
malm_time <- malmsbury_21$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_cleaned_21<- malmsbury_21 %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 960))

# using the difference between ewma and the cleaned level to remove outliers (20mm)

levels_1 <- malmsbury_cleaned_21$level
ewma_1 <- malmsbury_cleaned_21$ewma

malmsbury_cleaned_21 <- malmsbury_cleaned_21 %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1),   
          ewma_outliers = diff >= 0.030)


# Identify the indices of outliers in the dataset
outlier_indices_2 <- which(malmsbury_cleaned_21$ewma_outliers == TRUE)

# Iterate over the outlier indices
for (index in outlier_indices_2) {
  # Replace the outlier value with the linear regression predicted value
  malmsbury_cleaned_21$level[index] <- (malmsbury_cleaned_21$level[index +1] + 
                                          malmsbury_cleaned_21$level[index-1])/2
}



ggplot()+
  # ylim(442, 443)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_21, aes(timestamp, level), col = "green", size = 0.1 ) +
  # geom_line(data = malmsbury_21_ori, aes(timestamp, level), col = "blue", size = 0.01)+
  geom_line(data = malmsbury_cleaned_21, aes(timestamp,level), col = "red", size = 0.2)


# using the EWMA filter one more time , but this time removing the ewma outliers


malmsbury_cleaned_21_1 <- malmsbury_cleaned_21[1:2] 

malmsbury_cleaned_21_1 <- malmsbury_cleaned_21_1 %>%
  filter(!is.na(level))

# using the zoo package to check EWMA #

malm_level1 <- malmsbury_cleaned_21_1$level
malm_time1 <- malmsbury_cleaned_21_1$timestamp

malmsbury_zoo1 <- zoo(malm_level1, malm_time1)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_cleaned_21_1<- malmsbury_cleaned_21_1 %>% 
  mutate( ewma1 = ewma1 <- EMA(malmsbury_zoo1, n = 288))

# using the difference between ewma and the cleaned level to remove outliers (20mm)

levels_11 <- malmsbury_cleaned_21_1$level
ewma_11 <- malmsbury_cleaned_21_1$ewma1

malmsbury_cleaned_21_1 <- malmsbury_cleaned_21_1 %>% 
  mutate( diff1 = diff1 <- abs(levels_11 - ewma_11),   
          ewma_outliers1 = diff1 >= 0.03)


malmsbury_cleaned_21_1 <- malmsbury_cleaned_21_1 %>% 
  filter( !ewma_outliers1)




ggplot()+
  # ylim(442, 443)+
  labs(title = "comparison original data set vs cleaned") +
  theme_minimal()+
  geom_line(data = malmsbury_21, aes(timestamp, level), col = "green", size = 0.1 ) +
  # geom_line(data = malmsbury_21_ori, aes(timestamp, level), col = "blue", size = 0.01)+
  geom_line(data = malmsbury_cleaned_21, aes(timestamp,level), col = "red", size = 0.2)+
  geom_line(data = malmsbury_cleaned_21_1, aes(timestamp, level), col = "blue", size = 0.3)


# Read verified data
verified <- read_csv("406220.csv")  %>% 
  select(timestamp = Datetime , 
         verified_level = `Water Level` ,
         qc = QC ) %>% 
  mutate(timestamp = lubridate::dmy_hms(timestamp))

verified_2021 <- verified %>% 
  arrange(timestamp) %>% 
  filter( between(as.Date(timestamp), as.Date("2021-01-01"), 
                  as.Date("2021-12-31")))


lm_model <- lm(level ~ timestamp, data = malmsbury_21_ori)
lowess_smooth <- lowess(malmsbury_21_ori$level, f = 0.3)



# visualise in plotly comparison between 

plot_ly() %>%
  add_trace(x = malmsbury_21_ori$timestamp , y = malmsbury_21_ori$level, type = "scatter",
            mode = "lines", name = "Original", line=(list(width =0.5))) %>%
  add_trace(x = malmsbury_cleaned_21$timestamp, y = malmsbury_cleaned_21$level, type = "scatter", 
            mode = "lines" , name = "after ewam", line=(list(width = 0.75))) %>%
  add_trace(x= verified_2021$timestamp, y =verified_2021$verified_level, type = "scatter",
            mode = "lines", name= "wims", line = (list(width = 1))) %>%
  add_trace(x = malmsbury_cleaned_21_1$timestamp, y = malmsbury_cleaned_21_1$level, type = "scatter",
            mode = "lines", name = "after ewma2", line=(list(width = 1.5))) %>%
  add_trace(x = malmsbury_21_ori$timestamp, y = lowess_smooth$y, type = "scatter",
            mode = "lines", name = "LOWESS", line = list(width = 1.5)) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 
  
  # add_trace(x = malmsbury_21_ori$timestamp, y = predict(lm_model), 
  #         name = "Linear Regression", line = list(width = 0.01))





