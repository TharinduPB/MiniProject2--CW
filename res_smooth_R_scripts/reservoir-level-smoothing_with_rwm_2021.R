# using the rolling window method to clean data 

# full 2021 data set will be used # 

library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(runner)

# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  filter(!is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2020-12-25"), 
                                             as.Date("2021-12-31")))

#Define initial cutoff levels

full_supply_level <- 447.8
min_level <- 435.0

# filter initial cutoff levels 

malmsbury_21_F1 <- malmsbury_21 %>%
  mutate(FSL = level > full_supply_level + 2 ,
         min = level < min_level
         )

malmsbury_21_F1 <- malmsbury_21_F1 %>%
  filter(!FSL , !min)

# calculate the rolling median 

malmsbury_21_F1 <- malmsbury_21_F1 %>% 
  mutate( run_med = 
            runner( malmsbury_21_F1$level,
                    f = median,
                    k = 288
        )
  )

# calcualte the rolling MAD 

#Specify the window size

window_size <- 288

# Calculate the MAD using the rolling window method 

malmsbury_21_F1 <- malmsbury_21_F1 %>% 
  mutate(run_mad = rollapply(level, width = window_size, 
                             FUN = function(x) mad(x, constant = 1.4826), align = "right", fill = NA))


# calculating the modified Z score using the rolling window method

# Apply rolling modified z-score with window size 3

malmsbury_21_F1 <- malmsbury_21_F1 %>% 
  mutate( roll_modz = rollapply(level, width = window_size, 
                                     FUN = function(x) (x - median(x)) / run_mad , 
                                     align = "right", fill = NA, na.rm = TRUE))

# Define threshold for outlier detection
threshold <- 3.5

# Detect outliers
outliers <- abs(rolling_modified_zscore) > threshold



# calculate the MAD without using the rolling window method

malmsbury_21_F1 <- malmsbury_21_F1 %>% 
  mutate( MAD = abs(level - median(level)) / mad(level),
          MAD_out = MAD >= 3.5
          )






# find the anomalies using that data 






