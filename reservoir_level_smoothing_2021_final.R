# uses FSL MIN MAD to remove outliers, and use EWAM to further remove outlier/impute
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

# Physical constraints

full_supply_level <- 447.8
min_level <- 435.0


# finding the anomalies and adding new columns 

malmsbury_21 <- malmsbury_21 %>% 
  mutate(min = level <= min_level,
         FSL = level > full_supply_level + 2,
         z_score = abs(level - mean(level)) / sd(level) >= 3,
         MAD = abs(level - median(level)) / mad(level) >= 2.5
  )

# Clean the data, filter outliers  
malmsbury_play <- malmsbury_21%>% 
  filter(!MAD,!FSL,!min) %>% 
  mutate(level = round(level, 2))


# Visualise

ggplot()+
  # ylim(440, 450)+
  labs(title = "comparison original data set vs ewma") +
  theme_minimal()+
  geom_line(data = malmsbury_play, aes(timestamp, level), col = "blue", size = 0.05 )


# smoothing with Exponentially Weighted Moving Average #

# using the zoo package to check EWMA #

malm_level <- malmsbury_play$level
malm_time <- malmsbury_play$timestamp

malmsbury_zoo <- zoo(malm_level, malm_time)

# adding a new column to malmsbury_cleaned_2021 as ewma #

malmsbury_cleaned<- malmsbury_play %>% 
  mutate( ewma = ewma <- EMA(malmsbury_zoo, n = 150))



#plot in plotly

# define trace1 

trace_1 <- list(
  x= malmsbury_21$timestamp,
  y = malmsbury_21$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Data'
)

# define trace2

trace_2 <- list( 
  x= malmsbury_cleaned$timestamp,
  y= malmsbury_cleaned$level,
  type = "scatter",
  mode = 'lines',
  name = 'cleaned'
)

# define another trace for EWMA

trace_3 <- list(
  x= malmsbury_cleaned$timestamp,
  y = malmsbury_cleaned$ewma,
  type = 'scatter',
  mode = 'lines',
  name = 'ewma'
)

#Create the plot

plot_ly() %>%
  add_trace(x = trace_3$x, y = trace_3$y, type = trace_3$type,
            mode = trace_3$mode, name = trace_3$name, line=(list(width =0.5))) %>%
  add_trace(x = trace_1$x, y = trace_1$y, type = trace_1$type, 
            mode = trace_1$mode , name = trace_1$name, line=(list(width = 0.75))) %>%
  add_trace(x = trace_2$x, y = trace_2$y, type = trace_2$type,
            mode = trace_2$mode, name = trace_2$name, line=(list(width = 1.5))) %>%
  layout(title = 'original, cleaned , ewma ',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level'))


# using the difference between ewma and the cleaned level to remove outliers (20mm)

levels_1 <- malmsbury_cleaned$level
ewma_1 <- malmsbury_cleaned$ewma

malmsbury_cleaned <- malmsbury_cleaned %>% 
  mutate( diff = diff <- abs(levels_1 - ewma_1),   
          ewma_outliers = diff >= 0.03)

# Clean the data, filter outliers  
malmsbury_cleaned1 <- malmsbury_cleaned%>% 
  filter(!ewma_outliers) 

#comparing with the WIMS data

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


#plot in plotly

# define trace1 

trace_1 <- list(
  x= malmsbury_21$timestamp,
  y = malmsbury_21$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Data'
)

# define trace2

trace_2 <- list( 
  x= malmsbury_cleaned$timestamp,
  y= malmsbury_cleaned$level,
  type = "scatter",
  mode = 'lines',
  name = 'cleaned'
)

# define another trace for EWMA

trace_3 <- list(
  x= malmsbury_cleaned$timestamp,
  y = malmsbury_cleaned$ewma,
  type = 'scatter',
  mode = 'lines',
  name = 'ewma'
)


# define a trace for the data cleaned using ewma

trace_4 <- list(
  x= malmsbury_cleaned1$timestamp,
  y= malmsbury_cleaned1$level,
  type = 'scatter',
  mode = 'lines',
  name = 'final cleaned'
)


# define trace for the WIMS data 

trace_5 <- list(
  x = verified_2021$timestamp,
  y = verified_2021$verified_level,
  type = "scatter",
  mode = "lines",
  name = "WIMS"
)



#Create the plot

plot_ly() %>%
  add_trace(x = trace_3$x, y = trace_3$y, type = trace_3$type,
            mode = trace_3$mode, name = trace_3$name, line=(list(width =0.5))) %>%
  add_trace(x = trace_1$x, y = trace_1$y, type = trace_1$type, 
            mode = trace_1$mode , name = trace_1$name, line=(list(width = 0.5))) %>%
  add_trace(x = trace_2$x, y = trace_2$y, type = trace_2$type,
            mode = trace_2$mode, name = trace_2$name, line=(list(width = 0.5))) %>%
  add_trace(x = trace_4$x, y = trace_4$y, type = trace_4$type,
            mode = trace_4$mode, name = trace_4$name, line=(list(width =0.5))) %>% 
  add_trace(x = trace_5$x, y = trace_5$y, type = trace_5$type,
            mode = trace_5$mode, name = trace_5$name, line=(list(width =1))) 
layout(title = 'original, cleaned , ewma ',
       xaxis = list(title = 'timestamp'),
       yaxis = list(title = 'level'))


#comparing with the WIMS data

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




ggplot()+
  # ylim(440, 450)+
  labs(title = "WIMS data") +
  theme_minimal()+
  geom_line(data = verified_2021, aes(timestamp, verified_level), col = "blue", size = 0.05 )+
  geom_line(data = malmsbury_cleaned1, aes(timestamp, level), col = "red", size = 0.05)





#Imputing


malmsbury_cleaned2 <- malmsbury_cleaned  

# Identify the indices of outliers in the dataset
outlier_indices <- which(malmsbury_cleaned2$ewma_outliers == TRUE)

# Iterate over the outlier indices
for (index in outlier_indices) {
  # Replace the outlier value with the previous value in the dataset
  malmsbury_cleaned2$level[index] <- malmsbury_cleaned2$level[index - 1]
}

# Clean up the ewma_outliers column
malmsbury_cleaned2$ewma_outliers <- FALSE

# visualize the comparisons between original and new data set using plotly

# define trace1 

trace_1 <- list(
  x= malmsbury_21$timestamp,
  y = malmsbury_21$level, 
  type = 'scatter',
  mode = 'lines',
  name = 'Original Data'
)

# define trace2

trace_2 <- list( 
  x= malmsbury_cleaned$timestamp,
  y= malmsbury_cleaned$level,
  type = "scatter",
  mode = 'lines',
  name = 'cleaned'
)

# define another trace for EWMA

trace_3 <- list(
  x= malmsbury_cleaned$timestamp,
  y = malmsbury_cleaned$ewma,
  type = 'scatter',
  mode = 'lines',
  name = 'ewma'
)


# define a trace for the data cleaned using ewma

trace_4 <- list(
  x= malmsbury_cleaned1$timestamp,
  y= malmsbury_cleaned1$level,
  type = 'scatter',
  mode = 'lines',
  name = 'final cleaned'
)

# define a trace for the data imputed 

trace_5 <- list(
  x= malmsbury_cleaned2$timestamp,
  y = malmsbury_cleaned2$level,
  type = 'scatter',
  mode = 'lines',
  name = 'imputed'
)



#Create the plot

plot_ly() %>%
  add_trace(x = trace_3$x, y = trace_3$y, type = trace_3$type,
            mode = trace_3$mode, name = trace_3$name, line=(list(width =0.5))) %>%
  add_trace(x = trace_1$x, y = trace_1$y, type = trace_1$type, 
            mode = trace_1$mode , name = trace_1$name, line=(list(width = 0.75))) %>%
  add_trace(x = trace_2$x, y = trace_2$y, type = trace_2$type,
            mode = trace_2$mode, name = trace_2$name, line=(list(width = 1))) %>%
  add_trace(x = trace_4$x, y = trace_4$y, type = trace_4$type,
            mode = trace_4$mode, name = trace_4$name, line=(list(width =1.5))) %>%
  add_trace(x = trace_5$x, y = trace_5$y, type = trace_5$type,
            mode = trace_5$mode, name = trace_5$name, line=(list(width =2)))
layout(title = 'original, cleaned , ewma , imputer ',
       xaxis = list(title = 'timestamp'),
       yaxis = list(title = 'level'))




