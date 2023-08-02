#this script will animate the graphs

#load libraries

library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(gifski)
library(viridis)
library(tidyverse)
library(plotly)
library(TTR)
library(zoo)
library(signal)
library(levelsmooth)


# Read raw data and remove NA and 0 values filter 2021

malmsbury_21 <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  dplyr::filter( !is.na(level) & level !=0 & between(as.Date(timestamp), as.Date("2021-01-01"), 
                                                     as.Date("2021-12-31")))


malmsbury_21_ori <- malmsbury_21

#remove physical, MAD, savitzky golay and rate of rise to remove anomalies


res_2021_1 <- phycon(malmsbury_21,447.8,435.0)

res_2021_2 <- mad_outliers(res_2021_1, 3.5)

res_2021_3 <- sgfilter(res_2021_2,0,105,0.03)



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


# using the rate of rise to find the anomalous points

malmsbury_rate_rise <- malmsbury_21_ori[1:2]

malmsbury_rate_rise <- malmsbury_rate_rise %>%
  mutate(  dh = dh <- abs(malmsbury_rate_rise$level - lag(malmsbury_rate_rise$level)),
           dt = dt <- malmsbury_rate_rise$timestamp - lag(malmsbury_rate_rise$timestamp),
           rate = rate <- dh / as.numeric(dt),
           
  )

malmsbury_rate_rise1 <- malmsbury_rate_rise[c(1,5)] 


#animate the original data set 

malmsbury_21_ori %>%
  ggplot( aes(x=timestamp, y= level, color= "red")) +
  ylim(440,449)+
  geom_line() +
  geom_point(alpha = 0.7, size = 0.1, position = position_jitter(width = 0.2, height = 0)) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("malmsbury 2021 original data") +
  theme_ipsum() +
  ylab("level") +
  transition_reveal(timestamp)


malmsbury_rate_rise1 %>%
  ggplot( aes(x=timestamp, y= rate, color= "red")) +
  ylim(0,0.005)+
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("malmsbury 2021 rate of rise") +
  theme_ipsum() +
  ylab("rate of rise") +
  transition_reveal(timestamp)



# Save at gif:
anim_save("malm 2021.gif",animation = last_animation(), renderer = gifski_renderer())



plot_ly() %>%
  add_trace(data = malmsbury_21_ori , x= ~timestamp, y=~level ,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  add_trace(data= res_2021_3, x=~timestamp, y=~level,type = "scatter",
            mode = "lines" , line=(list(width = 0.75))) %>%
  layout(title = 'original vs smooth',
         xaxis = list(title = 'timestamp'),
         yaxis = list(title = 'level')) 



