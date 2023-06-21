# using only the data from May 2021, draw graphs for FSL, MAD, Z # 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)

# filter the data for the month of May 2021 #


Malmsbury_May21 <-filter(levels, between(as.Date(Timestamp), as.Date("2021-04-30"), 
                                            as.Date("2021-06-01")))


# Check the summary #

summary(Malmsbury_May21)

#####################################################################################################################

# Using the fsl limits find outliers #


full_supply_level <- 447.8
min_level <- 400    # Assumption


Malmsbury_Anomalies_May21 <- Malmsbury_May21 %>% 
  mutate(min = Level1 <= min_level,
         fsl = Level1 > full_supply_level + 2)

# plot the results # 

ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = fsl)) + 
  ylim(435,470) +
  geom_line(col="gray") +
  scale_x_datetime(date_labels = "%b - %d" ) + 
  geom_point(size = 0.25) +
  scale_color_grey(start = 0.8, end = 0.2, name = "fsl") +
  theme_bw() +
  theme(legend.position = "bottom")

#####################################################################################################################

# using the MAD for outlier detection # 

# MAM21_MAD - Malmsbury Anomalies May 2021 by using MAD # 


# Calculating MAD #

MAD <-  mad(Malmsbury_May21$Level1, na.rm = TRUE) 


print(MAD)

# Finding the outliers # 

MAM21_MAD <- which(((abs(Malmsbury_May21$Level1 - 
                         median(Malmsbury_May21$Level1, na.rm = TRUE))) / MAD) >= 3.5 )

# Assigning a column for outliers in Malmsbury_May21 #

Malmsbury_Anomalies_May21$outlier <- FALSE
Malmsbury_Anomalies_May21 $outlier[MAM21_MAD] <- TRUE

# Plot the data # 

ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = outlier )) + 
  ylim(435,470) +
  geom_line(col="gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "MAD") +
  theme_bw() +
  theme(legend.position = "bottom")


#####################################################################################################################

# Using Z scores #

#Calculate Standard deviation # 

SD <- sd(Malmsbury_May21$Level1, na.rm =TRUE)

# Calculate Z score # 

Z_score <- (abs(Malmsbury_May21$Level1 - 
                      mean(Malmsbury_May21$Level1, na.rm = TRUE)) / SD) > 1

# Assigning a column for Zoutliers in Malmsbury_May21 #

  
Malmsbury_Anomalies_May21$Zoutlier <- FALSE
Malmsbury_Anomalies_May21$Zoutlier[Z_score] <- TRUE


# Plot the results #

ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = Zoutlier )) + 
  ylim(435,470) +
  geom_line(col="gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "Z Score") +
  theme_bw() +
  theme(legend.position = "bottom")

###################################################################################################################

# plot all the graphs in one window #



plot1 <- ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = fsl)) + 
  ylim(435,470) +
  geom_line(col="blue") +
  scale_x_datetime(date_labels = "%b - %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "fsl") +
  theme_bw() +
  theme(legend.position = "bottom")

plot2 <- ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = outlier )) + 
  ylim(435,470) +
  geom_line(col="gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "MAD") +
  theme_bw() +
  theme(legend.position = "bottom")

plot3 <- ggplot(Malmsbury_Anomalies_May21, aes(Timestamp, Level1, col = Zoutlier )) + 
  ylim(435,470) +
  geom_line(col="gray") +
  scale_x_datetime(date_labels = "%b- %d" ) + 
  geom_point(size = 0.75) +
  scale_color_grey(start = 0.8, end = 0.2, name = "Z Score") +
  theme_bw() +
  theme(legend.position = "bottom")

combined_plots <- grid.arrange(plot1, plot2, plot3, nrow = 1)

print(combined_plots)
  









