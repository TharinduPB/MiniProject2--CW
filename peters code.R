library(tidyverse)


# Read raw data and remove NA and 0 values
malmsbury <- read_csv("rawdata.csv") %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  filter(!is.na(level) & level != 0)


# Inspect range
summary(malmsbury)


# Physical constraints
full_supply_level <- 447.8
min_level <- 400 # Assumption


malmsbury_anomalies <- malmsbury %>% 
  mutate(min = level <= min_level,
         fsl = level > full_supply_level + 2)



