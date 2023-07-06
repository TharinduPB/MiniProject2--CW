# Comparison with WIMS data

# Libraries

library(tidyverse)
library(lubridate)
library(plotly)

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


# Read raw data (filter 2021 and remove missing measurements)

rawdata <- read_csv("rawdata.csv", show_col_types = FALSE) %>% 
  select(timestamp = Timestamp, level = Level1) %>% 
  arrange(timestamp) %>% 
  # left_join(verified) %>% 
  na.omit()

# 2021 raw data
malmsbury_2021 <- rawdata %>% 
  filter(year(timestamp) == 2021)

# RMSE
nrow(malmsbury_2021)
with(malmsbury_2021, sqrt(mean((level - verified_level)^2)))

# Remove physical constraints
full_supply_level <- 447.8 + 1 
min_level <- 435.0

malmsbury_2021_phys <- malmsbury_2021 %>% 
  filter(between(level, min_level, full_supply_level))

# RMSE
nrow(malmsbury_2021_phys)
with(malmsbury_2021_phys, sqrt(mean((level - verified_level)^2)))

## Visualise

# Create the plot
plot_ly() %>%
  add_trace(x = verified_2021$timestamp, 
            y = verified_2021$verified_level, 
            type = "scatter", mode = "lines", 
            name = "Verified WIMS Data") %>%
  add_trace(x = malmsbury_2021_phys$timestamp, 
            y = malmsbury_2021_phys$level, 
            type = "scatter", mode = "lines", 
            name = "Physical Constraints") %>% 
  layout(title = "verified and cleaned data",
         yaxis = list(title = "Level (AHD)"))
