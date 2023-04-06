library(tidyverse)
library(mapdata)
setwd("~/OneDrive - University of Tasmania/Research/TFMP/R")


d <- read_csv(dir(pattern = 'csv')[1])

unique(d$`Platform ID No.`)

albie_ptt <- c('238314', '238315', '238316', '238317', '238318',
               '238319', '238320', '238321', '238322', '238323')

# Check if all albie tags transmitted
albie_ptt %in% unique(d$`Platform ID No.`) # all true


# Plot locations
map <- map_data("world")

d %>% 
  filter(d$`Platform ID No.` %in% albie_ptt) %>%
  ggplot(aes(x = Longitude, y = Latitude)) + 
  # geom_map(data = map, aes(long, lat, map_id = region), map = map, fill = 'grey90') +
  geom_map(map_id='Australia', map = map, fill = 'grey90') +
  geom_point(aes(color = as.factor(`Platform ID No.`))) +
  labs(color = 'argos id') +
  facet_wrap(~`Platform ID No.`) + 
  lims(x= c(145, 148), y = c(-45, -39)) +
  theme_bw()
  

# Plot fur seals
map_australia <- map %>% filter(region == 'Australia')

d %>% 
  filter(!d$`Platform ID No.` %in% albie_ptt) %>%
  ggplot(aes(x = Longitude, y = Latitude)) + 
  geom_map(map_id ='Australia', map = map, fill = 'grey90') +
  geom_point(aes(color = `Msg Date`), size = .5) +
  labs(color = 'argos id') +
  lims(x= c(138, 148), y = c(-45, -39)) +
  facet_wrap(~`Platform ID No.`) +
  theme_bw()



