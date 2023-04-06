setwd("~/OneDrive - University of Tasmania/Research/TFMP/R")
source('load packages__tracks.R')
source('tfmp_functions.R')

# Load shearwater data
files <- dir('./data/shearwaters', full.names = T)
d <- read_csv(files)

# Clean data
d <- janitor::clean_names(d)
d <- d %>% 
  mutate(datetime = mdy_hms(paste(date, time))) %>%
  select(-date, -time) %>% 
  rename(lat = latitude, lon = longitude, date = datetime) %>% 
  filter(lon > 0)
d$id <- as.factor(d$id) 

# Plot tracks
plot_tracks(d) 

# dark mode
# scale_fill_viridis_c(option = "inferno")+
# scale_color_viridis_c(option = "inferno")+
# theme_dark() +
# theme(panel.grid = element_blank())



anim <- animate_tracks(d) + 
  ggtitle('Short-tail shearwaters from SW Tas')

animate(anim, fps = 10, width = 5, height = 5, units = 'in', res = 150)

anim_save('stsw-anim.gif')
