# Identify key biodiversity areas

rm(list = ls())

# Load required packages
source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")

library(track2KBA)

# Load data
tracks <- load_latest_rds("./all_species_tracks.rds", "./data/exports/") %>% 
  dplyr::select(-trip_max_distance)

# Identify age groups that are central place foragers
tracks %>% 
  dplyr::select(id, species, age_group, breeding_stage) %>% 
  distinct() %>% 
  arrange(species, age_group) %>% 
  print(n = Inf)



# Create separate data frames for each species and age group
tracks_split <- tracks %>% 
  group_by(species, age_group, breeding_stage) %>%
  group_split()


# Get number of unique individuals per group
tracks_split %>% 
  map(~ n_distinct(.$id)) %>% 
  unlist() %>% 
  print()

# Only analyse for KBA if group is not "juvenile" age_group and has n > 10 individuals
df1 <- tracks_split[[7]]


# Format data for track2KBA
dataGroup <- formatFields(
  dataGroup = df1, 
  fieldID   = "id", 
  fieldDateTime = "date", 
  fieldLon  = "lon", 
  fieldLat  = "lat"
)

str(dataGroup)

# Handle central place foragers by splitting tracks into trips
# First we need to identify location of the central places
# Typically it's the first points in the data set where they were tagged.
# Plot the tracks first to double check making sure the first point is actually at the colony 

df1 %>% 
  ggplot(aes(x = lon, y = lat, group = id)) +
  geom_path() +
  geom_point(data = df1 %>% group_by(id) %>% slice(1), aes(color = id)) +
  theme_bw()

# Albatross - Mewstone: -43.733333, 146.366667
# Karamu Bay: -43.51067852017067, 146.05060672090673
albatross_colony <- tibble(Longitude = 146.366667, Latitude = -43.733333)
colony <- albatross_colony

# Albatross parameters, innerBuff = 10, returnBuff = 200, duration = 6 (Beal 2021, PhD thesis)
# Mason et al. 2023 used buffer of 400m from colony (albatross island) to classify trips

# Need to split albatross by incubation (sep deployments) and chick rearing period (start dec)


innerBuff <- 10
returnBuff <- 100
duration <- 6

# Split tracks into trips
trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = innerBuff,      # kilometers
  returnBuff = returnBuff, 
  duration   = duration,      # hours
  rmNonTrip  = TRUE
)

mapTrips(trips = trips, colony = colony)

# trips_df <- trips %>% data.frame() %>% tibble() %>% 
#   mutate(ColDist = ColDist / 1000) # convert to km 
# 
# # Plot trips coloured by distance from colony
# trips_df %>% 
#   ggplot(aes(x = Longitude, y = Latitude, group = tripID, color = ColDist > 50)) +
#   geom_path() +
#   facet_wrap(~ID) +
#   theme_bw()
# 
# # filter ids with incomplete trips and plot entire track coloured by trip
# ids_with_incomplete <- trips_df %>% 
#   filter(Returns == "No") %>% 
#   pull(ID) %>%
#   unique()
#   
# trips_df %>% 
#   filter(ID %in% ids_with_incomplete) %>% 
#   ggplot(aes(x = Longitude, y = Latitude, group = tripID, color = Returns)) +
#   geom_path() +
#   facet_wrap(ID~tripID) + 
#   theme_bw()
# 
# 
# 


# Summarise Trip Statistics
trips <- subset(trips, trips$Returns == "Yes" )
sumTrips <- tripSummary(trips = trips, colony = colony) %>% 
  mutate(duration_d = duration / 24)

sumTrips

summary(sumTrips)


# Identify important habitats ---------------------------------------------
# Create equal area projection
tracks1 <- projectTracks( dataGroup = trips, projType = 'azim', custom=TRUE )
class(tracks1)

hVals <- findScale(
  tracks   = tracks1,
  sumTrips = sumTrips)

hVals

# Find KDEs
tracks1 <- tracks1[track1s$ColDist > innerBuff, ] # remove trip start and end points near colony

KDE <- estSpaceUse(
  tracks = tracks1, 
  scale = hVals$href, 
  levelUD = 50, 
  polyOut = TRUE,

)

mapKDE(KDE = KDE$UDPolygons, colony = colony)  


# Does this sample of individuals represent the population? ---------------

indEffectTest(
  tracks = tracks1,
  tripID = "tripID",
  groupVar = "ID",
  scale = hVals$href
)

repr <- repAssess(
  tracks    = tracks1, 
  KDE       = KDE$KDE.Surface, 
  levelUD   = 50,
  iteration = 1000, 
  bootTable = FALSE)

Site <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = 7300, 
  polyOut = TRUE
)

class(Site)

Sitemap <- mapSite(Site, colony = colony)
Site %>% 
  dplyr::filter(.data$potentialSite==TRUE) %>% 
  ggplot() +
  geom_sf(aes(fill = N_animals)) +
  geom_point(data = colony, aes(x = Longitude, y = Latitude), color = "red") +
  theme_bw() +
  scale_fill_viridis(option = "magma")

potSite <- Site %>% dplyr::filter(.data$potentialSite==TRUE) %>% 
  summarise(
    max_animals = max(na.omit(N_animals)), # maximum number of animals aggregating in the site
    min_animals = min(na.omit(N_animals))  # minimum number using the site
  )
