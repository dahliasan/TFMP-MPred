# Combine all species tracks

source("./scripts/load packages__tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/ids.R")
source("./scripts/utils.R")


# Specify the directory containing the .rds files
dir_path <- "./data/exports/animotum ssm fits/2023-05-16/"

# List all .rds files in the directory
file_list <- list.files(path = dir_path, pattern = "*.rds", full.names = TRUE)

ssms <- purrr::map(file_list, function(file) {
  ssm <- read_rds(file)
  ssm <- grab(ssm, what = "predicted")
  
  # detect species
  if(str_detect(file, "adult")) {
    specie <- "adult female"
  }  else if(str_detect(file, "juv")) {
    specie <- "juvenile"
  } else if(str_detect(file, "albatross")) {
    specie <- "shy albatross"
  } else if(str_detect(file, "stsw")) {
    specie <- "short-tailed shearwater"
  }
  
  # detect fur seals species
  if(specie == "adult female" | specie == "juvenile") {
    # adult females detected
    ssm <- ssm %>%
      mutate(file = file) %>%
      dplyr::select(id, date, lon, lat)
    
    id <- furseals %>%
      filter(age_group == specie) %>%
      dplyr::select(id, species, age_group)
    
    ssm <- ssm %>% left_join(id)
    
    return(ssm)
  }
  
  ssm <- ssm %>%
    mutate(file = file, species = specie, age_group = "adult") %>%
    dplyr::select(id, date, lon, lat, species, age_group)
  
  return(ssm)
}) %>% 
  bind_rows()



# Load shearwater ---------------------------------------------------------

load("./data/exports/cleaned/stsw_gps_ecotone.RData")
load("./data/exports/cleaned/stsw_gpsc.RData")


shear <- stsw_gps %>%
  ungroup() %>%
  select(id, date, lat, lon, trip_max_distance) %>% 
  mutate(species = "short-tailed shearwater", 
         age_group = "adult", 
         trip_type = ifelse(
           trip_max_distance < 500000,
           "short",
           "long")
  )


shear_cat <- stsw_gpsc %>%
  ungroup() %>%
  select(id, date, lat, lon) %>% 
  mutate(species = "short-tailed shearwater", 
         age_group = "adult")




# Save file ---------------------------------------------------------------
save_rds(all, filename = "all_species_tracks.rds", "./data/exports/")
