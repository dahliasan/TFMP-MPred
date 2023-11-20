# Load packages
library(sf)
library(tidyverse)

# load shp file:
shp_file <- list.files("./data",
    recursive = TRUE,
    pattern = "SouthEastAMPNetwork\\.shp$", full.names = TRUE
)
se_network <- st_read(shp_file)
tfmp <- se_network %>% filter(RESNAME == "Tasman Fracture")


# load albatross deployment data
file <- list.files(
    recursive = TRUE,
    pattern = "Mewstone deployments_oct_Dec22 and retrieval.csv"
)
shy_deployment <- read_csv(file)
shy_deployment <- janitor::clean_names(shy_deployment)


# all species cleaned data directory
clean_dir <- "./data/exports/cleaned/"
ssm_export_dir <- "./data/exports/animotum ssm fits/"

# today's date
today <- Sys.Date()
