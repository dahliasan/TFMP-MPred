

# check if aniMotum library is installed: if not, install it
if(!require(aniMotum)) {
  
  # If macs are facing issues: 
  # See https://github.com/ianjonsen/aniMotum/issues/42
  
  install.packages("aniMotum", 
                   repos = c("https://cloud.r-project.org",
                             "https://ianjonsen.r-universe.dev"),
                   dependencies = TRUE)
  
}

# Load libraries for plotting tracks
library(sf)
library(rnaturalearth)
library(paletteer)
library(patchwork)
library(tidyverse)
library(viridis)
library(plotly)
library(aniMotum)
library(traipse)
library(lubridate)
library(traipse)
library(gganimate)

