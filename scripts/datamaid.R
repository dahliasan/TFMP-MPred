# Generate data metadata / data dictionary

# install and load libraries
install.packages("dataReporter")
library(dataReporter)
library(tidyverse)

# load clean track data for species
dir <- "./data/exports/cleaned/"

# list cleaned track files for pattern .RData that is case insensitive
files <- list.files(path = dir, pattern = ".RData", full.names = TRUE)

# load files

for (file in files) {
  load(file)
}

loaded <- files %>% purrr::map(~ load(.x))
loaded <- purrr::reduce(loaded, c)


for (df in loaded) {
  data <- get(df)
  filename <- paste0('./data/data reports/', df, '.Rmd')
  makeDataReport(data, file = filename, replace = TRUE, openResult = FALSE )

  print(df)
}


create_data_dictionary(data)

