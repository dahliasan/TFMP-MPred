# generate last tx date to deployment summary file

# load libraries
library(tidyverse)
library(readxl)
library(writexl)
# load data (sheet cannot be opened)
data <- read_excel("./tfmp deployment data - base copy.xlsx", sheet = "deployment summary")
data <- janitor::clean_names(data)


# set deployment and retrieval dates to date format
data <- data %>% mutate(
  deployment_date = as.Date(deployment_date),
  retrieval_date = as.Date(retrieval_date)
)

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

# loop through each species

# 13 april 2023, thursday, still need to fix deployment info. for females...

add_last_tx_date <- function(x, y, join_col) {
  left_join(x, y, by = join_col) %>%
    mutate(last_tx_date = coalesce(last_tx_date.x, last_tx_date.y)) %>%
    select(-c(last_tx_date.x, last_tx_date.y)) %>%
    filter(!is.na(last_tx_date))
}

process_data <- function(d, subset, join_col) {
  out <- d %>%
    group_by(id) %>%
    summarise(last_tx_date = max(date))

  final_data <- add_last_tx_date(subset, out, join_col)

  return(final_data)
}


final <- list()

for (file in loaded) {
  print(file)

  d <- get(file)

  if (file == "stsw_gpsc") {
    data_subset <- data %>% filter(str_detect(species, "short"), str_detect(tag_model, "CatLogger"))

    final[[file]] <- process_data(d, data_subset, join_by(id == id))

    next
  }

  if (file == "seal_gps") {
    # get the last tx date
    out <- d %>%
      group_by(id) %>%
      summarise(age_class = first(age_class), last_tx_date = max(date))

    # join it to the original spreadsheet
    data_subset <- data %>%
      filter(str_detect(species, "seal"))

    final$juv <- add_last_tx_date(data_subset %>% filter(age_class == "juvenile"), out, join_by(id == id, age_class))
    final$female <- add_last_tx_date(data_subset %>% filter(age_class == "adult"), out, join_by(id == id, age_class))

    next
  }

  data_subset <- data %>%
    filter(id %in% d$id)

  join_col <- switch(file,
    "shy_gps" = join_by(id == id),
    "stsw_gps" = join_by(id == id),
    "shy_gpsc" = join_by(id == id)
  )

  final[[file]] <- process_data(d, data_subset, join_col)
}

final <- bind_rows(final)

final <- final %>% 
  mutate(deployment_duration_d = difftime(last_tx_date, deployment_date, units = 'days') %>% as.numeric() %>% round(0))




# summary of deployment ---------------------------------------------------

summary <- final %>%
  group_by(species, age_class) %>%
  summarise(colony = paste(unique(colony), collapse = ", "),
            deployment_dates = paste(unique(deployment_date), collapse = ", "),
            tag_types = paste(unique(tag_type), collapse = ", "),
            data_collected = paste(unique(data_collected), collapse = ", "),
            deployment_duration = paste0(round(mean(deployment_duration_d), 1), 
                                        " (",
              paste(range(deployment_duration_d), collapse = ", "),
              ")"),
            
            n = n())



# save data to excel spread sheet
write_xlsx(summary, "./tfmp deployment summary - output.xlsx")
write_xlsx(final, "./tfmp deployment data - output.xlsx")
