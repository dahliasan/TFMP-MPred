source("./scripts/load_packages_for_tracks.R")
source("./scripts/tfmp_functions.R")
source("./scripts/utils.R")


# Load data
d <- read_locations_csv("./data/original/albatross/catlogger/", ".csv")

# Check for errors
if (length(d[[2]]) > 0) {
    error_files <- d[[2]]
    e <- read_csv(error_files)
} else {
    d <- d[[1]]
}

# Add logger id
d <- d %>% mutate(id = basename(dirname(file)) %>%
    str_replace("Logger ", "") %>%
    gsub("^0+", "", .))

# Clean data
d1 <- clean_catlogger_data(d)

# get actual deployment start and end dates
deploy <- shy_deployment %>%
    filter(logger_type == "CatLogger GPS") %>%
    select(logger_id, deployment_date, retrieval_date) %>%
    rename(id = logger_id) %>%
    mutate(
        deployment_date = force_tz(deployment_date,
            tzone = "Australia/Sydney"
        ),
        retrieval_date = force_tz(retrieval_date,
            tzone = "Australia/Sydney"
        )
    )

d1 <- left_join(d1, deploy)
d1 <- d1 %>% filter(date >= deployment_date, date <= retrieval_date)
d1 <- d1 %>% filter(!is.na(lc))

shy_gpsc <- d1
save(shy_gpsc, file = paste0(clean_dir, "shy_gpsc.RData"))
