# Basic information
# furseals <- read_csv('furseal ptt ids.csv')
furseals <- read_csv("furseal tag deployments.csv")
furseals <- janitor::clean_names(furseals)
furseals$deploy_date <- furseals$deploy_date %>% dmy()
furseals <- furseals %>% select(ptt, id, colony, age_group, species)
juvenile_ids <- furseals %>%
    filter(age_group == "juvenile") %>%
    pull(ptt)
female_ids <- furseals %>%
    filter(age_group == "adult female") %>%
    pull(ptt)
