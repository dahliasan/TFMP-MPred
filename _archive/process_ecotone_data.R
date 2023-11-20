# unused archived functions


# For ecotone birds.
data <- data %>%
  select_if(~ !all(is.na(.))) %>% 
  select(-c(year, month, day, hour, minute, second, date_2))

# Process data by matching ids to deployment dates
data_clean <- tibble()

for (tag in unique(deploy$tag_id)) {
  print(tag)
  
  # get number of birds deployed with tag
  birds <- deploy %>% filter(tag_id == tag)
  nbirds <- birds %>% nrow()
  
  df <- data %>%filter(tag_id == tag)
  
  # if nbirds is 0
  if(nbirds == 0) next
  
  # if nbirds is 1 
  if(nbirds == 1) {
    df$id <- birds$bird_name
    data_clean <- data_clean %>% bind_rows(df) 
    next
  }
  
  # if nbirds is > 1
  # order rows from newest to oldest dates
  birds_desc <- birds %>% arrange(desc(date))
  
  for (i in seq_along(birds_desc$date)) {
    # set parameters for current bird
    new_tag <- paste(tag, i, sep='_')
    startdate <- birds_desc$date[i]
    birdname <- birds_desc$bird_name[i]
    
    # filter current bird
    this_bird <- df %>% filter(date >= startdate )
    this_bird$id <- birdname
    this_bird$tag_id <- new_tag
    
    # remove this_bird rows from original df
    df <- anti_join(df, this_bird, by = "date")
    
    # add to final output df
    data_clean <- data_clean %>% bind_rows(this_bird) 
    print(new_tag)
    print(birdname)
  }
  
}
