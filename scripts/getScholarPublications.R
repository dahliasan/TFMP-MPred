# Functions to fetch publications data

# From Google Scholar -----------------------------------------------------
library(scholar)
library(dplyr)
library(yaml)

hindell_id <- "8J8xm0kAAAAJ"
lea_id <- "CEBV6KoAAAAJ"
younger_id <- "U7aQd08AAAAJ"
bestley_id <- "9Nl9LvkAAAAJ"
scholar_ids <- c(hindell_id, lea_id, younger_id, bestley_id)

# Get all publications
publications <- scholar_ids %>%
  purrr::map(function(x) {
    get_publications(x) %>%
      tibble() %>%
      mutate(author_id = x)
  }) %>%
  bind_rows()

# Add url & abstract to most recent
publications <- publications %>%
  rowwise() %>%
  mutate(url = paste0("https://scholar.google.com.au/citations?view_op=view_citation&hl=en&user=", author_id, "&citation_for_view=", author_id, ":", pubid))


# convert the dataframe to a YAML string
yaml_string <- as.yaml(publications, column.major = FALSE)

# save the YAML string to a file
write(yaml_string, file = "db/publications.yaml")
