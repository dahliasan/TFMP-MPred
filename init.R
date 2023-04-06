# Initialize renv for the project
renv::init()

# Restore the project library from the lockfile
renv::restore()

# init lintr
lintr::use_lintr(type = "tidyverse")
usethis::use_github_action("lint-project")
lintr::lint_dir()
