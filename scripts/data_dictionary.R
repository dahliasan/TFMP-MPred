# Load necessary libraries
library(dplyr)

create_data_dictionary <- function(data, variable_descriptions = NULL) {
  
  # Function to calculate summary statistics for numeric variables
  numeric_summary <- function(col) {
    if (is.numeric(col)) {
      return(c(min = min(col, na.rm = TRUE),
               max = max(col, na.rm = TRUE),
               mean = mean(col, na.rm = TRUE),
               sd = sd(col, na.rm = TRUE),
               se = sd(col, na.rm = TRUE) / sqrt(length(col[!is.na(col)])),
               range = diff(range(col, na.rm = TRUE))))
    } else {
      return(c(min = NA, max = NA, mean = NA, sd = NA, se = NA, range = NA))
    }
  }
  
  # Function to calculate summary statistics for factor, character, and datetime variables
  non_numeric_summary <- function(col) {
    return(c(n_unique_values = length(unique(col)),
             n_missing_values = sum(is.na(col))))
  }
  
  # Calculate summary statistics for numeric and non-numeric variables
  numeric_stats <- sapply(data, numeric_summary)
  non_numeric_stats <- sapply(data, non_numeric_summary)
  
  # Create a data frame for the data dictionary
  data_dictionary <- data.frame(Variable = colnames(data),
                                Description = variable_descriptions,
                                Max = numeric_stats["max", ],
                                Min = numeric_stats["min", ],
                                Mean = numeric_stats["mean", ],
                                SD = numeric_stats["sd", ],
                                SE = numeric_stats["se", ],
                                Range = numeric_stats["range", ],
                                NObservations = nrow(data),
                                NUniqueValues = non_numeric_stats["n_unique_values", ],
                                NMissingValues = non_numeric_stats["n_missing_values", ],
                                VariableType = sapply(data, class),
                                stringsAsFactors = FALSE)
  
  return(data_dictionary)
}

# Example usage
data <- data.frame(A = rnorm(100, 10, 2),
                   B = rnorm(100, 5, 1),
                   C = rnorm(100, 0, 0.5),
                   D = factor(sample(c("M", "F"), 100, replace = TRUE)),
                   E = sample(LETTERS[1:5], 100, replace = TRUE),
                   F = as.POSIXct(Sys.time()) + runif(100, 0, 86400))

variable_descriptions <- c("Variable A description",
                           "Variable B description",
                           "Variable C description",
                           "Variable D description",
                           "Variable E description",
                           "Variable F description")

data_dictionary <- create_data_dictionary(data, variable_descriptions)

# Print the data dictionary
print(data_dictionary)
