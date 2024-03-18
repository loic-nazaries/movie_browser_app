library(dplyr)
library(DT)
library(stringr)
library(purrr)

# Function to format variable names
format_variable_names <- function(names) {
    names %>%
        str_replace_all("_", " ") %>%        # Replace "_" with space
        str_to_lower() %>%                    # Convert to lowercase
        str_squish() %>%                      # Remove extra spaces
        str_split(" ") %>%                    # Split words
        # map_chr(~ str_to_title(.x)) %>%       # Capitalize split words
        # str_c(collapse = " ")                 # Join words back into a single string
        map(function(x) {
            str_to_title(x) %>%               # Capitalize split words
            str_c(collapse = " ")             # Join words back into a single string
        })  # Use map and str_c to combine elements
}

# # Load Star Wars Data
# datatable(starwars)

# Load Movie Data
load("movies.RData")

# Apply formatting to column names
formatted_names <- format_variable_names(colnames(movies))
colnames(movies) <- formatted_names

str(movies)

sapply(movies, class)

# Get the numeric column indices
numeric_columns <- which(sapply(movies, is.numeric))

# Subset the data table to include only numeric columns
numeric_data <- movies[, numeric_columns]

# Get the numeric column indices
factor_columns <- which(sapply(movies, is.factor))

# Subset the data table to include only numeric columns
factor_data <- movies[, factor_columns]
