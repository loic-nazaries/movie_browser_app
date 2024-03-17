# Get the data (only use the first time)
data_file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destination_file <- "movies.RData"

download.file(data_file, destination_file)
