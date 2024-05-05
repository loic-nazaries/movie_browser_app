# Helper Functions

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Function to format variable names
format_variable_name <- function(names) {
  names %>%
    str_replace_all("_", " ") %>%   # Replace "_" with space
    str_to_lower() %>%              # Convert to lowercase
    str_squish() %>%                # Remove extra spaces
    str_split(" ") %>%              # Split words
    map(
      function(x) {
        str_to_title(x) %>%     # Capitalize split words
          str_c(collapse = " ")   # Join words back into a single string
      }
    )
}


# Function to log user input
log_input <- function(input_id, input_value) {
  timestamp <- Sys.time()
  log_message <- paste(
    timestamp,
    "- User input from", input_id, ":", input_value,
    "\n"
  )
  # Append log message to a log file
  cat(log_message, file = "user_log.txt", append = TRUE)
}


# Function to log user actions
log_action <- function(action) {
  timestamp <- Sys.time()
  log_message <- paste(timestamp, "- Action:", action, "\n")
  # Append log message to a log file
  cat(log_message, file = "user_log.txt", append = TRUE)
}


# Function to filter data based on user interactions
filter_data <- function(input, dataframe) {
  # If the check box is selected, display the data
  if (input$show_data) {

    # Check if points are selected/brushed in the scatter plot
    if (!is.null(input$plot_brush)) {

      # If points are selected, filter the data based on selected points
      brushed_data <- brushedPoints(
        df = dataframe,
        brush = input$plot_brush
      )
      return(brushed_data)
    } else if (!is.null(input$plot_hover)) {

      hover_data <- nearPoints(
        df = dataframe,
        coordinfo = input$plot_hover
      )
      return(hover_data)
    } else {
      # If no points are selected, display the entire dataset
      return(dataframe)
    }
  }
}


# Function to calculate averages of selected x and y variables
calculate_averages <- function(dataframe, variable_x, variable_y) {
  # Calculate the average values
  average_x <- dataframe %>% pull(variable_x) %>% mean() %>% round(digits = 1)
  average_y <- dataframe %>% pull(variable_y) %>% mean() %>% round(digits = 1)

  # Convert the calculated value to a string
  str_x <- paste("Average", variable_x, "=", average_x)
  str_y <- paste("Average", variable_y, "=", average_y)

  # Call the strings as HTML element
  HTML(
    paste(
      "<span style='color: red;'>",
      str_x,
      str_y,
      "</span>",
      sep = "<br/>"
    )
  )
}


# Function to perform correlation analysis between selected variables
perform_correlation <- function(dataframe, variable_x, variable_y) {
  dataframe %>%
    summarize(
      data = cor(
        x = !!sym(variable_x),
        y = !!sym(variable_y)
      )
    ) %>%
    mutate(data = scales::percent(data, accuracy = 0.1)) %>%
    pull(data) # Extract the correlation coefficient value
}


# Function to perform linear regression on selected variables
perform_linear_regression <- function(dataframe, variable_x, variable_y) {
  x <- dataframe %>% pull(variable_x)
  y <- dataframe %>% pull(variable_y)
  lm(formula = y ~ x, data = dataframe)
}


# Function to create a scatter plot
draw_scatterplot <- function(
  dataframe,
  variable_x,
  variable_y,
  colour,
  alpha_value,
  size_value,
  correlation_coefficient,
  regression_model
) {
  ggplot(
    data = dataframe,
    mapping = aes_string(
      # Enclose input names in backticks when they contain a space
      x = paste0("`", variable_x, "`"),
      y = paste0("`", variable_y, "`"),
      color = paste0("`", colour, "`")  # Label the points based on a category
    )
  ) +
    geom_point(alpha = alpha_value, size = size_value) +
    labs(title = paste(variable_x, "vs.", variable_y)) +

    # # Add linear regression line with confidence interval
    # geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +

    # Add text annotation for correlation coefficient
    geom_text(
      x = Inf, y = -Inf,
      label = paste("Correlation = ", correlation_coefficient),
      hjust = 1,
      vjust = -0.5,
      color = "red",
      size = 5,
    ) +

    # Add text annotation for linear regression equation
    geom_text(
      x = Inf, y = Inf,
      label = paste(
        "Regression: ",
        sprintf("%.2f", coef(regression_model)[1]), " + ",
        sprintf("%.2f", coef(regression_model)[2]), " * ",
        variable_x
      ),
      hjust = 1,
      vjust = 1,
      color = "blue",
      size = 5,
    )
}
