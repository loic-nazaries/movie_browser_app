# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(DT)
library(bslib)
library(thematic)
library(ragg)

# If you want `{ragg}` to handle the font rendering in a Shiny app
options(shiny.useragg = TRUE)

# Call thematic_shiny() prior to launching the app, to change
# R plot theming defaults for all the plots generated in the app
thematic_shiny(font = "auto")


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

# Load data
load("movies.RData")

# Apply formatting to column names
formatted_names <- format_variable_name(colnames(movies))
colnames(movies) <- formatted_names

# Get the numeric column indices
numeric_columns <- which(sapply(movies, is.numeric))

# Subset the data table to include only numeric columns
numeric_data <- movies[, numeric_columns]

# Get the categorical column indices
factor_columns <- which(sapply(movies, is.factor))

# Subset the data table to include only categorical columns
factor_data <- movies[, factor_columns]

# Define the inputs to be logged
inputs_to_log <- c(
  "select_y",
  "select_x",
  "colour",
  "alpha",
  "size",
  "show_data",
  "movie_type"
)

# Define UI
user_interface <- fluidPage(
  theme = bs_theme(
    preset = "solar",
    base_font = font_google("Pacifico"),
  ),

  # App title
  titlePanel("Movie Browser"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      selectInput(
        inputId = "select_y",
        label = "Y-axis:",
        choices = colnames(numeric_data),
        selected = "Audience Score",
      ),
      selectInput(
        inputId = "select_x",
        label = "X-axis:",
        choices = colnames(numeric_data),
        selected = "Critics Score",
      ),
      selectInput(
        inputId = "colour",
        label = "Colour By:",
        choices = colnames(factor_data),
        selected = "Mpaa Rating",
      ),
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0,
        max = 1,
        value = 0.5,
      ),
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0,
        max = 5,
        step = 0.5,
        value = 2,
      ),
      checkboxInput(
        inputId = "show_data",
        label = "Show Data Table",
        value = TRUE,
      ),
      hr(),

      checkboxGroupInput(
        inputId = "movie_type",
        label = "Select Movie Type(s):",
        choices = levels(factor_data$`Title Type`),
        selected = levels(factor_data$`Title Type`),
      ),

    ),

    # Main panel for displaying outputs
    mainPanel(

      textOutput(outputId = "title"),

      plotOutput(
        outputId = "scatterplot",
        brush = "plot_brush",
        hover = "plot_hover",
      ),
      br(),

      # used to output HTML content -> see 'renderUI'
      uiOutput(outputId = "number_obs"),
      htmlOutput(outputId = "averages"),

      verbatimTextOutput(outputId = "regression_output"),

      br(),
      DTOutput(outputId = "movie_table"),
      # Display the download button if the 'show_data' box is ticked
      conditionalPanel(
        condition = "input.show_data == true",
        downloadButton(outputId = "download_data", label = "Download Data"),
      ),

    ),
  ),
)

server <- function(input, output, session) {

  # Observe changes in selected inputs
  observe({
    lapply(inputs_to_log, function(input_id) {
      observeEvent(
        eventExpr = input[[input_id]],
        handlerExpr = {
          input_value <- input[[input_id]]
          log_input(input_id = input_id, input_value = input_value)
        }
      )
    })
  })

  # Subset the movies by title type (documentary, movie or TV show)
  movie_subset <- reactive({
    req(input$movie_type) # Check availability of the input
    movies %>%
      filter(`Title Type` %in% input$movie_type)
  })

  # Filter table based on selected (brushed) points with the mouse
  # (all points selected by default)
  filtered_data <- reactive({

    # If the check box is selected, display the data
    if (input$show_data) {

      # Check if points are selected in the scatter plot
      if (!is.null(input$plot_brush)) {
        # If points are selected, filter the data based on selected points
        brushed_data <- brushedPoints(
          df = movie_subset(),
          brush = input$plot_brush
        )
        return(brushed_data)
      } else if (!is.null(input$plot_hover)) {
        hover_data <- nearPoints(
          df = movie_subset(),
          coordinfo = input$plot_hover
        )
        return(hover_data)
      } else {
        # If no points are selected, display the entire dataset
        return(movie_subset())
      }
    }
  })

  # Print number of movies plotted
  output$number_obs <- renderUI({
    HTML(
      text = paste(
        "The plot displays the relationship between the",
        input$select_x, "and", input$select_y, "of",
        nrow(filtered_data()), "movies."
      )
    )
  })

  output$scatterplot <- renderPlot({

    # Calculate correlation coefficient
    correlation <- movie_subset() %>%
      summarize(
        data = cor(
          x = !!sym(input$select_x),
          y = !!sym(input$select_y)
        )
      ) %>%
      mutate(data = scales::percent(data, accuracy = 0.1)) %>%
      pull(data) # Extract the correlation coefficient value

    # Calculate linear regression coefficients
    lm_model <- lm(
      formula = as.formula(
        paste(
          # Enclose input names in backticks when they contain a space
          paste0("`", input$select_y, "`"),
          "~",
          paste0("`", input$select_x, "`")
        )
      ),
      data = movie_subset(),
    )

    # # Print Linear Regression Summary
    # output$regression_output <- renderPrint({
    #   x <- movie_subset() %>% pull(input$select_x)
    #   y <- movie_subset() %>% pull(input$select_y)
    #   print(
    #     summary(lm(formula = y ~ x, data = movie_subset())),
    #     digits = 3,
    #     signif.stars = TRUE,
    #   )
    # })

    # Plot the scatter plot
    ggplot(
      data = movie_subset(),
      mapping = aes_string(
        # Enclose input names in backticks when they contain a space
        x = paste0("`", input$select_x, "`"),
        y = paste0("`", input$select_y, "`"),
        color = paste0("`", input$colour, "`")  # Label the points based on a category
      )
    ) +
      geom_point(alpha = input$alpha, size = input$size) +

      # Add a title to the plot
      labs(title = paste(input$select_x, "vs.", input$select_y)) +

      # # Add linear regression line with confidence interval
      # geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +

      # Add text annotation for correlation coefficient
      geom_text(
        x = Inf, y = -Inf,
        label = paste("Correlation = ", correlation),
        hjust = 1,
        vjust = -0.5,
        color = "orange",
        size = 5,
      ) +

      # Add text annotation for linear regression equation
      geom_text(
        x = Inf, y = Inf,
        label = paste(
          "Regression: ",
          sprintf("%.2f", coef(lm_model)[1]), " + ",
          sprintf("%.2f", coef(lm_model)[2]), " * ",
          input$select_x
        ),
        hjust = 1,
        vjust = 1,
        color = "white",
        size = 5,
      )
  })

  output$averages <- renderText({
    # Calculate the average values
    average_x <- movie_subset() %>%
      pull(input$select_x) %>%
      mean() %>%
      round(digits = 1)
    average_y <- movie_subset() %>%
      pull(input$select_y) %>%
      mean() %>%
      round(digits = 1)

    # Convert the calculated value to a string
    str_x <- paste("Average", input$select_x, "=", average_x)
    str_y <- paste("Average", input$select_y, "=", average_y)

    # Call the strings as HTML element
    HTML(
      text = paste(
        "<span style='color: red;'>",
        str_x,
        str_y,
        "</span>",
        sep = "<br/>"
      )
    )
  })

  # Display the table of data based on the filtered data (brushed or not)
  output$movie_table <- renderDT({
    datatable(
      data = filtered_data(),
      options = list(pageLength = 10),
      rownames = FALSE,
    )
  })

  # Download file following filtering, or not
  output$download_data <- downloadHandler(
    filename = "movie_data.csv", # Default name; can be change on save
    content = function(file) {
      write_csv(filtered_data(), file)

      # Log the download action
      log_action(action = "Downloaded movie data")
    }
  )
}

shinyApp(ui = user_interface, server = server)
