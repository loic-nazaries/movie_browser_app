# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(DT)

# Helper Functions

# Function to format variable names
format_variable_names <- function(names) {
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
        title,
        correlation_coefficient,
        regression_model
) {
    ggplot(
        data = dataframe,
        mapping = aes_string(
            # Enclose input names in backticks when they contain a space
            x = paste0("`", variable_x, "`"),
            y = paste0("`", variable_y, "`"),
            color = paste0("`", colour, "`")
        )
    ) +
        geom_point(alpha = alpha_value, size = size_value) +
        ggtitle(label = title) +
        labs(color = colour) +

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
                input$select_x
            ),
            hjust = 1,
            vjust = 1,
            color = "blue",
            size = 5,
        )
}


# Load data
load("movies.RData")

# Apply formatting to column names
formatted_names <- format_variable_names(colnames(movies))
colnames(movies) <- formatted_names

# Get the numeric column indices
numeric_columns <- which(sapply(movies, is.numeric))

# Subset the data table to include only numeric columns
numeric_data <- movies[, numeric_columns]

# Get the numeric column indices
factor_columns <- which(sapply(movies, is.factor))

# Subset the data table to include only numeric columns
factor_data <- movies[, factor_columns]


# Define UI
user_interface <- fluidPage(
    
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
                step=0.5,
                value = 2,
            ),
            checkboxInput(
                inputId = "show_data",
                label = "Show Data Table",
                value = TRUE,
            ),
            textInput(
                inputId = "plot_title",
                label = "Plot Title",
            ),
            actionButton(
                inputId = "update_plot_title",
                label = "Update Plot Title",
            ),
            hr(),

            checkboxGroupInput(
                inputId = "movie_type",
                label = "Select Movie Type(s):",
                choices = levels(factor_data$`Title Type`),
                selected = "Feature Film",
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

    # Define the inputs to be logged
    inputs_to_log <- c(
        "select_y",
        "select_x",
        "colour",
        "alpha",
        "size",
        "show_data",
        "plot_title",
        "movie_type"
    )

    # Observe changes in selected inputs
    observe({
        lapply(inputs_to_log, function(input_id) {
            observeEvent(
                eventExpr = input[[input_id]],
                handlerExpr = {
                    input_value <- input[[input_id]]
                    log_input(input_id=input_id, input_value=input_value)
                }
            )
        })
    })

    # Set initial value for plot title text input
    observe({
        updateTextInput(
            session = session,
            inputId = "plot_title",
            value = paste(input$select_y, "vs.", input$select_x)
        )
    })

    # Use a reactive event to set the plot's title
    new_plot_title <- eventReactive(
        eventExpr = input$update_plot_title,
        valueExpr = input$plot_title,
        ignoreNULL = FALSE, # does not apply the change when the app first loads
    )

    # Subset the movies by title type (documentary, movie or TV show)
    movie_subset <- reactive({
        req(input$movie_type) # Check availability of the input
        movies %>%
            filter(`Title Type` %in% input$movie_type)
        # movies %>% select(input$movie_type)
    })

    # Filter table based on selected (brushed) points with the mouse
    # (all points selected by default)
    filtered_data <- reactive({

        # If the check box is selected, display the data
        if (input$show_data) {

            # Check if points are selected/brushed in the scatter plot
            if (!is.null(input$plot_brush)) {

                # If points are selected, filter the data based on selected points
                brushed_data <- brushedPoints(
                    df = movie_subset(),
                    brush = input$plot_brush
                )
                return(brushed_data)
            }
            
            else if (!is.null(input$plot_hover)) {
                
                hover_data <- nearPoints(
                    df = movie_subset(),
                    coordinfo = input$plot_hover
                )
                return(hover_data)
            }

            else {
                # If no points are selected, display the entire dataset
                return(movie_subset())
            }
        }
    })

    # Print number of movies plotted
    output$number_obs <- renderUI({
        HTML(
            text = paste(
                "The plot displays the relationship between the <br>",
                input$select_x, "and", input$select_y, "of",
                nrow(filtered_data()), "movies."
            )
        )
    })

    output$scatterplot <- renderPlot({

        # Calculate correlation coefficient
        correlation_coefficient <- perform_correlation(
            dataframe = movie_subset(),
            variable_x = input$select_x,
            variable_y = input$select_y
        )

        # Perform Linear Regression
        regression_model <- perform_linear_regression(
            dataframe = movie_subset(),
            variable_x = input$select_x,
            variable_y = input$select_y
        )

        # # Print Linear Regression Summary
        # output$regression_output <- renderPrint({
        #     print(summary(regression_model), digits = 3, signif.stars = TRUE)
        # })

        # Plot the scatter plot
        # draw_scatterplot(
        #     dataframe = movies,
        #     variable_x = input$select_x,
        #     variable_y = input$select_y,
        #     colour = input$colour,
        #     alpha_value = input$alpha,
        #     size_value = input$size,
        #     title = input$plot_title,
        #     correlation_coefficient = correlation_coefficient,
        #     regression_model = regression_model
        # )
        ggplot(
            data = movie_subset(),
            mapping = aes_string(
                # Enclose input names in backticks when they contain a space
                x = paste0("`", input$select_x, "`"),
                y = paste0("`", input$select_y, "`"),
                color = paste0("`", input$colour, "`")
            )
        ) +
            geom_point(alpha = input$alpha, size = input$size) +

            # Add a title to the plot
            ggtitle(label = input$plot_title) +
            
            # # Add a title to the plot using a reactive expression
            # ggtitle(label = new_plot_title()) +

            # # Update plot title ONLY when one of the other input is modified
            # ggtitle(label = isolate({input$plot_title})) +
            
            # Label/colourise the points based on a category
            labs(color = input$colour) +

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
                    input$select_x
                ),
                hjust = 1,
                vjust = 1,
                color = "blue",
                size = 5,
            )
    })

    # Print the variable averages as HTML elements
    output$averages <- renderText({
        calculate_averages(
            dataframe = movie_subset(),
            variable_x = input$select_x,
            variable_y = input$select_y
        )
    })

    # Display the table of movie data
    output$movie_table <- renderDT({
        filtered_data()
    })

    # Download file
    output$download_data <- downloadHandler(
        filename = "movie_data.csv", # Default name; can be change on save
        content = function(file) {
            write_csv(filtered_data(), file)

            # Log the download action
            log_action(action="Downloaded movie data")
        }
    )
}

shinyApp(ui = user_interface, server = server)