# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(DT)

# Import helper Functions
source("helpers.R")

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
                    log_input(input_id = input_id, input_value = input_value)
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
    })

    # Filter table based on selected (brushed) points with the mouse
    # (all points selected by default)
    filtered_data <- reactive({
        filter_data(input = input, dataframe = movie_subset())
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
            log_action(action = "Downloaded movie data")
        }
    )
}

shinyApp(ui = user_interface, server = server)