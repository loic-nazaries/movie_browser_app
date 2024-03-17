# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

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
            hr(),

            checkboxGroupInput(
                inputId = "movie_type",
                label = "Select Movie Type(s):",
                choices = levels(factor_data$`Title Type`),
                selected = "Feature Film",
            ),
            # numericInput(
            #     inputId = "size",
            #     label = "Sample Size",
            #     value = 300,
            # ),

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
            verbatimTextOutput(outputId = "lmoutput"),
            br(),

            dataTableOutput(outputId = "movie_table"),
            # Display the download button if the 'show_data' box is ticked
            conditionalPanel(
                condition = "input.show_data == true",
                downloadButton(outputId = "download_data", label = "Download Data"),
            ),

        ),
    ),
)

server <- function(input, output, session) {
    filtered_data <- reactive({

        # If the check box is selected, display the data
        if (input$show_data) {

            # Check if points are selected in the scatter plot
            if (!is.null(input$plot_brush)) {

                # If points are selected, filter the data based on selected points
                brushed_data <- brushedPoints(df = movies, brush = input$plot_brush)
                return(brushed_data)
            }

            else {
                # If no points are selected, display the entire dataset
                return(movies)
            }
        }
    })

    output$scatterplot <- renderPlot({

        # Calculate correlation coefficient
        correlation <- numeric_data %>%
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
            data = movies,
        )

        # Plot the scatter plot
        ggplot(
            data = movies,
            mapping = aes_string(
                # Enclose input names in backticks when they contain a space
                x = paste0("`", input$select_x, "`"),
                y = paste0("`", input$select_y, "`"),
                color = paste0("`", input$colour, "`")
            )
        ) + 
            geom_point(alpha = input$alpha, size = input$size) +
            ggtitle(label = input$plot_title) +
            labs(color = input$colour) +

            # # Add linear regression line with confidence interval
            # geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +

            # Add text annotation for correlation coefficient
            geom_text(
                x = Inf, y = -Inf,
                label = paste("Correlation = ", correlation),
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
                    sprintf("%.2f", coef(lm_model)[1]), " + ",
                    sprintf("%.2f", coef(lm_model)[2]), " * ",
                    input$select_x
                ),
                hjust = 1,
                vjust = 1,
                color = "blue",
                size = 5,
            )
    })

    output$averages <- renderText({
        # Calculate the average values
        average_x <- movies %>% pull(input$select_x) %>% mean() %>% round(digits = 1)
        average_y <- movies %>% pull(input$select_y) %>% mean() %>% round(digits = 1)

        # Convert the calculated value to a string
        str_x <- paste("Average", input$select_x, "=", average_x)
        str_y <- paste("Average", input$select_y, "=", average_y)

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
    })

    # Print Linear Regression Summary
    output$lmoutput <- renderPrint({
        x <- movies %>% pull(input$select_x)
        y <- movies %>% pull(input$select_y)
        print(
            summary(lm(formula = y ~ x, data = movies)),
            digits = 3,
            signif.stars = TRUE,
        )
    })

    output$movie_table <- renderDataTable(
        {
            filtered_data()
        },
        caption = "Details of the Movie Dataset.",
    )

    # Download file
    output$download_data <- downloadHandler(
        filename = "movie_data.csv", # Default name; can be change on save
        content = function(file) {
            write_csv(filtered_data(), file)
        }
    )

    # Set initial value for plot title text input
    observe({
        updateTextInput(
            session = session,
            inputId = "plot_title",
            value = paste(input$select_y, "vs.", input$select_x)
        )
    })
}

shinyApp(ui = user_interface, server = server)