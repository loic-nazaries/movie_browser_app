library(shiny)

ui <- fluidPage(
    
    # App title
    titlePanel(""),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
        ),
    ),
)

server <- function(input, output) {
    
}

shinyApp(ui, server)