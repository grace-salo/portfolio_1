#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# import data
data <- read.csv("../bechdel_test_data.csv")
# year, imdb, title, test, clean_test, binary, budget, domgross, intgross, code, budget_2013., domgross_2013., intgross_2013., period.code, decade.code


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Bechdel Test Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput(inputId = "select1",
                        label = "Select Input Variables:",
                        choices = c("Year"="data$year", 
                                    "10-option Test"="test", "5-option Test"="clean_test", "Yes-No Test"="binary", "Budget"="budget", "Domestic Gross"="domgross", "International Gross"="intgross"))
            #choices = c("Year"="data$year", "imdb", "title", "test", "clean_test", "binary", "budget", "domgross", "intgross"))
            # dateRangeInput(inputId = "dates",
            #                label = "Select Date Range:",
            #                min = min(data$year),
            #                max = max(data$year))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "distPlot"),
            #textOutput(),
            #imageOutput(),
            plotOutput(outputId = "scatter"),
            plotOutput(outputId = "scatteruser")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- data$year
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(data$year, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # scatter plot: budget over time, colored by binary
    output$scatter <- renderPlot({
        ggplot(data, aes(x=year, y=budget, color=binary)) + geom_point() + geom_smooth(method=lm) + scale_color_brewer(palette="Dark2") + labs(title="Budget over Time", subtitle="Colored by Binary Test", x="Year", y="Budget (US Dollars)", color="Binary Test")
    })
    
    # scatter plot: controlled by usr
    output$scatteruser <- renderPlot({
        ggplot(data, aes(x=year, y=input$select1)) + geom_point()
    })
    
    #   renderDataTable(), renderImage()
    #output$image <- renderImage({
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
