library(shiny)

ui <- fluidPage(
  titlePanel("Hypothesis Testing for Mean in 1 Population"),
  sidebarLayout(
    sidebarPanel(
        numericInput("mu", "Population Mean:", value = 0),
        numericInput("mean", "Sample Mean:", value = 0),
        numericInput("n_sample", "Sample Size:", value = 1, min = 1),
        numericInput("sd", "Sample Standard Deviation:", value = 1, min = 0.01),
        numericInput(
            "alpha",  
            "Significance Level (alpha):", 
            value = 0.05, 
            min = 0.01, 
            max = 0.1
        ),
        actionButton("update", "Update Plot")
    ),
    mainPanel(
        plotOutput("plot"),
        textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui = ui, server = server)