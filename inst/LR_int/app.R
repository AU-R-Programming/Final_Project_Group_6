# Define UI for application that draws a plot of the approximate integral
ui <- fluidPage(

  # Application title
  titlePanel("Logistic Regression Estimates"),

  # Sidebar with text input for the function to integrate, numeric inputs for the range of integration and number of replications
  sidebarLayout(
    sidebarPanel(
      textInput("Data", "Data Frame Name:"),
      textInput("Y", "Y Column Name"),
      textInput("X", "X Column Name(s):"),
      numericInput("alpha", "significance level:", 0, min = 10^5, max = 1),
      numericInput("B", "Number of Bootstrap Iterations:", 10^5,
                   min = 100, max = 10^9),
      actionButton("button", "Compute Estimates")
    ),

    # Show a plot of the integrated area under the function
    mainPanel(
      plotOutput("betaPlot")
    )
  )
)

# Define server logic required to draw the integration plot
server <- function(input, output) {

  a <- eventReactive(input$button, {
    graphci(x = input$X, y = input$Y, data = input$Data, alpha = input$alpha, B = input$B)
  })

  output$betaPlot <- renderPlot({
    plot(a())
  })

}

# Run the application
shinyApp(ui = ui, server = server)
