
ui <- fluidPage(
  radioButtons("dist", "Survey method:",
               c("Area seach" = "area_search",
                 "Transects" = "transects")),
  plotOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    hist(dist(500))
  })
}

shinyApp(ui, server)






ui = (fluidPage(

    sidebarPanel(
      radioButtons("col","Switch Plot",
                   choices = c("A", "B","C"),
                   selected = "A")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.col == 'A'", transects),
      conditionalPanel(
        condition = "input.col == 'B'", area_search)

    )
  )
)

server = function(input,output)
  
  shinyApp(ui, server)


