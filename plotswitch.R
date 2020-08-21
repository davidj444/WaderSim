plotType <- function(type) {
  switch(type,
         search = area_search,
         tran = transects)
}

ui <- 
  wellPanel(
    fluidRow(
  column(6, 
         radioButtons("a_method", "Survey method",
               c("Area search" = "search",
                 "transects" = "tran"))),
         column(6,
                radioButtons("b_method", "Survey method",
               c("Area search" = "search",
                 "transects" = "tran")))),
  fluidRow(
    column(3, plotOutput('a_design')),
    column(3, plotOutput('b_design'))))

server = function(input, output) {
  output$a_design <- renderPlot({ 
    plotType(input$a_method)})
    
    output$b_design <- renderPlot({ 
      plotType(input$b_method)})
}

shinyApp(ui, server)



