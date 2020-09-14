plotType <- function(type) {
  switch(type,
         search = area_search,
         tran = transects)
}

ui <- fluidPage(
wellPanel(
fluidRow(
  column(6, 
         radioButtons("a_method", "Survey method",
               c("Area search" = "search",
                 "transects" = "tran"))),
         column(6,
                radioButtons("b_method", "Survey method",
               c("Area search" = "search",
                 "transects" = "tran"))))),
wellPanel("main panel",
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("a_design"), plotOutput("b_design"))
          )))
          


server = function(input, output) {
  output$a_design <- renderPlot({ 
    plotType(input$a_method)})
    
    output$b_design <- renderPlot({ 
      plotType(input$b_method)})
}

shinyApp(ui, server)


