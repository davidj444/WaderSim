ui <- fluidPage(
  radioButtons("method", "Survey method",
               c("Area search" = "search",
                 "transects" = "tran")),
  plotOutput("coverage")
)

server <- function(input, output) {
  output$coverage <- renderPlot({
    If ("method" == "search") {
      area_search
    } else {
      transects
    }
  })
}

shinyApp(ui, server)

If ("method" == "search") {
  area_search
} else {
  transects
}


