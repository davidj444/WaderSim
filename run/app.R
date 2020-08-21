# Define UI
ui <- navbarPage(title = "WaderSim 1.0.1",
                tabPanel("About",
                         tags$h2("WaderSim: a tool for simulating the effectiveness of different survey designs"),
                         p("Find out more about this tool",
                           
                           tags$a(href="http://www.conservationecology.org/david-jarrett.html", "here.")),

"WaderSim is a tool to be used at the planning stage of breeing wader productivity projects.
You can use WaderSim to investigate how survey design, 
survey timing and species traits can influence the  
the effectiveness of productivity estimates."),
                
tabPanel("Specify parameters",
         fluidPage(theme="simplex.min.css",
                   tags$style(type="text/css",
                              "label {font-size: 12px;}",
                              ".recalculating {opacity: 1.0;}")),
         
         fluidRow(
             column(6, tags$h3("Species parameters A")),
             column(6, tags$h3("Species parameters B"))),
         
         fluidRow(
             column(6, renderInputs("a")),
             column(6, renderInputs("b"))),
         
         fluidRow(
             column(6, renderInputsB("a")),
             column(6, renderInputsB("b")))),  

                tabPanel("Example plots",
                         wellPanel(
                             column(6,
                             plotOutput("a_distPlot")),
                             column(6,          
                             plotOutput("b_distPlot")))    
                         ),
                
                tabPanel("Simulate",
"Here there will be a button that says number of sims to run:
There will be two panels at the top showing the comparison between est. productivity
and actual productivity in the two situations.
                
Then a 'run comparison' button parameter set 'A' and parameter set 'B' which plots the
estimates from parameter set A / B and ",
                        
numericInput("sims", "Simulate outcomes:", 100, min = 20, max = 2000),

       actionButton("recalc", "Run simulation", icon("random"))
),
                
                tabPanel("Code",
                         "there will be an rmarkdown file here"
                         )
                )

# server

server<-function(input, output, session){
    
    paramNames <- c("inc.surv", "chick.surv", "relay.prob",
                    "ter.dens","sep.dist")
    
    getParams <- function(prefix) {
        input[[paste0(prefix, "_recalc")]]
        
        params <- lapply(paramNames, function(p) {
            input[[paste0(prefix, "_", p)]]
        })
        names(params) <- paramNames
        params
    }
    
    # specify the correct parameters are used in the data for each plot
    plotA <- reactive(do.call(get.outcomes, getParams("a")))
    plotB <- reactive(do.call(get.outcomes, getParams("b")))
    
    
#  set up the plots
    output$a_distPlot <- renderPlot({
        p1<-ggplot(data = plotA()) +
            geom_sf(size = 3, shape = 19, aes(colour=nest_status))+ 
            coord_sf(xlim = c(0, 2000), ylim = c(0, 2000))
        p1+facet_wrap(~week)+ scale_color_manual(labels = c("Attempt abandoned","Chicks","Display","Nest failed","Fledged","Incubate"), values = c("c" = "#339900", "d" = "blue", "f"= "red","fl"="black","i"="green","a"="grey82"))+theme_minimal()
    })
    output$b_distPlot <- renderPlot({
        p1<-ggplot(data = plotB()) +
            geom_sf(size = 3, shape = 19, aes(colour=nest_status))+ 
            coord_sf(xlim = c(0, 2000), ylim = c(0, 2000))
        p1+facet_wrap(~week)+ scale_color_manual(labels = c("Attempt abandoned","Chicks","Display","Nest failed","Fledged","Incubate"), values = c("c" = "#339900", "d" = "blue", "f"= "red","fl"="black","i"="green","a"="grey82"))+theme_minimal()
    }) 

}

# run the application
shinyApp(ui = ui, server = server)