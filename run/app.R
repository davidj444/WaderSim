#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
renderInputs <- function(prefix) {
    wellPanel(
        fluidRow(
            column(6,
                   sliderInput(paste0(prefix, "_", "inc.surv"), "Daily incubation survival rate",step = 0.001, value = 0.990, min = 0.85, max = 1),
                   sliderInput(paste0(prefix, "_", "chick.surv"), "Daily chick survival rate", step = 0.001, value = 0.990, min = 0.85, max = 1),
                   sliderInput(paste0(prefix, "_", "relay.prob"), "probability of relay following nest failure", value = 0.5, min = 0, max = 1),
                   sliderInput(paste0(prefix, "_", "ter.dens"), "Density of territories per sq/km", value = 5, min = 0.5, max = 20),
                   actionButton(paste0(prefix,"_","recalc"), "Update", icon("random")),
                   numericInput("sims", "Simulate outcomes:", 100, min = 20, max = 2000)
            ),
            column(6,
                   sliderInput(paste0(prefix, "_", "band1"), "Detectability within 25m of observer", step = 0.05, value = 0.9, min = 0, max = 1),
                   sliderInput(paste0(prefix, "_", "band2"), "Detectability 25-100m of observer", step = 0.05, value = 0.8, min = 0, max = 1),
                   sliderInput(paste0(prefix, "_", "band3"), "Detectability 100-250m of observer", step = 0.05, value = 0.7, min = 0, max = 1),
                   sliderInput(paste0(prefix, "_", "sep.dist"), "Min. distance between territories (m)", value = 30, min = 0, max = 250)
            )
        ))
}


# Define UI
ui<- navbarPage(title = "WaderSim 1.0.1",
                tabPanel("How it works",
                         tags$h2("WaderSim: a tool for simulating the effectiveness of different survey designs"),
                         p("Find out more about this tool",
                           tags$a(href="http://www.conservationecology.org/david-jarrett.html", "here.")),
                         "WaderSim is designed to help at the planning stage of projects designed to gather data on the productivity of breeding waders. It can be used to compare the effectiveness of different survey designs, and to assess the statistical power of different approaches."),
                tabPanel("Species parameters",
                         
                         fluidPage(theme="simplex.min.css",
                                   tags$style(type="text/css",
                                              "label {font-size: 12px;}",
                                              ".recalculating {opacity: 1.0;}"
                                   ),
                                   
                                   
                                   fluidRow(
                                       column(6, tags$h3("Species A parameters")),
                                       column(6, tags$h3("Species B parameters"))
                                   ),
                                   fluidRow(
                                       column(6, renderInputs("a")),
                                       column(6, renderInputs("b"))
                                   ),
                                   fluidRow(
                                       column(6,
                                              "Example simulation in a 4km2 study site.
                    Productive territories = 75%",
                                              plotOutput("a_distPlot")
                                              
                                       ),
                                       column(6,
                                              "Example simulation in a 4km2 study site.
                   Productive territories = 58%",
                                              plotOutput("b_distPlot")
                                       )
                                   ))),
                
                tabPanel("Survey design","Survey design",
                         wellPanel(
                             fluidRow(
                                 column(3,
                                        radioButtons(inputId = "method",label = "Select survey method", c("Parallel transects (500m apart)" = "tran","Area search" = "search"))),
                                 column(3,
                                        sliderInput("sites", "How many sites will you survey (max 100):", min = 1, max = 80, value = 10)),
                                 column(3,
                                        sliderInput("n_sim", "Number of simulations:", min = 1, max = 200, value = 20)),
                                 column(3,
                                        actionButton("recalc", "Run simulation", icon("random")))
                             ))
                         
                         
                ))

####this is the stuff to make it reactive


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