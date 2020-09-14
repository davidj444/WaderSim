
# Define UI
ui <- navbarPage(theme = shinytheme("flatly"), title = "WaderSim 1.0.1",
                tabPanel("About",
                         tags$h2("WaderSim: a tool for simulating the effectiveness of different survey designs"),
                         p("Find out more about this tool",
                           
                           tags$a(href="http://www.conservationecology.org/david-jarrett.html", "here.")),

"WaderSim is a tool to be used at the planning and design stage of surveys and research projects aimed at gathering data on wader productivity.
You can use WaderSim to investigate how species traits and survey design, 
timing, and effort influence the  
the reliability of productivity estimates."),
    

            
tabPanel("Species parameters",
         fluidRow(
             column(5, offset = 1, tags$h3("Species parameters A")),
             column(5, offset = 1, tags$h3("Species parameters B"))),
         
         fluidRow(
             column(5, offset = 1,
                    tabsetPanel(
                        tabPanel("Timing",
                                 renderInputsTIM("a")),
                        tabPanel("Detectability",
                                 renderInputsDET("a")),
                        tabPanel("Territoriality",
                                 renderInputsTER("a")),
                        tabPanel("Survival",
                                 renderInputsSVL("a")))),
                        
              column(5, offset = 1,
                    tabsetPanel(
                        tabPanel("Timing",
                                 renderInputsTIM("b")),
                        tabPanel("Detectability",
                             renderInputsDET("b")),
                        tabPanel("Territoriality",
                                 renderInputsTER("b")),
                        tabPanel("Survival",
                                 renderInputsSVL("b")))))),
         
###################################survey design################################################
tabPanel("Survey design",
             fluidRow(
             column(6, tags$h3("Survey design A")),
             column(6, tags$h3("Survey design B"))),
         
             fluidRow(
             column(6,
             tabsetPanel(
                        tabPanel("Survey schedule",
                        renderInputsB("a")),
                        
                        tabPanel("Survey design",
                        radioButtons("a_meth", "Route design A",
                        c("Area search - within 100m of every point" = "search",
                        "Parallel transects - 500m apart" = "tran")),
                        plotOutput("a_des")
                                 ))),
                            
                 column(6, 
                 tabsetPanel(
                             tabPanel("Survey schedule",
                             renderInputsB("b")),
                            
                     tabPanel("Survey design",
                    radioButtons("b_meth", "Route design B",
                    c("Area search - within 100m of every point" = "search",
                    "Parallel transects - 500m apart" = "tran")),
                    plotOutput("b_des")
                                     ))))),

 ################################example data############################################
                tabPanel("Example data",
                         h3("Example site data"),"Using the buttons below you can generate an example site survey for the species parameters and survey design you have selected. The date of the early and late survey visits will be randomly selected from the within the survey windows you have selected. If you are happy with your design, you should then proceed to the next tab and run a simulation.",
                         fluidRow(
                           column(6, actionButton("a_make_plot", "Generate example site survey for parameters A", icon("random"))),
                           column(6, actionButton("b_make_plot", "Generate example site survey for parameters B", icon("random")))),
                          fluidRow(
                             column(6,
                                    tabsetPanel(
                                      tabPanel("Early visit",
                                               verbatimTextOutput("a_early_date"),
                                          plotOutput("a_early_plot", width = "100%")),
                                      tabPanel("Late visit",
                                               verbatimTextOutput("a_late_date"),
                             plotOutput("a_late_plot", width = "100%")))),
                             column(6,
                                    tabsetPanel(
                                      tabPanel("Early visit",
                                               verbatimTextOutput("b_early_date"),
                                               plotOutput("b_early_plot", width = "100%")),
                                      tabPanel("Late visit",
                                               verbatimTextOutput("b_late_date"),
                                               plotOutput("b_late_plot", width = "100%"))))
                         )),
                
##################################simulate###########################################
                tabPanel("Simulate",
fluidRow(
  column(6,numericInput("a_sims", "Number of simulations to run with paramaters A:", 5, min = 1, max = 30),
       actionButton("a_run", "Run simulation A", icon("random"))),

column(6, numericInput("b_sims", "Number of simulations to run with parameters B:", 5, min = 1, max = 30),
actionButton("b_run", "Run simulation B", icon("random")))),



tabsetPanel(
    tabPanel("tables",
             fluidRow("comparison of estimated vs actual productivity"),
             fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), htmlOutput("a_tabs"), htmlOutput("b_tabs")))),
    tabPanel("Frequency distribution plots",
                fluidRow(tags$br(),"These plots show the distribution of productivity estimates generated by your survey design compared to actual productivity in the simulated system. If your survey design is appropriate, then the distribution of the productivity estimates will be close to the distribution of the actual productivity values. If the range of your estimated productivity values is wide, you may need to increase the number of sites you survey. If the estimated values are different from the actual values, you may need to adjust the timing of your surveys.",
             splitLayout(cellWidths = c("50%", "50%"), plotOutput("a_hist"), plotOutput("b_hist")))))),
                
#####################test page####################################################################
                tabPanel("test page"
                         )
                )

######################################### server###################################################
####if i want to return the actual date of the survey visit i have
####to generate this outside the main function
server<-function(input, output, session){

  output$a_early_date<-renderText({
    paste("!DATES ARE WRONG! The date of this survey visit was",
    as.character(input$a_Edates[1]))
    })
  
  output$a_late_date<-renderText({
    paste("The date of this survey visit was",
          as.character(input$a_Ldates[1]))
  })
  
  output$b_early_date<-renderText({
    paste("The date of this survey visit was",
          as.character(input$b_Edates[1]))
  })
  
  output$b_late_date<-renderText({
    paste("The date of this survey visit was",
          as.character(input$b_Ldates[1]))
  })
  
######################## visualisation of the survey method plots in 'survey design' tab##############################
  
  output$a_des <- renderPlot({ 
    plotType(input$a_meth)})
  
  output$b_des <- renderPlot({ 
    plotType(input$b_meth)})  
     
############################# select the variables used in the example plots###################################################
 ## example.outcomes<-function(disp.period,ter.dens, sep.dist,lay.date  inc.surv, ,  chick.surv,
 ##                            fledge.period, det.centre, det.second, det.third, inc.offset, Edate.st, Edate.end, Ldate.st, Ldate.end){

  plots.datA <- eventReactive(input$a_make_plot,{(   
  
example.outcomes(input$a_disp.period,input$a_ter.dens, input$a_sep.dist, yday(input$a_lay.date), input$a_inc.surv, input$a_inc.period, input$a_relay.prob, input$a_chick.surv,
                 input$a_fledge.period, input$a_det.centre, input$a_det.second, input$a_det.third, input$a_inc.offset,yday(input$a_Edates[1]),yday(input$a_Edates[2]),yday(input$a_Ldates[1]),yday(input$a_Ldates[2]))
  )})
 
  plots.datB <- eventReactive(input$b_make_plot,{( 
    
    example.outcomes(input$b_disp.period,input$b_ter.dens, input$b_sep.dist, yday(input$b_lay.date), input$b_inc.surv, input$b_inc.period, input$b_relay.prob, input$b_chick.surv,
                     input$b_fledge.period, input$b_det.centre, input$b_det.second, input$b_det.third, input$b_inc.offset,yday(input$b_Edates[1]),yday(input$b_Edates[2]),yday(input$b_Ldates[1]),yday(input$a_Ldates[2]))
    
    
  )})

 ##################################set up the example site plots ###################################################################### 
      
####################################a_early####################################    
output$a_early_plot <- renderPlot({
ggplot(data = plots.datA()) +
    tr.band1 + tr.band2 + tr.band3 + tr.labs + tr.scalex + tr.scaley + tr.line1 +tr.line2 +
    geom_sf(size = 4, shape = 21, aes(fill=action.early, colour=action.early, stroke = 2)) + 
    scale_colour_manual(labels = labs, values = cols, limits = limits) + 
    scale_fill_manual  (limits = limits, values = fills, breaks = breaks, labels = labels) +
    guides(colour=FALSE) + mytheme
}, height = 600, width = 450)

####################################a_late###############################################  
output$a_late_plot <- renderPlot({  
ggplot(data = plots.datA()) +
    tr.band1 + tr.band2 + tr.band3 + tr.labs + tr.scalex + tr.scaley + tr.line1 +tr.line2 +
    geom_sf(size = 4, shape = 21, aes(fill=action.late, colour=action.late, stroke = 2)) + 
    scale_colour_manual(labels = labs, values = cols,limits = limits) + 
    scale_fill_manual(limits = limits, values = fills, breaks = breaks, labels = labels) + 
    guides(colour=FALSE) + mytheme
 }, height = 600, width = 450) 

  
 #####################################b_early########################################################## 
output$b_early_plot <- renderPlot({
  ggplot(data = plots.datB()) +
    tr.band1 + tr.band2 + tr.band3 + tr.labs + tr.scalex + tr.scaley + tr.line1 +tr.line2 +
    geom_sf(size = 4, shape = 21, aes(fill=action.early, colour=action.early, stroke = 2)) + 
      
    scale_colour_manual(labels = labs, values = cols, limits = limits) + 
    scale_fill_manual  (limits = limits, values = fills, breaks = breaks, labels = labels) +
    guides(colour=FALSE) + mytheme
}, height = 600, width = 450)

########################################################b_late###############################################  
output$b_late_plot <- renderPlot({  
  ggplot(data = plots.datB()) +
    tr.band1 + tr.band2 + tr.band3 + tr.labs + tr.scalex + tr.scaley + tr.line1 +tr.line2 +
    geom_sf(size = 4, shape = 21, aes(fill=action.late, colour=action.late, stroke = 2)) + 
    scale_colour_manual(labels = labs, values = cols, limits = limits) + 
    scale_fill_manual(limits = limits, values = fills, breaks = breaks, labels = labels) + 
    guides(colour=FALSE) + mytheme
}, height = 600, width = 450) 
  
#################################run the simulation#############################################################
    
#   within eventReactive means it only updates when you click the/a button
    
#   # can rerun A with 'run_a' & rerun B with 'run_b', rerun both with 'run' 
    a_rerun <- eventReactive(input$a_run,{ 
      
      
      run.sims(input$a_sims,input$a_ter.dens,input$a_sep.dist,input$a_disp.period,yday(input$a_lay.date),input$a_inc.surv,input$a_inc.period,input$a_relay.prob,input$a_chick.surv,
                    input$a_fledge.period,input$a_det.centre,input$a_det.second,input$a_det.third,input$a_inc.offset,yday(input$a_Edates[1]),yday(input$a_Edates[2]),yday(input$a_Ldates[1]),yday(input$a_Ldates[2]),input$a_sites)
      
    })
    
    b_rerun <- eventReactive(input$b_run,{ 
      
      run.sims(input$b_sims,input$b_ter.dens,input$b_sep.dist,input$a_disp.period,yday(input$b_lay.date),input$b_inc.surv,input$b_inc.period,input$b_relay.prob,input$b_chick.surv,
               input$b_fledge.period,input$b_det.centre,input$b_det.second,input$b_det.third,input$b_inc.offset,yday(input$a_Edates[1]),yday(input$b_Edates[2]),yday(input$b_Ldates[1]),yday(input$b_Ldates[2]),input$b_sites)
    })   
    
  
  
##################################trying to make a reactive table

    tab.names <- c(
      'Iteration', 'Actual<br/>productivity', 'Estimated<br/>productivity', 
      'Pairs on<br/>early visit','Pairs on<br/>late visit')
 
output$a_tabs<-renderText({data=a_rerun()  %>% knitr::kable(format = "html",digits=2, col.names = tab.names,escape = FALSE) %>% kable_styling("striped")})
output$b_tabs<-renderText({data=b_rerun() %>% knitr::kable(format = "html", digits=2, col.names = tab.names,escape = FALSE) %>% kable_styling("striped")}) 

output$a_hist <- renderPlot({
  a_rerun() %>% .[c(1:3)] %>% gather(iteration) %>% 
    ggplot(aes(value, fill = iteration)) + geom_density(alpha = 0.2) +
    xlim(0,1)+xlab("Productivity estimate")+ylab("Frequency")+ theme_minimal()+theme(legend.title = element_blank(), legend.text = element_text(size = 13))+
    scale_fill_manual(values = c("est.prod" = "green","actual.prod" = "blue"),
      labels = c("est.prod" = "Estimated Productivity","actual.prod" = "Actual productivity"))
    })

output$b_hist <- renderPlot({
  b_rerun() %>% .[c(1:3)] %>% gather(iteration) %>% 
  ggplot(aes(value, fill = iteration)) + geom_density(alpha = 0.2) +
    xlim(0,1)+xlab("Productivity estimate")+ylab("Frequency")+theme_minimal()+theme(legend.title = element_blank(), legend.text = element_text(size = 13))+
    scale_fill_manual(values = c("est.prod" = "green","actual.prod" = "blue"),
                      labels = c("est.prod" = "Estimated Productivity","actual.prod" = "Actual productivity"))
})

}

# run the application
shinyApp(ui = ui, server = server)

# code for progress bar
# withProgress(message = 'hold on!', value = 0, {})