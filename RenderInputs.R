renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_", "inc.surv"), "Daily incubation survival rate",step = 0.001, value = 0.990, min = 0.85, max = 1),
             sliderInput(paste0(prefix, "_", "chick.surv"), "Daily chick survival rate", step = 0.001, value = 0.990, min = 0.85, max = 1),
             sliderInput(paste0(prefix, "_", "relay.prob"), "probability of relay following nest failure", value = 0.5, min = 0, max = 1),
             sliderInput(paste0(prefix, "_", "ter.dens"), "Density of territories per sq/km", value = 5, min = 0.5, max = 20),
             sliderInput(paste0(prefix, "_", "lay.date"), "Mean laying date (day of year)", value = 100, min = 50, max = 150)
             
      ),
      column(6,
             sliderInput(paste0(prefix, "_", "band1"), "Detectability within 25m of observer", step = 0.05, value = 0.9, min = 0, max = 1),
             sliderInput(paste0(prefix, "_", "band2"), "Detectability 25-100m of observer", step = 0.05, value = 0.8, min = 0, max = 1),
             sliderInput(paste0(prefix, "_", "band3"), "Detectability 100-250m of observer", step = 0.05, value = 0.7, min = 0, max = 1),
             sliderInput(paste0(prefix, "_", "sep.dist"), "Min. distance between territories (m)", value = 30, min = 0, max = 250),
             sliderInput(paste0(prefix, "_", "chick.move"), "Mean daily movement of groups with chicks (m)", value = 30, min = 0, max = 250)
      )
    ))
}

renderInputsB <- function(prefix) {
  wellPanel(
    fluidRow(
             tags$h3("Survey design"),
             sliderInput(paste0(prefix, "_", "sites"), "How many sites will you survey:", min = 1, max = 80, value = 10),
             dateRangeInput("dates", label = h3("Early survey visit date range")),
             dateRangeInput("dates", label = h3("Late survey visit date range")),
             radioButtons(paste0(prefix, "_", "method"), label = "Select survey method", c("Parallel transects (500m apart)" = "tran","Area search" = "search"))
      ))
}

