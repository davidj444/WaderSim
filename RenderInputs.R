# need to add in incubation period and fledging period

renderInputsB <- function(prefix) {
  wellPanel(
    fluidRow(
             tags$h3(""),
             sliderInput(paste0(prefix, "_", "sites"), "How many sites will you survey:", min = 1, max = 80, value = 10),
             dateRangeInput(paste0(prefix, "_", "Edates"), "Early survey visit date range (day/month)", min = "2020-03-15", max = "2020-06-15", start = "2020-04-15", end = "2020-04-30", format = "yyyy/mm/dd"),
             dateRangeInput(paste0(prefix, "_", "Ldates"), "Late survey visit date range (day/month)", min = "2020-05-15", max = "2020-08-15", start = "2020-06-15", end = "2020-06-30", format = "yyyy/mm/dd")
      ))
    }

renderInputsTIM <- function(prefix) {
    fluidRow(
      sliderInput(paste0(prefix, "_", "disp.period"), "Display period", value = 21, min = 14, max = 28),
  dateInput(paste0(prefix, "_", "lay.date"), "Mean laying date (first clutches)", format = "yyyy-mm-dd", value = "2020-04-20", min = "2020-03-01", max = "2020-07-01"),
  sliderInput(paste0(prefix, "_", "inc.period"), "Mean incubation period", value = 25, min = 18, max = 29),
  sliderInput(paste0(prefix, "_", "fledge.period"), "Mean fledging period", value = 30, min = 20, max = 40))
  }

renderInputsDET <- function(prefix) {
  fluidRow(
    sliderInput(paste0(prefix, "_", "det.centre"), "Detectability within 25m of observer", step = 0.05, value = 0.9, min = 0, max = 1),
    sliderInput(paste0(prefix, "_", "det.second"), "Detectability 25-100m of observer", step = 0.05, value = 0.8, min = 0, max = 1),
    sliderInput(paste0(prefix, "_", "det.third"), "Detectability 100-250m of observer", step = 0.05, value = 0.7, min = 0, max = 1),
    sliderInput(paste0(prefix, "_", "inc.offset"), "Incubation period offset", step = 0.05, value = 0.8, min = 0, max = 1))
}

renderInputsTER <- function(prefix) {
  fluidRow(
    sliderInput(paste0(prefix, "_", "ter.dens"), "Density of territories per sq/km", value = 5, min = 0.5, max = 20),
    sliderInput(paste0(prefix, "_", "sep.dist"), "Min. distance between territories (m)", value = 30, min = 0, max = 250),
    sliderInput(paste0(prefix, "_", "chick.move"), "Est. mean daily movement of groups with chicks (m)", value = 30, min = 0, max = 250))
}

renderInputsSVL <- function(prefix) {
  fluidRow(
    sliderInput(paste0(prefix, "_", "inc.surv"), "Daily incubation survival rate",step = 0.001, value = 0.990, min = 0.85, max = 1),
    sliderInput(paste0(prefix, "_", "chick.surv"), "Daily chick survival rate", step = 0.001, value = 0.990, min = 0.85, max = 1),
    sliderInput(paste0(prefix, "_", "relay.prob"), "probability of relay following nest failure", value = 0.5, min = 0, max = 1))
}





