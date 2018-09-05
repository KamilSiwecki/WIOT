
library(shiny)
library(shinythemes)
kraje<-read.csv("C:/Users/Kamil/Desktop/vaio/aplikacja/dane.csv")
print(kraje<-as.vector(kraje[,1]))
print(kraje[1])
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme=shinytheme("darkly"),
  # Application title
  titlePanel("Zmiana udzialu wartosci dodanej-macierze przeplywow"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(


      sliderInput("incA",
                  "Number of incA:",
                  min = 0.0001,
                  max = 1,
                  value = 0.01),
      selectInput("kraj1",
                  "Kraj wysylajacy srodki",
                  as.list(setNames(c(1:44),kraje) ),selected = "4"),
      selectInput("kraj2",
                  "Kraj przyjmujacy srodki",
                  as.list(setNames(c(1:44),kraje) ),selected="7"),
      selectInput("lp",
                  "Wybierz ilosc przeplywow",
                  as.list(setNames(c(1:56),c(1:56))), selected = "3" )
    
    ),
    mainPanel(
      h1(verbatimTextOutput("print")),
      h2(verbatimTextOutput("print1")), width=12,
      plotOutput("distPlot"),
      plotOutput("plot")
    )
  )
))
