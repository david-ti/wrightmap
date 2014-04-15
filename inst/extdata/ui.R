library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Hello Shiny!"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    fileInput('eap', 'Choose Person Estimates File'),
    fileInput('shw', 'Choose Show File'),
    sliderInput("throld","Threshold",min=.01,max = .99, value = .5, step = .01),
    checkboxInput('use.hist', 'Histogram?', TRUE),
    checkboxInput('show.thr.lab', 'Show Threshold Labels', FALSE),
    sliderInput("cex", 
                "Symbol size", 
                min = 1,
                max = 5, 
                value = 2.5, step = .1)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
  #textOutput("model"),
    plotOutput("wmap"),
    verbatimTextOutput("command")
  )
))