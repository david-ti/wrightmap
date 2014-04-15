library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Hello Shiny!"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    fileInput('eap', 'Choose Person Estimates File'),
    fileInput('shw', 'Choose Show File'),
    sliderInput("cex", 
                "cex:", 
                min = 1,
                max = 5, 
                value = 2.5, step = .1)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
  #textOutput("model"),
    plotOutput("wmap")
  )
))