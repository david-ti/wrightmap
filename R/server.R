library(shiny)


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  

  
  
  
  output$model <- renderPrint(CQmodel(input$eap$datapath,input$shw$datapath,"EAP"))
  output$wmap <- renderPlot({
  	
  	if(is.null(input$eap) || is.null(input$shw))
  	return(NULL)
  	
  	
  	
    
      wrightMap( CQmodel(input$eap$datapath,input$shw$datapath,"EAP"),throld = input$throld,type="thresholds",
               show.thr.lab = input$show.thr.lab, use.hist = input$use.hist,
               thr.sym.cex = input$cex)
    
  })
})