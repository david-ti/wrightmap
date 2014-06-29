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
  

  
  
  
  model1 <- reactive({CQmodel(input$eap$datapath,input$shw$datapath,input$p.type)})
  main.title <- reactive({
  	if(input$autotitle)
  		return(paste("Wright Map (",input$type,")",sep=""))
  	return(input$title)
  	
  })
  
  output$wmap <- renderPlot({
  	
  	if(is.null(input$eap) || is.null(input$shw))
  	return(NULL)
  	
  	
  	
    
      wrightMap( model1(),throld = input$throld,type=input$type,main.title = main.title(),
               show.thr.lab = input$show.thr.lab, use.hist = input$use.hist, axis.logits = input$axis.logits,
               thr.sym.cex = input$cex)
    
  })
  
  start.text <- reactive({paste("wrightMap(\"",input$eap$name,"\",\"",input$shw$name,"\"",sep="")})
  type.text <- reactive({paste(",type = \"",input$type,"\"",sep="")})
  throld.text <- reactive({
  	if(input$throld == .5)
  		return("")
  	return(paste(",throld = ",input$throld,sep=""))
  })
  main.title.text <- reactive({
  	if(input$autotitle)
  		return("")
  	return(paste(",main.title = ",input$title,sep=""))
  })
  use.hist.text <- reactive({
  	if(input$use.hist)
  		return("")
  	return(",use.hist = FALSE")
  })
  axis.logits.text <- reactive({
  	if(input$axis.logits == "Logits")
  		return("")
  	return(paste(",axis.logits =",input$axis.logits))
  })
  show.thr.lab.text <- reactive({
  	if(input$show.thr.lab)
  		return("")
  	return(",show.thr.lab = FALSE")
  })
  thr.sym.cex.text <- reactive({
  	if(abs(input$cex-1.2) < .09)
  		return("")
  	return(paste(",thr.sym.cex =",input$cex))
  })
  output$command <- renderPrint(cat(start.text(),type.text(),throld.text(),use.hist.text(),main.title.text(),axis.logits.text(),show.thr.lab.text(),thr.sym.cex.text(),")",sep=""))
})