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
  
  
  output$wmap <- renderPlot({
  	
  	if(is.null(input$eap) || is.null(input$shw))
  		return(NULL)
  		
  	if(input$autotitle)
  		main.title <- NULL
  	else
  		main.title <- input$title

  	
  	if(is.null(input$item.table) || input$item.table == "default") 
  		item.table <- NULL
  	else
  		item.table <- input$item.table
  		
  	if(is.null(input$step.table) || input$step.table == "default") 
  		step.table <- NULL
  	else
  		step.table <- input$step.table
  		
  	if(is.null(input$interactions.table) || input$interactions.table == "default") 
  		interactions.table <- NULL
  	else
  		interactions.table <- input$interactions.table
  		
    
      wrightMap( model1(),item.table = item.table, interactions = interactions.table, step.table = step.table, throld = input$throld,type=input$type,main.title = main.title,
               show.thr.lab = input$show.thr.lab, use.hist = input$use.hist, axis.logits = input$axis.logits,
               thr.sym.cex = input$cex)
    
  })
  
  start.text <- reactive({paste("wrightMap(\"",input$eap$name,"\",\"",input$shw$name,"\"",sep="")})
  item.table.text <- reactive({
  	if(is.null(input$item.table) || input$item.table == "default")
  		return("")
  	return(paste(",item.table = ",input$item.table,sep=""))
  })
    interactions.text <- reactive({
  	if(is.null(input$interactions) || input$interactions.table == "default")
  		return("")
  	return(paste(",interactions = ",input$interactions.table,sep=""))
  })
    step.table.text <- reactive({
  	if(is.null(input$step.table) || input$step.table == "default")
  		return("")
  	return(paste(",step.table = ",input$step.table,sep=""))
  })
  type.text <- reactive({
  	if(input$type == "default")
  		return("")
  	return(paste(",type = \"",input$type,"\"",sep=""))
  	})
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
  output$command <- renderPrint(cat(start.text(),item.table.text(),interactions.text(),step.table.text(),type.text(),throld.text(),use.hist.text(),main.title.text(),axis.logits.text(),show.thr.lab.text(),thr.sym.cex.text(),")",sep=""))
  
  tables <- reactive({
  	names(model1()$RMP)
  })
  
  item.tables <- reactive ({
  	if(is.null(input$eap) || is.null(input$shw))
      return()
  	tables.list <- tables()
	interactions.at <- grep("\\*", tables.list)
	if(length(interactions.at) > 0)
		return(tables.list[-interactions.at])
	return(tables.list)
  })
  
  step.tables <- reactive ({
  	if(is.null(input$item.table))
  		return()
  	if(input$item.table == "default")
  		return(item.tables())
  	return(item.tables()[item.tables()!=input$item.table])
  })
  
  interactions <- reactive({
  	if(is.null(input$eap) || is.null(input$shw))
      return()
  	tables.list <- tables()
	interactions.at <- grepl("^[^\\*]+\\*[^\\*]+$", tables.list)
	if(!is.null(input$item.table)) {
		if(input$item.table != "default") {
			interactions.at <- interactions.at & grepl(input$item.table,tables.list)
		}
		if(input$step.table !="default") {
			interactions.at <- interactions.at & grepl(input$step.table,tables.list)
		}
	}
	return(tables.list[interactions.at])

  })
  
    output$item.table <- renderUI({
 
    selectInput("item.table", "Choose item table", 
                        choices  = c("default",item.tables()),
                        selected = "default")
  })
  
      output$step.table <- renderUI({
    
    selectInput("step.table", "Choose step table", 
                        choices  = c("default",step.tables()),
                        selected = "default")
  })
  
      output$interactions.table <- renderUI({
        # Create the checkboxes and select them all by default
    selectInput("interactions.table", "Choose interactions table", 
                        choices  = c("default",interactions()),
                        selected = "default")
  })
  
  
})