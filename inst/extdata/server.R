library(shiny)


shinyServer(function(input, output,session) {
  
  
  
  model1 <- reactive({CQmodel(input$eap$datapath,input$shw$datapath,input$p.type)})
  main.title <- reactive({
  	if(input$autotitle)
  		return()
  	input$title
  })
  
  thr.sym.pch <- reactive({
  	if(is.null(wmap_bare()))
  		return()
  	symby <- input$sym_by
  	if(symby == "all")
  		return(as.integer(input$sym))
  	if(symby == "step") {
  		step_pch <- unlist(lapply(1:length(stepnames()), function(i) {
  			as.integer(input[[paste("sym",i,sep="_")]])
  		}))
  		return(rep(step_pch,each=length(itemnames())))
  	}
  	if(symby == "item") {
  		item_pch <- unlist(lapply(1:length(itemnames()), function(i) {
  			as.integer(input[[paste("sym",i,sep="_")]])
  		}))
  		return(item_pch)
  	}
  	return()
  })
  
    thr.sym.col <- reactive({
  	if(is.null(wmap_bare()))
  		return()
  	colby <- input$color_by
  	if(colby == "all")
  		return(input$col)
  	if(colby == "step") {
  		step_col <- unlist(lapply(1:length(stepnames()), function(i) {
  			input[[paste("col",i,sep="_")]]
  		}))
  		return(rep(step_col,each=length(itemnames())))
  	}
  	if(colby == "item") {
  		item_col <- unlist(lapply(1:length(itemnames()), function(i) {
  			input[[paste("col",i,sep="_")]]
  		}))
  		return(item_col)
  	}
  	return()
  })
  
 # output$bugprint <- renderPrint({
  	# thr.sym.pch()

  # })
  
  
  wmap <- reactive({
  	
  		  		
  	if(input$datatype == "CQ") {
	  	if(is.null(input$eap) || is.null(input$shw))
	  		return(NULL)
	  		
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
	  		
	    
	      return(wrightMap( model1(),item.table = item.table, interactions = interactions.table, step.table = step.table, throld = input$throld,item.type=input$which_type,main.title = main.title(),
	               show.thr.lab = input$show.thr.lab, use.hist = input$use.hist, axis.logits = input$axis.logits,
	               thr.sym.cex = input$cex,thr.sym.pch=thr.sym.pch(),thr.sym.col.bg=thr.sym.col()))
	 }
	 if(input$datatype == "R" && input$thetas != "" && input$thresholds!="") {
	 	#print(input$thetas)
	 	if(!exists(input$thetas,mode="numeric") || !exists(input$thresholds,mode="numeric"))
	 		return()
	 	if(input$slopes != "" && exists(input$slopes,mode="numeric"))
	 		slopes = get(input$slopes)
	 	else
	 		slopes = 1
	 	if(input$which_type == "deltas")
	 		throld = NULL
	 	else
	 		throld = input$throld
	 	wrightMap(get(input$thetas),get(input$thresholds),alpha = slopes,throld = throld,make.from = input$make_from,main.title=main.title(),show.thr.lab = input$show.thr.lab, use.hist = input$use.hist, axis.logits = input$axis.logits,
	               thr.sym.cex = input$cex,thr.sym.pch=thr.sym.pch(),thr.sym.col.bg=thr.sym.col(),axis.persons = input$axis.persons,axis.items = input$axis.items)
	 }

  	
  })
  
  wmap_bare <- renderPlot({
  	
  		  		
  	if(input$datatype == "CQ") {
	  	if(is.null(input$eap) || is.null(input$shw))
	  		return(NULL)
	  		
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
	  		
	    
	      return(wrightMap( model1(),item.table = item.table, interactions = interactions.table, step.table = step.table, item.type=input$which_type))
	 }
	 if(input$datatype == "R" && input$thetas != "" && input$thresholds!="") {
	 	#print(input$thetas)
	 	if(!exists(input$thetas,mode="numeric") || !exists(input$thresholds,mode="numeric"))
	 		return()
	 	
	 	wrightMap(get(input$thetas),get(input$thresholds))
	 }

  	
  })

  
  output$wmap <- renderPlot({
  	    wmap()
  })
  
  thetas.text <- reactive({
  	if(input$datatype == "CQ")
  		return(paste("\"",input$eap$name,"\"",sep=""))
  	if(input$datatype == "R")
  		return(input$thetas)
  	})
  	  thresholds.text <- reactive({
  	if(input$datatype == "CQ")
  		return(paste(",\"",input$shw$name,"\"",sep=""))
  	if(input$datatype == "R")
  		return(paste(",",input$thresholds,sep=""))
  	})
  	
  	make.from.text <- reactive({
  		if(input$datatype == "CQ")
  			return("")
  		if(input$datatype == "R")
  			return(paste(",make.from = ",input$make_from,sep=""))
  	})
  item.table.text <- reactive({
  	if(is.null(input$item.table) || input$item.table == "default")
  		return("")
  	return(paste(",item.table = ",input$item.table,sep=""))
  })
    interactions.text <- reactive({
  	if(is.null(input$interactions.table) || input$interactions.table == "default")
  		return("")
  	return(paste(",interactions = ",input$interactions.table,sep=""))
  })
    step.table.text <- reactive({
  	if(is.null(input$step.table) || input$step.table == "default")
  		return("")
  	return(paste(",step.table = ",input$step.table,sep=""))
  })
  type.text <- reactive({
  	if(input$which_type == "default")
  		return("")
  	return(paste(",item.type = \"",input$which_type,"\"",sep=""))
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
  thr.sym.pch.text <- reactive({
  	pchs <- thr.sym.pch()
  	if(is.null(wmap_bare()) || is.null(pchs) || pchs == 23)
  		return("")
  	return(paste(",thr.sym.pch =",list(pchs)))
  })
  
  thr.sym.col.text <- reactive({
  	cols <- thr.sym.col()
  	if(is.null(cols) || cols == rgb(0, 0, 0, 0.3))
  		return("")
  	return(paste(",thr.sym.col.bg =",list(cols)))
  })
  output$command <- renderPrint(cat("wrightMap(",thetas.text(),thresholds.text(),item.table.text(),interactions.text(),step.table.text(),make.from.text(),type.text(),throld.text(),use.hist.text(),main.title.text(),axis.logits.text(),show.thr.lab.text(),thr.sym.cex.text(),thr.sym.pch.text(),thr.sym.col.text(),")",sep=""))
  
  
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
  
  stepnames <- reactive({
  	dimnames(wmap_bare())[[2]]
  	})
  	
  	itemnames <- reactive({
  	dimnames(wmap_bare())[[1]]
  	})
  
  sym_choices <- c("Diamond" = 23,"Circle" = 21, "Square" = 22, "Triangle" = 24)
  
  
  output$sym_pickers <- renderUI({
  	symby <- input$sym_by
  	steps <- stepnames()
  	items <- itemnames()
  	if(symby == "all")
  		return(selectInput("sym","Choose symbol",choices = sym_choices))
  	else if(symby == "step") {
  			lapply(1:length(steps),function(i) {
  				selectInput(paste("sym",i,sep="_"),paste("Choose symbol for step",steps[i]),choices = sym_choices)
  			})
  		}
  	else if(symby == "item")
  		lapply(1:length(items),function(i) {
  				selectInput(paste("sym",i,sep="_"),paste("Choose symbol for item",items[i]),choices = sym_choices)
  			})
  })
    
  col_choices <- c("gray"= rgb(0, 0, 0, 0.3),"red"= rgb(1, 0, 0, 0.7),"green"= rgb(0, 1, 0, 0.7),"blue"= rgb(0, 0, 1, 0.7))
  
  output$color_pickers <- renderUI({
  	colby <- input$color_by
  	steps <- stepnames()
  	items <- itemnames()
  	if(colby == "all")
  		return(selectInput("col","Choose color",choices = col_choices))
  	else if(colby == "step") {
  			lapply(1:length(steps),function(i) {
  				selectInput(paste("col",i,sep="_"),paste("Choose color for step",steps[i]),choices = col_choices)
  			})
  		}
  	else if(colby == "item")
  		lapply(1:length(items),function(i) {
  				selectInput(paste("col",i,sep="_"),paste("Choose color for item",items[i]),choices = col_choices)
  			})
  })
  
  
  
})