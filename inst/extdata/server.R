library(shiny)


shinyServer(function(input, output,session) {
  
	observe({
    	if(input$selectedTab == "wmap") {
    		if(input$datatype == "R" && !is.null(input$c.params) && input$c.params != "" && exists(input$c.params) && any(get(input$c.params) != 0)) {
    			updateRadioButtons(session,"make_from",selected = "deltas")
    			panelChoices <- c("File options" = "files","Formatting options" = "format", "Text options" = "labels","Item labels" = "label.items","Person display options" = "person.disp","Symbol options" = "sym.disp","Item color options" = "color.disp")
    		}
    		else {
    			updateRadioButtons(session,"make_from",selected = "thresholds")
    			panelChoices <- c("File options" = "files","Data options" = "data","Formatting options" = "format","Text options" = "labels","Item labels" = "label.items","Person display options" = "person.disp","Symbol options" = "sym.disp","Item color options" = "color.disp")
    			}
    	}
    	else if(input$selectedTab == "fitgraph") {
    		if(input$datatype == "CQ")
    			panelChoices <- c("File options" = "files","Data options" = "data")
    		else if(input$datatype == "R")
    			panelChoices <- c("File options" = "files")
    		}
    	else if(input$selectedTab == "difplot") {
    		if(input$datatype == "CQ")
    			panelChoices <- c("File options" = "files","Data options" = "data")
    		else if(input$datatype == "R")
    			panelChoices <- c("File options" = "files")
    		}
		
		updateRadioButtons(session, "showPane", choices = panelChoices)
# 
#     # Can also set the label and select an item
#     updateRadioButtons(session, "inRadio2",
#       label = paste("Radio label", x),
#       choices = r_options,
#       selected = sprintf("option-%d-2", x)
#     )
   })

####  
  
  model1 <- reactive({CQmodel(p.est = input$eap$datapath,show = input$shw$datapath,input$p.type)
  	})
  	
  	model2 <- reactive({CQmodel(show = input$shw$datapath)})
  
  
  #######
  
  tables <- reactive({
  	#return(c("a","b"))
  	names(model2()$RMP)
  })
  
  item.tables <- reactive ({
  	if(is.null(input$shw))
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
  	if(is.null(input$shw))
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
  
  observe({
  	if(is.null(input$shw))
     	tables <- c("Please select files" = "none")
     else {
     		tables <- tables()
     		if(input$selectedTab == "difplot") {
     			interactions.at <- grepl("^[^\\*]+\\*[^\\*]+$", tables)
				tables <- tables[interactions.at]
				steps.at <- grepl("^step|step$",tables)
				tables <- tables[!steps.at]		
     		}
     }
     updateSelectInput(session, "pick.table",choices = tables,selected = tables[1])
  })
  
  observe({
  	if(is.null(input$shw))
  		types <- c("Please select files" = "none")
  	else {
  		types <- rev(unlist(strsplit(input$pick.table,"*",fixed = TRUE)))
  	}
  	updateSelectInput(session, "grouptype",choices = types,selected = types[1])
  })
  
  observe({
  	if(is.null(input$shw))
  		groups <- c("Please select files" = "none")
  	else {
  		table <- model2()$RMP[[input$pick.table]]
  		#message(head(table))
  		if(!is.null(table) && input$grouptype != "") {
  			#message(input$grouptype)
  			groups <- unique(table[[input$grouptype]])
  			}
  		else
  			groups <- c("Please select files" = "none")
  	}
  	updateSelectInput(session, "group",choices = groups,selected = groups[1])
  })
  
  #############
  
  stepnames <- reactive({
  	#return(dimnames(wmap_bare())[[2]])
  	#if(is.null(wmap_bare()))
  	#	return()
  	wm <- wmap_bare()
  	steps <- dimnames(wm)[[2]]
  	if(is.null(steps))
  		steps <- c(1:NCOL(wm))
  	return(steps)
  	})
  	
  	itemnames <- reactive({
  		wm <- wmap_bare()
  	items <- dimnames(wm)[[1]]
  	if(is.null(items))
  		items <- names(wm)
  	if(is.null(items))
  		items <- c(1:NROW(wm))
  	return(items)
  	})
  	
  	dim_names <- reactive({
  		if(input$datatype == "CQ") {
  			model <- model2()
  			return(model$dimensions)
  		}
  		if(input$datatype == "R") {
  			 if(input$thetas == "" || !exists(input$thetas))
  				 return()
  			return(paste("Dimension",1:NCOL(get(input$thetas))))
  		}
  		
  	})
  
  sym_choices <- c("Diamond" = 23,"Circle" = 21, "Square" = 22, "Triangle" = 24)
  
  
  output$sym_pickers <- renderUI({
  	symby <- input$sym_by
  	
  	
  	if(symby == "all")
  		return(selectInput("sym","Choose symbol",choices = sym_choices))
  	else if(symby == "step") {
  			steps <- stepnames()
  			lapply(1:length(steps),function(i) {
  				selectInput(paste("sym",i,sep="_"),paste("Choose symbol for step",steps[i]),choices = sym_choices)
  			})
  		}
  	else if(symby == "item") {
  		items <- itemnames()
  		lapply(1:length(items),function(i) {
  				selectInput(paste("sym",i,sep="_"),paste("Choose symbol for item",items[i]),choices = sym_choices)
  			})
  		}
  	else if(symby == "dim") {
  		dims <- dim_names()
  		lapply(1:length(dims),function(i) {
  				selectInput(paste("sym",i,sep="_"),paste("Choose symbol for",dims[i]),choices = sym_choices)
  			})
  		}
  })
    
  col_choices <- c("gray"= rgb(0, 0, 0, 0.3),"red"= rgb(1, 0, 0, 0.7),"green"= rgb(0, 1, 0, 0.7),"blue"= rgb(0, 0, 1, 0.7),"white","black")
  
  output$color_pickers <- renderUI({
  	colby <- input$color_by
  	steps <- stepnames()
  	items <- itemnames()
  	if(colby == "all")
  		return(selectInput("col","Choose color",choices = col_choices))
  	if(colby == "step") {
  			return(lapply(1:length(steps),function(i) {
  				selectInput(paste("col",i,sep="_"),paste("Choose color for step",steps[i]),choices = col_choices)
  			}))
  		}
  	if(colby == "item")
  		return(lapply(1:length(items),function(i) {
  				selectInput(paste("col",i,sep="_"),paste("Choose color for item",items[i]),choices = col_choices)
  			}))
  		return()
  })
  
  output$item.labels <- renderUI({
  	items <- itemnames()
  	lapply(1:length(items),function(i) {
  				textInput(paste("lab",i,sep="_"),paste("Choose label for item",items[i]))
  			})
  })
  
    # output$step.labels <- renderUI({
  	# steps <- stepnames()
  	# lapply(1:length(steps),function(i) {
  				# textInput(paste("steplab",i,sep="_"),paste("Choose label for step",steps[i]))
  			# })
  # })
  
  output$dim.labels <- renderUI({
  	dims <- dim_names()
  	lapply(1:length(dims),function(i) {
  				textInput(paste("dimlab",i,sep="_"),paste("Choose label for",dims[i]))
  			})
  	
  })
  
  output$dim.colors <- renderUI({
  	dims <- dim_names()
  	lapply(1:length(dims),function(i) {
  				selectInput(paste("dimcol",i,sep="_"),paste("Choose color for dim",dims[i]),choices = c("default",col_choices))
  			})
  })
  
  output$dim.items <- renderUI({
  	dims <- dim_names()
  	dnums <- 1:length(dims)
  	names(dnums) <- dims
  	items <- itemnames()
  	
  	lapply(1:length(items), function(i) {
  		selectInput(paste("itemdim",i,sep="_"),paste("Choose dimension for item",items[i]),choices = dnums)
  	})
  })
  
  
  item.dims <- reactive({
  	return(unlist(lapply(1:length(itemnames()), function(i) {
  			as.integer(input[[paste("itemdim",i,sep="_")]])
  		})))

  })
  
  #########

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
  	if(symby == "dim") {
  		dim_pch <- unlist(lapply(1:length(dim_names()), function(i) {
  			as.integer(input[[paste("sym",i,sep="_")]])
  		}))
  		return(dim_pch[item.dims()])
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
  	if(colby == "dim") {
  		return(dcols.items()[item.dims()])
  	}
  	return()
  })
  
  label.items <- reactive({
  	lab_i <- input$label_items
  	num_i <- length(itemnames())
  	if(lab_i == "default")
  		return(NULL)
  	if(lab_i == "num")
  		return(1:num_i)
  labels <- unlist(lapply(1:num_i, function(i) {
  			input[[paste("lab",i,sep="_")]]
  		}))
  	return(labels)
  })
  
  dim.names <- reactive({
  	dnames <- unlist(lapply(1:length(dim_names()), function(i) {
  			input[[paste("dimlab",i,sep="_")]]
  		}))
  	if(all(dnames == ""))
  		return()
  	return(dnames)
  })
  
  
  dimcols <- reactive({
  	return(unlist(lapply(1:length(dim_names()), function(i) {
  			input[[paste("dimcol",i,sep="_")]]
  		})))
  })
  
  dim.color <- reactive({
  	dcols <- dimcols()
  	if(input$use.hist)
  		dcols[dcols == "default"] <- "white"
  	else
  		dcols[dcols == "default"] <- "black"
  	return(dcols)
  })
  
  dcols.items <- reactive({
  	dcols <- dimcols()
  	dcols[dcols == "default"] <- rgb(0,0,0,0.3)
  	return(dcols)
  })
  
    minl <- reactive({
  	min.l <- as.numeric(input$minl)
  	if(is.na(min.l))
  		return()
  	return(min.l)
  })
  
    maxl <- reactive({
  	max.l <- as.numeric(input$maxl)
  	if(is.na(max.l))
  		return()
  	return(max.l)
  })
  
 # output$bugprint <- renderPrint({
  	# thr.sym.pch()

  # })
  
  ##########
  
  wm.args <- reactive({
  	
  	#on.exit(dev.off)	
  	args<- list()
  	if(input$datatype == "CQ") {
	  	if(is.null(input$eap) || is.null(input$shw))
	  		return()
	  	
	  	args <- c(args,"thetas" = list(model1()),"throld" = input$throld,"item.type" = input$which_type)
	  		
	  	if(!is.null(input$item.table) && input$item.table != "default") 
	  		args <- c(args,"item.table"= input$item.table)
	  		
	  	if(!is.null(input$step.table) && input$step.table != "default") 
	  		args <- c(args,"step.table"= input$step.table)
	  		
	  	if(!is.null(input$interactions.table) && input$interactions.table != "default") 
	  		args <- c(args,"interactions"= input$interactions.table)
	  	
	 }
	 else if(input$datatype == "R") {
	 	#cat("here")
	 	#print(input$thetas)
	 	if(input$thetas == "" || input$thresholds == "" || !exists(input$thetas) || !exists(input$thresholds) || (input$slopes != "" && !exists(input$slopes)))
	 		return()
	 	args <- 	c(args,"thetas" = list(get(input$thetas)), "thresholds" = list(get(input$thresholds)))
	 	
	 	if(input$slopes != "" && exists(input$slopes))
	 		args <- c(args,"alpha" = list(get(input$slopes)))
	 		
	 	if(input$c.params != "" && exists(input$c.params) && any(get(input$c.params) != 0))
	 		args <- c(args,"c.params"= list(get(input$c.params)))
	 	else
	 		args <- c(args,"make.from" = input$make_from)
	 		
	 	if(input$make_from == "thresholds" || input$which_type == "thresholds" || (input$which_type == "default" && input$throld != .5)) {
	 		#cat("throlds")
	 		args <- c(args,"throld" = input$throld)
	 		}
	 	
	 }
	 #message(args[4])
	 
	 args <- c(args,"main.title" = main.title(), "use.hist" = input$use.hist, "axis.logits" = input$axis.logits,"axis.persons" = input$axis.persons,"axis.items" = input$axis.items,"label.items" = list(label.items()),"label.items.rows" = input$label_items_rows,"label.items.srt" = input$label.items.srt,"label.items.ticks" = input$label.items.ticks, "show.thr.lab" = input$show.thr.lab,"show.thr.sym" = input$show.thr.sym,
	               "thr.sym.cex" = input$cex,"thr.sym.pch"=list(thr.sym.pch()),"thr.sym.col.bg"=list(thr.sym.col()),"dim.names" = list(dim.names()),dim.color = list(dim.color()),min.l = minl(),max.l = maxl(),item.prop = input$itemprop)
	  		
	    
	 #return(do.call(call,c("wrightMap",as.list(args))))
	 #return(list(args))
	 #return(do.call(wrightMap,args))
	 #message(class(args),"in")
	 return(args)

  	
  })
  
  
  ##########
  
  wmap_bare <- reactive({
  	
  	#on.exit(try(dev.off()))
  	if(input$datatype == "CQ") {
	  	if(is.null(input$shw))
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
	  		
	    
	      return(wrightMap( model2(),item.table = item.table, interactions = interactions.table, step.table = step.table, item.type=input$which_type))
	 }
	 if(input$datatype == "R" && input$thetas != "" && input$thresholds!="") {
	 	#print(input$thetas)
	 	if(!exists(input$thetas) || !exists(input$thresholds))
	 		return()
	 	
	 	wrightMap(get(input$thetas),get(input$thresholds))
	 }

  	
  })

  ##########
  output$wmap <- renderPlot({
  		#message(wm.call())
  		args <- wm.args()
  		if(is.null(args))
  			return()
  		est <- as.numeric(input$est)
  		if(!is.null(est) && !is.na(est)) {
  			SE <- as.numeric(input$err)
  			if(is.na(SE))
  				SE <- 0
  			args <- c(args,"est" = est,"SE" = SE)
  			return(do.call(kidMap,args))
  		}
  	    do.call(wrightMap,args)
  	    #wm.args()
  })
  
  #########
  func.text <- reactive({
  	est <- as.numeric(input$est)
  		if(!is.null(est) && !is.na(est)) {
  			return("kidMap(")
  			}
  	return("wrightMap(")
  })
  
  thetas.text <- reactive({
  	if(input$datatype == "CQ")
  		return(paste("\"",input$eap$name,"\",",sep=""))
  	if(input$datatype == "R")
  		return(paste0(input$thetas,","))
  	})
  	  thresholds.text <- reactive({
  	if(input$datatype == "CQ")
  		return(paste("\"",input$shw$name,"\"",sep=""))
  	if(input$datatype == "R")
  		return(input$thresholds)
  	})
  	
  	est.text <- reactive({
  		est <- as.numeric(input$est)
  		if(!is.null(est) && !is.na(est)) {
  			SE <- as.numeric(input$err)
  			if(is.na(SE))
  				SE <- 0
  			return(paste0(",est = ",est,",SE = ",SE))
  			}
  		return("")
  	})
  	
  	make.from.text <- reactive({
  		if(input$datatype == "CQ" || is.null(input$make_from) || input$make_from == "deltas" || input$throld == .5)
  			return("")
  		return(",make.from = \"thresholds\"")
  	})
  	
  	alphas.text <- reactive({
  		if(input$datatype == "R" && input$slopes != "")
  			return(paste(",alpha =",input$slopes))
  		return("")
  	})
  item.table.text <- reactive({
  	if(is.null(input$item.table) || input$item.table == "default")
  		return("")
  	return(paste(",item.table = \"",input$item.table,"\"",sep=""))
  })
    interactions.text <- reactive({
  	if(is.null(input$interactions.table) || input$interactions.table == "default")
  		return("")
  	return(paste(",interactions = \"",input$interactions.table,"\"",sep=""))
  })
    step.table.text <- reactive({
  	if(is.null(input$step.table) || input$step.table == "default")
  		return("")
  	return(paste(",step.table = \"",input$step.table,"\"",sep=""))
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
  	return(paste(",main.title = \"",input$title,"\"",sep=""))
  })
  use.hist.text <- reactive({
  	if(input$use.hist)
  		return("")
  	return(",use.hist = FALSE")
  })
  axis.logits.text <- reactive({
  	if(input$axis.logits == "Logits")
  		return("")
  	return(paste(",axis.logits = \"",input$axis.logits,"\"",sep=""))
  })
    axis.persons.text <- reactive({
  	if(input$axis.persons == "Respondents")
  		return("")
  	return(paste(",axis.persons = \"",input$axis.persons,"\"",sep=""))
  })
    axis.items.text <- reactive({
  	if(input$axis.items == "Items")
  		return("")
  	return(paste(",axis.items = \"",input$axis.items,"\"",sep=""))
  })
  
  label.items.text <- reactive({
  	labs <- label.items()
  	if(is.null(labs))
  		return("")
  	return(paste(",label.items =",list(labs)))
  })
  label.items.rows.text <- reactive({
  	if(input$label_items_rows == 1)
  		return("")
  	return(paste(",label.items.rows =",input$label_items_rows))
  })
  label.items.srt.text <- reactive({
  	if(input$label_items_rows > 1 || input$label.items.srt == 0)
  		return("")
  	return(paste(",label.items.srt =",input$label.items.srt))
  })
  label.items.ticks.text <- reactive({
  	if(input$label.items.ticks == formals(wrightMap.default)[["label.items.ticks"]])
  		return("")
  	return(paste(",label.items.ticks =",input$label.items.ticks))
  })
  show.thr.lab.text <- reactive({
  	if(input$show.thr.lab)
  		return("")
  	return(",show.thr.lab = FALSE")
  })
    show.thr.sym.text <- reactive({
  	if(input$show.thr.sym)
  		return("")
  	return(",show.thr.sym = FALSE")
  })
  thr.sym.cex.text <- reactive({
  	if(abs(input$cex-1.2) < .09)
  		return("")
  	return(paste(",thr.sym.cex =",input$cex))
  })
  thr.sym.pch.text <- reactive({
  	pchs <- thr.sym.pch()
  	#return()
  	if(length(pchs) == 0 || all(pchs == 23))
  			return("")
  	return(paste(",thr.sym.pch =",list(pchs)))
  })
  
  thr.sym.col.text <- reactive({
  	cols <- thr.sym.col()
  	if(is.null(cols) || all(cols == rgb(0, 0, 0, 0.3)))
  		return("")
  	if(length(cols) == 1)
  		cols <- paste("\"",cols,"\"",sep="")
  	return(paste(",thr.sym.col.bg =",list(cols)))
  })
  
  dim.names.text <- reactive({
  	dims <- dim.names()
  	if(is.null(dims))
  		return("")
  	if(length(dims) == 1)
  		dims <- paste("\"",dims,"\"",sep="")
  	return(paste(",dim.names =",list(dims)))
  })
  
  min.l.text <- reactive({
  	min.l <- minl()
  	if(is.null(min.l))
  		return("")
  	return(paste(",min.l =",min.l))
  })
  
    max.l.text <- reactive({
  	max.l <- maxl()
  	if(is.null(max.l))
  		return("")
  	return(paste(",max.l =",max.l))
  })
  
  item.prop.text <- reactive({
  	item.prop <- input$itemprop
  	if(item.prop == formals(wrightMap.default)[["item.prop"]])
  		return("")
  	return(paste(",item.prop =",item.prop))
  })
  
  output$wmap.command <- renderPrint(cat(func.text(),thetas.text(),thresholds.text(),est.text(),item.table.text(),interactions.text(),step.table.text(),make.from.text(),alphas.text(),type.text(),throld.text(),use.hist.text(),main.title.text(),axis.logits.text(),axis.persons.text(),axis.items.text(),label.items.text(),label.items.rows.text(),label.items.srt.text(),label.items.ticks.text(),show.thr.lab.text(),show.thr.sym.text(),thr.sym.cex.text(),thr.sym.pch.text(),thr.sym.col.text(),dim.names.text(),min.l.text(),max.l.text(),item.prop.text(),")",sep=""))
  
  ##########
  
 fitdata.text <- reactive({
 	if(input$datatype == "CQ")
 		return(paste("\"",input$shw$name,"\"",sep=""))
 	if(input$datatype == "R")
 		return(paste0(input$fitEst,",fitLB = ",input$fitLB,",fitUB = ",input$fitUB))
 })
  
  table.text <- reactive({
  	if(input$datatype == "R" || input$pick.table == "none")
  		return("")
  	return(paste0(",table = \"",input$pick.table,"\""))
  })
  
  fit.type.text <- reactive({
  	if(input$datatype == "R" || input$fit.type == formals(fitgraph.CQmodel)[["fit.type"]])
  		return("")
  	return(paste0(",fit.type = \"",input$fit.type,"\""))
  })
  

  
  output$fitplot.command <-
  renderPrint(cat("fitgraph(",fitdata.text(),table.text(),fit.type.text(),")",sep = ""))
  
  ##########
  
  difcomm.text <- reactive({
  	if(input$datatype == "CQ")
 		return("difplot(")
 	if(input$datatype == "R")
 		return("plotCI(")
  })
  
  difdata.text <- reactive({
 	if(input$datatype == "CQ")
 		return(paste("\"",input$shw$name,"\"",sep=""))
 	if(input$datatype == "R")
 		return(paste0(input$difEst,",errors = ",input$difErr))
 })
  
  grouptype.text <- reactive({
  	if(input$datatype == "R" || input$grouptype == "none")
  		return("")
  	return(paste0(",grouptype = \"",input$grouptype,"\""))
  })
  
  group.text <- reactive({
  	if(input$datatype == "R" || input$group == "none")
  		return("")
  	return(paste0(",group = \"",input$group,"\""))
  })
  
  output$difplot.command<-
  renderPrint(cat(difcomm.text(),difdata.text(),table.text(),grouptype.text(),group.text(),")",sep = ""))
  
  
  ################
    
  
  output$fitPlot.ui <- renderUI({
  	plotOutput("fitPlot",width = paste0(input$width,"%"))
  })
  
  output$fitPlot <- renderPlot({
  	if(input$datatype == "CQ") {
	  	if(is.null(input$shw))
	  		return("")
	  	if(input$pick.table == "none")
	  		fit.table <- NULL
	  	else
	  		fit.table <- input$pick.table
	  	fitgraph(model2(),table = fit.table,fit.type = input$fit.type)
	 }
	 else if(input$datatype == "R") {
	 	if(input$fitEst == "" || input$fitLB == "" || input$fitUB == "" || !exists(input$fitEst) || !exists(input$fitUB) || !exists(input$fitLB))
	 		return()
	 	else
	 		fitgraph(get(input$fitEst), get(input$fitLB), get(input$fitUB), itemLabels = "")
	 }
  })
  
  ##############
  
  output$difplot <- renderPlot({
  	if(input$datatype == "CQ") {
	  	if(is.null(input$shw))
		  		return()
		  model <- model2()
		  table.name <- input$pick.table
		  group <- input$group
		  grouptype <- input$grouptype
		  table <- model$RMP[[table.name]]
		  
		  RMPs <- names(model$RMP)
		  if(table.name %in% RMPs && grouptype %in% names(table) && group %in% table[[grouptype]])
		  	return(difplot(model,table = table.name,group = group,grouptype = grouptype))
	}
	else if(input$datatype == "R") {
		
		if(input$difEst != "" && input$difErr != "" && exists(input$difEst) && exists(input$difErr)) {
			#message(get(ests),get(errors))
			return(plotCI(get(input$difEst),get(input$difErr)))
			}
	}
	  return()
  })
  
  ##########
  

 
  
  
})