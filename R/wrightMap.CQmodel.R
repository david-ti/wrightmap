wrightMap.CQmodel <-
function(thetas, tables = NULL, label.items = NULL, main.title = NULL, thr.lab.text = NULL, dim.names = NULL, 
	...) {
		
	model <- thetas
	
	p.est <- model$p.est
	columns.at <- grep("^est", names(p.est), perl = TRUE)
	thetas <- p.est[columns.at]

	step.table <- NULL
	if (!is.null(model$GIN) && is.null(tables)) {
		if(class(model$GIN) == "matrix")
			throlds <- model$GIN
		else {
			names <- names(model$GIN)
			steps <- c(1:ncol(model$GIN[[1]]))
			if(length(steps) > 1)
				names <- c(outer(steps,names,paste))
			throlds <- do.call(cbind,model$GIN)
			colnames(throlds) <- names
			thr.lab.text <- as.data.frame(matrix(rep(names, each=nrow(throlds)), nrow = nrow(throlds)))
			#print(thr.lab.text)
		}
		#print(throlds)
		if (is.null(main.title)) 
			main.title <- "Wright Map (thresholds)"
		if (is.null(label.items)) 
			label.items <- row.names(throlds)
	} else {
		RMP <- model$RMP
		if (!is.null(tables)) {
			if (length(tables) == 1) {
				throlds = RMP[[tables]]$est
			} else {
				cross.name <- tables[grep("\\*", tables)]
				cross.parts <- unlist(strsplit(cross.name, "\\*"))
				item.name <- tables[tables %in% cross.parts][1]
				step.name <- cross.parts[cross.parts != item.name]
				throlds = RMP[[item.name]]$est
				step.table = RMP[[cross.name]]
			}
		} else {
			throlds <- RMP$item$est
			step.table <- RMP$"item*step"
			item.name <- "item"
			step.name <- "step"
		}


		if (!is.null(step.table)) {
			if(step.name=="step") 
				step.col = "step"
			else
				step.col = paste("n",step.name,sep="_")
			if(item.name=="step") 
				item.col = "step"
			else
				item.col = paste("n",item.name,sep="_")
			#print(item.col)
			#print(step.col)
			steps <- reshape(step.table[c(item.col, step.col, "est")], direction = "wide", timevar = step.col, 
				idvar = item.col)
			steps <- steps[colSums(!is.na(steps)) != 0]
			num_items = length(throlds)
			if(nrow(steps)!=num_items) {
				#print(steps)
				full.steps = as.data.frame(matrix(nrow=num_items-nrow(steps),ncol=ncol(steps)))
				item.nums <- 1:num_items
				full.steps[1] = item.nums[!item.nums %in% steps$n_item]
				full.steps[2] = 0
				#print(full.steps)
				names(full.steps) <- names(steps)
				steps <- rbind(steps,full.steps)
				steps <- steps[order(steps[1]),]
				#print(steps)
				
			}
			#print(steps)
			steps <- steps[-c(1)]
			throlds <- steps + throlds
			#print(throlds)
			throlds <- throlds[rowSums(!is.na(throlds)) != 0,]
			#print(throlds)
			#print(steps)
			#print(rowSums(!is.na(throlds)) != 0)
			if (is.null(main.title)) 
				main.title <- "Wright Map (Deltas)"
			if (is.null(thr.lab.text)) 
			#print(step.table)
			step.names <- unique(step.table[step.name])[[1]]
			if(step.name == "step")
				step.names <- step.names[step.names!=0]
				thr.lab.text <- as.data.frame(matrix(rep(step.names, each=nrow(throlds)), nrow = nrow(throlds)))
		} else {
			if (is.null(thr.lab.text)) 
				thr.lab.text <- ""
			if (is.null(main.title)) 
				main.title <- "Wright Map"
		}
		if (is.null(label.items)) 
			label.items <- model$run.details$names[[item.name]]
			if(item.name=="step")
			#print(label.items)
				label.items <- label.items[label.items!=0]
				#print(label.items)

	}

	if (is.null(dim.names)) 
		dim.names <- model$dimensions
	wrightMap(thetas, throlds, label.items = label.items, dim.names = dim.names, main.title = main.title, thr.lab.text = thr.lab.text, 
		...)
}
