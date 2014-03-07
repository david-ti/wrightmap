wrightMap.CQmodel <- function(thetas, tables = NULL, type = "default", label.items = NULL, main.title = NULL, thr.lab.text = NULL, dim.names = NULL, ...) {

	unpack.GIN <- function(GIN) {
		if (class(GIN) == "matrix") 
			return(GIN)
		else {
			return(do.call(cbind, lapply(GIN, unpack.GIN)))
		}
	}

	unpack.names <- function(GIN, sofar = "") {
		if (class(GIN) == "matrix") {
			my.names <- c(1:ncol(GIN))
		} else my.names <- names(GIN)


		if (length(my.names) == 1) {
			names <- sofar
		} else if (length(sofar) == 1) {
			names <- my.names
		} else names <- c(outer(my.names, sofar, paste))

		if (class(GIN) == "matrix") 
			return(names)
		else return(unpack.names(GIN[[1]], names))
	}

	model <- thetas

	p.est <- model$p.est
	columns.at <- grep("^est", names(p.est), perl = TRUE)
	thetas <- p.est[columns.at]


	if (!is.null(model$GIN) && is.null(tables)%% (type != "deltas")) {
		throlds <- unpack.GIN(model$GIN)
		names <- unpack.names(model$GIN)
		colnames(throlds) <- names
		

		if (is.null(main.title)) 
			main.title <- "Wright Map (thresholds)"
				message("Using GIN table for thresholds parameters")
	} else {
		RMP <- model$RMP
		if (!is.null(tables) && (length(tables) == 1)) {
			item.name = tables
		} else {
			if (!is.null(tables)) {
				step.table.name <- tables[grep("\\*", tables)]
				cross.parts <- unlist(strsplit(step.table.name, "\\*"))
				item.name <- tables[tables %in% cross.parts][1]
			} else {
				step.table.name <- "item*step"
				item.name <- "item"
			}
			if (!is.null(RMP[[step.table.name]])) {
				if (type != "thresholds") {
					throlds <- make.deltas(model, item.table = item.name, step.table = step.table.name)
					if (is.null(main.title)) 
						main.title <- "Wright Map (Deltas)"
					message("Using ", item.name, " and ", step.table.name, " tables to create delta parameters")
				} else {
					throlds <- make.thresholds(model, item.table = item.name, step.table = step.table.name)
					if (is.null(main.title)) 
						main.title <- "Wright Map (Thresholds)"
					message("Using ", item.name, " and ", step.table.name, " tables to create threshold parameters")
				}

				
			} else {
				throlds <- RMP[[item.name]]$est
			if (is.null(thr.lab.text)) 
				thr.lab.text <- ""
			if (is.null(main.title)) 
				main.title <- "Wright Map"
			message("Using ", item.name, " table for dichotomous item parameters")
			}
		}
		
		#print(label.items)
		
	}
	
	if (is.null(label.items)) {
			label.items <- rownames(throlds)
					}
	
	if (is.null(thr.lab.text)) {
		if(!is.null(colnames(throlds)))


				thr.lab.text <- as.data.frame(matrix(rep(colnames(throlds), each = nrow(throlds)), nrow = nrow(throlds)))
}

	if (is.null(dim.names)) 
		dim.names <- model$dimensions


	wrightMap(thetas, throlds, label.items = label.items, dim.names = dim.names, main.title = main.title, thr.lab.text = thr.lab.text, ...)
}
