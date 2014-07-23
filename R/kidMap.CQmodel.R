kidMap.CQmodel <-
function(thetas, est, SE, item.table = NULL, interactions = NULL, step.table = NULL, label.items = NULL, main.title = NULL, 
	thr.lab.text = NULL, dim.names = NULL, ...) {

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
	
	if(!is.null(model$p.est)) {
	p.est <- model$p.est
	columns.at <- grep("^est", names(p.est), perl = TRUE)
	thetas <- p.est[columns.at]
	}
	else
		thetas <- 0


	if (!is.null(model$GIN) && is.null(item.table)) {
		#print("false")
		throlds <- unpack.GIN(model$GIN)
		names <- unpack.names(model$GIN)
		colnames(throlds) <- names


		if (is.null(main.title)) 
			main.title <- "Kid Map"
		#message("Using GIN table for threshold parameters")
	} else {
		RMP <- model$RMP

		
			throlds <- make.deltas(model, item.table = item.table, interactions = interactions, step.table = step.table)
				if (is.null(main.title)) 
					main.title <- "Kid Map"
				
		





		#print(label.items)
		
	}

	if (is.null(label.items)) {
		if(class(throlds) == "matrix")
		label.items <- rownames(throlds)
		else
		label.items <- names(throlds)
	}

	if (is.null(thr.lab.text)) {
		if (!is.null(colnames(throlds))) 


		thr.lab.text <- as.data.frame(matrix(rep(colnames(throlds), each = nrow(throlds)), nrow = nrow(throlds)))
	}

	if (is.null(dim.names)) 
		dim.names <- model$dimensions


	wrightMap(thetas, throlds, est, SE, label.items = label.items, dim.names = dim.names, main.title = main.title, thr.lab.text = thr.lab.text, ...)
}
