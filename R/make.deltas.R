make.deltas <- function(item.params, ...) {
	UseMethod("make.deltas")
}

make.deltas.character <- function(item.params, ...) {
	return(make.deltas(CQmodel(show = item.params), ...))
}

make.deltas.CQmodel <- function(item.params, item.table = NULL, interactions = NULL, step.table = NULL, ...) {
	#print("deltas")
	RMP <- item.params$RMP
	if (is.null(item.table)) {
		parts <- unique(unlist(item.params$parts))
		print(parts)
		if (length(parts) > 2) 
			stop("Please specify an item.table as well as the interactions and/or step.table")
		tables <- names(RMP)
		interactions.at <- grep("\\*", tables)
		interactions <- tables[interactions.at]
		single.tables <- tables[-interactions.at]
		item.table <- single.tables[1]
		if (length(single.tables) == 2) 
			step.table <- single.tables[2]
		else if (length(single.tables) > 2) 
			stop("Please specify tables")
	}
	
	item.name <- item.table
	item.table <- RMP[[item.table]]
	throlds = item.table$est
	throlds <- throlds[!is.na(throlds)]
	
	if (!is.null(step.table)) {
		step.name <- step.table
		step.table <- RMP[[step.table]]
		steps <- step.table$est
		steps <- steps[!is.na(steps)]
		}
	else {
		cross.parts <- unlist(strsplit(interactions, "\\*"))
		step.name <- cross.parts[cross.parts != item.name]
		steps <- 0
	}

	
	if (!is.null(interactions)) {
		interactions <- RMP[[interactions]]
		if (step.name == "step") 
			step.col = "step"
		else step.col = paste("n", step.name, sep = "_")
		if (item.name == "step") 
			item.col = "step"
		else item.col = paste("n", item.name, sep = "_")

		crosses <- reshape(interactions[c(item.col, step.col, "est")], direction = "wide", timevar = step.col, idvar = item.col)
		crosses <- crosses[colSums(!is.na(crosses)) != 0]
		crosses <- crosses[rowSums(!is.na(crosses)) > 1, ]
	}
	else {
		crosses <- 0
	}
	throlds <- make.deltas(throlds, crosses, steps)

	item.names <- unlist(item.table[item.name])
	if (!is.null(step.table)) 
		step.names <- unlist(step.table[step.name])
	else step.names <- unique(unlist(interactions[step.name]))
	if (item.name == "step") 
		item.names <- item.names[item.names != 0]
	if (step.name == "step") 
		step.names <- step.names[step.names != 0]
	rownames(throlds) <- item.names
	colnames(throlds) <- step.names
	return(throlds)

}

make.deltas.default <- function(item.params, cross.params, step.params = 0, ...) {
	#print(item.params)
	#print(step.params)
	throlds <- item.params
	crosses <- cross.params
	steps <- step.params
	#print(throlds)
	#print(crosses)
	#print(steps)
	num.items <- length(throlds)
	if(length(steps)> 1)
		steps <- matrix(steps, nrow = num.items, ncol = length(steps))

	if (length(crosses) >1) {
		item.nums <- 1:num.items
		full.steps <- matrix(nrow = max(item.nums), ncol = ncol(crosses))
		full.steps[, 2] <- 0

		full.steps[item.nums %in% unlist(crosses[1]), ] <- as.matrix(crosses)


		crosses <- full.steps[, -1]
	} else crosses <- 0

	throlds <- crosses + throlds + steps
	throlds <- throlds[rowSums(!is.na(throlds)) != 0, ]

	return(throlds)


}
