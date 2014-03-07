make.deltas <- function(item.params, ...) {
	UseMethod("make.deltas")
}

make.deltas.character <- function(item.params,...) {
	return(make.deltas(CQmodel(show=item.params),...))
}

make.deltas.CQmodel <- function(item.params, item.table = "item", step.table = "item*step", ...) {
	#print("deltas")
	RMP <- item.params$RMP
	item.name <- item.table
	cross.parts <- unlist(strsplit(step.table, "\\*"))
	step.name <- cross.parts[cross.parts != item.name]
	item.table <- RMP[[item.table]]
	throlds = item.table$est
	step.table = RMP[[step.table]]

	if (step.name == "step") 
		step.col = "step"
	else step.col = paste("n", step.name, sep = "_")
	if (item.name == "step") 
		item.col = "step"
	else item.col = paste("n", item.name, sep = "_")
	
	steps <- reshape(step.table[c(item.col, step.col, "est")], direction = "wide", timevar = step.col, idvar = item.col)
	steps <- steps[colSums(!is.na(steps)) != 0]
	
	throlds <- make.deltas(throlds,steps)
	
	item.names <- unlist(item.table[item.name])
	step.names <- unique(unlist(step.table[step.name]))
	if (item.name == "step") 
			item.names <- item.names[item.names != 0]
	if (step.name == "step") 
			step.names <- step.names[step.names != 0]
	rownames(throlds) <- item.names
	colnames(throlds) <- step.names
	return(throlds)

}

make.deltas.default <- function(item.params, step.params, ...) {
	throlds <- item.params
	steps <- step.params
	
	item.nums <- 1:length(throlds)
	full.steps <- matrix(nrow = max(item.nums), ncol = ncol(steps))
	full.steps[,2] <- 0
	
	full.steps[item.nums %in% unlist(steps[1]),] <- as.matrix(steps)
	
	steps <- full.steps[,-1]
	throlds <- steps + throlds
	throlds <- throlds[rowSums(!is.na(throlds)) != 0, ]
	
	return(throlds)


}
