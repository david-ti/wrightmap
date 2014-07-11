fitgraph.CQmodel <-
function(fitEst,table = NULL,type = "W",itemLabels =NULL,...) {
	if(is.null(table))
		table.num <- 1
	else
		table.num <- match(table,names(fitEst$RMP))
	table <- fitEst$RMP[[table.num]]
	estlabel <- paste(type,"fit",sep=".")
	llabel <- paste(type,"Low",sep=".")
	ulabel <- paste(type,"High",sep=".")
	
	if(is.null(itemLabels)) {
		colNames <- unlist(strsplit(names(fitEst$RMP)[table.num],"*",fixed = TRUE))
		itemLabels <- do.call(paste,table[colNames])
	}
	fitgraph(unlist(table[estlabel]),unlist(table[llabel]),unlist(table[ulabel]),itemLabels)
}
