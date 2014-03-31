fitgraph.CQmodel <-
function(fitEst,table = NULL,type = "W",itemLabels =NULL,...) {
	if(is.null(table)) {
		table <- fitEst$RMP[[1]]
	}
	else {
		table <- fitEst$RMP[[table]]
	}
	estlabel <- paste(type,"fit",sep=".")
	llabel <- paste(type,"Low",sep=".")
	ulabel <- paste(type,"High",sep=".")
	
	if(is.null(itemLabels))
		itemLabels <- unlist(table$item)
	
	fitgraph(unlist(table[estlabel]),unlist(table[llabel]),unlist(table[ulabel]),itemLabels)
}
