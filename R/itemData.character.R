itemData.character <- function(thresholds, p.type = NULL,...) {
	model <- CQmodel(show = thresholds, p.type = p.type)
	return(itemData(model,...))
}