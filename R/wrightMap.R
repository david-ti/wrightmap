wrightMap <- function(thetas, thresholds = NULL, item.side = itemModern, person.side = personHist, throld = NULL, design.matrix = "normal", make.from = "deltas", alpha = 1, c.params = 0, main.title = "Wright Map", min.logit.pad = 0.25, max.logit.pad = 0.25, min.l = NULL, max.l = NULL, item.prop = 0.8, return.thresholds = TRUE, new.quartz = FALSE, ...) {

	## Helper Functions


	thetas <- personData(thetas)
	if(is.null(thresholds))
		thresholds <- thetas
	thresholds <- itemData(thresholds,...)
	
	if (!is.null(throld)) {
		thresholds <- make.thresholds(thresholds, design.matrix = design.matrix, throld = throld, alpha = alpha, make.from = make.from, c.params = c.params)
	} else if (any(c.params != 0)) {
		thresholds <- make.thresholds(thresholds, design.matrix = design.matrix, throld = 0.5, alpha = alpha, make.from = make.from, c.params = c.params)
	}
	
	# Setting plot parameters
	thresholds <- as.matrix(thresholds)
	
	nD <- ncol(thetas)
	
	min.theta <- quantile(thetas, probs = c(0.01), na.rm = TRUE)
	max.theta <- quantile(thetas, probs = c(0.99), na.rm = TRUE)

	if (is.null(min.l)) {
		min.l <- min(c(min.theta, thresholds), na.rm = TRUE) - min.logit.pad
	}

	if (is.null(max.l)) {
		max.l <- max(c(max.theta, thresholds), na.rm = TRUE) + max.logit.pad
	}

	yRange <- c(min.l, max.l)
	
	# Generating Full Map
	
	if (new.quartz) 
		dev.new(width = 9, height = 5)
	
	op <- par("oma", "mar", "mgp")
	
	#par(oma = op$oma + c(0, 5, 0, 5))
	par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
	#par(mgp = c(op$mar[1] - 2.4, 1, 0))
	close.screen(all.screens = TRUE)
	par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
	left.marg <- 0.05
	right.marg <- .1
	plots <- 1 - (left.marg + right.marg)
	divider <- plots * (1 - item.prop) + left.marg
	person.screen <- c(left.marg,divider,0,1)
	item.screen <- c(divider,1 - right.marg,0,1)
	screens <- matrix(c(item.screen,person.screen),ncol = 4,byrow = TRUE)
	
	split.screen(screens)
	
	
	do.call(item.side,list(thresholds,yRange,oma = c(0,0,0,0),...))
	close.screen(1)

	do.call(person.side,list(thetas,yRange,oma = c(0,0,0,0),axis.logits = NULL,...))
	
	par(oma = c(0, 0, 3, 0))
	mtext(main.title, side = 3, line = 1, outer = TRUE, font = 2)
	par(op)
	if (return.thresholds) {
		return(thresholds)
	}
	
	close.screen(all.screens = TRUE)

}
