personDens <-
function(thetas, yRange = NULL, dim.lab.cex = 0.6, dim.lab.side = 3, dim.lab.adj = 0.5,dim.names = NULL,dim.color = "black",oma = c(0, 5, 0, 5), axis.logits = "Logits",show.axis.logits = TRUE, axis.persons = "Respondents",...) {

	densExt <- function(densElem) {
		xDim <- densElem["y"][[1]]
		yDim <- densElem["x"][[1]]
		xDim <- xDim/max(xDim)

		densInfo <- cbind(xDim, yDim)
		return(densInfo)
	}

	theta.dens <- function(thetas) {
		densList <- apply(thetas, 2, density, na.rm = TRUE)
		distInfo <- lapply(densList, densExt)

		return(distInfo)
	}

	person.plot <- function(distInfo, yRange, xRange, dim.lab.side, dim.lab.cex, dim.lab.adj, p.cex.lab, p.font.lab, p.lwd) {
		par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
		
		plot(distInfo, ylim = yRange, xlim = xRange, type = "l", axes = FALSE, ylab = "", xlab = "", cex.lab = p.cex.lab, font.lab = p.font.lab, lwd = p.lwd, col = attr(distInfo, "dim.color"))
		mtext(attr(distInfo, "dim.name"), side = dim.lab.side, line = -1, cex = dim.lab.cex, font = 1, adj = dim.lab.adj)
		box(bty = "c")
		
		if (screen() != max(split.screen())) 
			screen(screen() + 1)
	}
	
	thetas <- as.matrix(thetas)
	
	nD <- ncol(thetas)
	if(is.null(yRange))
		yRange <- c(min(thetas),max(thetas))

	xRange <- c(1, 0)
	distInfo <- theta.dens(thetas)
	
	if (is.null(dim.names)) {
		if (!is.null(names(thetas))) {
			dim.names <- names(thetas)
		} else dim.names <- c(paste("Dim", seq(1:nD), sep = ""))
	}
	
	if (ncol(thetas) > 1 & length(dim.color) == 1) {
		dim.color <- rep(dim.color, ncol(thetas))
	}
	
	for (i in 1:nD) {
		attr(distInfo[[i]], "dim.name") <- dim.names[i]
		attr(distInfo[[i]], "dim.color") <- dim.color[i]
	}
	
	split.screen(c(1, nD))
	first <- screen()

	op <- par("mar","oma")

	par(oma = oma)

		lapply(distInfo, FUN = person.plot, yRange = yRange, xRange = xRange, dim.lab.cex = dim.lab.cex, dim.lab.side = dim.lab.side, dim.lab.adj = dim.lab.adj, p.cex.lab = 1.3, p.font.lab = 3, p.lwd = 2)
		
		if (show.axis.logits) {
		axis(4, las = 1, cex.axis = 0.7, font.axis = 2)
	}
	mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
		
		screen(first)
	#print(distInfo)
	mtext(axis.persons, side = 2, line = 1, cex = 0.8, font = 3)

}
