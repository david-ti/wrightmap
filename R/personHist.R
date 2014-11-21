personHist <- function(thetas, yRange = NULL, breaks = "FD", dim.lab.cex = 0.6, dim.lab.side = 3, dim.lab.adj = 0.5, dim.names = NULL, dim.color = "white", axis.persons = "Respondents", oma = c(0, 5, 0, 5), axis.logits = "Logits", ...) {

	densExt <- function(densElem) {
		bin.size <- abs(densElem$breaks[1] - densElem$breaks[2])
		thetaHist <- data.frame(xleft = densElem$mids - (bin.size/2), ybottom = densElem$mids * 0, xright = densElem$mids + (bin.size/2), 
			ytop = densElem$counts)

		return(thetaHist)
	}

	theta.dens <- function(thetas, breaks) {
		densList <- apply(thetas, 2, hist, plot = FALSE, breaks = breaks)
		distInfo <- lapply(densList, densExt)
		return(distInfo)
	}

	person.plot <- function(distInfo, use.hist, yRange, dim.lab.side, dim.lab.cex, dim.lab.adj, p.cex.lab, p.font.lab, p.lwd) {
		#par(mar = c(0,0,0,0))
		
		#print(screen())
		
		par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))

		plot(c(min(distInfo[, 1]), max(distInfo[, 3])), c(min(distInfo[, 2]), max(distInfo[, 4])), ylim = yRange, xlim = c(max(distInfo[, 
			4]), 0), type = "n", ylab = "", xlab = "", axes = FALSE, cex.lab = p.cex.lab, font.lab = p.font.lab, lwd = p.lwd, col = attr(distInfo, 
			"dim.color"))

		rect(distInfo[, 4], distInfo[, 1], distInfo[, 2], distInfo[, 3], col = attr(distInfo, "dim.color"))
		mtext(attr(distInfo, "dim.name"), side = dim.lab.side, line = -1, cex = dim.lab.cex, font = 1, adj = dim.lab.adj)
		box(bty = "c")

		if (screen() != max(split.screen())) 
			screen(screen() + 1)
	}

	thetas <- as.matrix(thetas)
	nD <- ncol(thetas)

	if (is.null(yRange)) 
		yRange <- c(min(thetas), max(thetas))

	distInfo <- theta.dens(thetas, breaks)

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
	#print(first)
	#print(split.screen())

	op <- par("mar","oma")

	par(oma = oma)



	lapply(distInfo, FUN = person.plot, yRange = yRange, dim.lab.cex = dim.lab.cex, dim.lab.side = dim.lab.side, dim.lab.adj = dim.lab.adj, 
		p.cex.lab = 1.3, p.font.lab = 3, p.lwd = 2)
	if (!is.null(axis.logits)) {
		axis(4, las = 1, cex.axis = 0.6, font.axis = 2)
		mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)

	}
	screen(first)
	#print(distInfo)
	mtext(axis.persons, side = 2, line = 1, cex = 0.8, font = 3)

}
