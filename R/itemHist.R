itemHist <- function(thr, yRange = NULL,axis.items = "Items",axis.logits = "Logits",oma = c(0,0,0,3),...) {

	
	Nbins <- function(x) {

		itemRange <- range(x)
		round((itemRange[2] - itemRange[1])/0.2, 0)

		return(seq(from = itemRange[1], to = itemRange[2], length.out = 25))

	}
	
	thr <- as.matrix(thr)

	nI <- dim(thr)[1]
	
	if(is.null(yRange))
		yRange <- c(min(thr),max(thr))

	item.hist <- hist(thr, plot = FALSE, breaks = Nbins(yRange))
	bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
	item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2), ybottom = item.hist$mids * 0, xright = item.hist$mids + (bin.size/2), 
		ytop = item.hist$counts)
	par(oma = oma)
	par(mgp = c(1, 0.2, 0))

	plot(c(min(item.hist[, 1]), max(item.hist[, 3])), c(min(item.hist[, 2]), max(item.hist[, 4])), ylim = yRange, xlim = c(0, max(item.hist[, 
		4])), type = "n", axes = FALSE, ylab = "", xlab = axis.items,cex.lab = .8,font.lab = 3)

	box(bty = "o")
	usr <- par("usr")
	par(mgp = c(3, 1, 0))
	axis(4, las = 1, cex.axis = 0.8, font.axis = 2)
	par(mgp = c(0, 0.2, 0))
	rect(item.hist[, 4], item.hist[, 1], item.hist[, 2], item.hist[, 3])
	
	mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
}
