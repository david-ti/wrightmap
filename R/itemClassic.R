itemClassic <- function(thr, yRange = NULL, axis.items = "Items",axis.logits = "Logits",oma = c(0,0,0,3),...) {
	Nbins <- function(x) {

		itemRange <- range(x)
		round((itemRange[2] - itemRange[1])/0.2, 0)

		# return(seq(from = itemRange[1], to = itemRange[2], by = .1))
		return(seq(from = itemRange[1], to = itemRange[2], length.out = 25))

	}

	binItems <- function(level, labelMat, cutMat) {

		paste(sort(labelMat[cutMat == level]), collapse = " | ")

	}
	
	thr <- as.matrix(thr)

	nI <- dim(thr)[1]
	nL <- dim(thr)[2]
	
	if(is.null(yRange))
		yRange <- c(min(thr),max(thr))
	par(oma = oma)	
	par(mgp = c(1, 0.2, 0))

	plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, ylab = "", ylim = yRange, xlim = c(0.5, nI + 
		0.5), cex.lab = .8, font.lab = 3)

	box(bty = "o")
	usr <- par("usr")
	par(mgp = c(3, 1, 0))
	axis(4, las = 1, cex.axis = .7, font.axis = 2)

	
	item.hist <- hist(thr, plot = FALSE, breaks = Nbins(yRange))

	itemBinLocations <- item.hist$mids
	bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
	item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2), ybottom = item.hist$mids * 0, xright = item.hist$mids + 
		(bin.size/2), ytop = item.hist$counts)

	item.labels <- matrix(rep(formatC(1:nI, digits = 1, format = "d", flag = "0"), nL), ncol = nL)
	item.labels <- t(apply(item.labels, 1, paste, c(1:nL), sep = "."))

	binnedItems <- matrix(cut(thr, breaks = c(item.hist[, 1], tail(item.hist[, 3], 1)), labels = c(1:length(item.hist[, 
		1] + 1))), ncol = nL)

	binnedList <- unlist(lapply(1:length(itemBinLocations), binItems, item.labels, binnedItems))

	text(cbind(0, itemBinLocations), labels = binnedList, pos = 4, offset = 1 * 15/nI,cex = .65)
	
	mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)

}