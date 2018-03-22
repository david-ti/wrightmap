itemClassic <-
function(thr, yRange = NULL, axis.items = "Items",axis.logits = "Logits",show.axis.logits = "R",oma = c(0,0,0,3),cutpoints = NULL, label.items = NULL,label.steps = NULL,label.sep = ".",thr.lab.sep = " | ",thr.lab.par = list(), axis.logits.par = list(),logits.text.par = list(),...) {
	Nbins <- function(thr,itemRange) {
		
		#print(paste("thr =", c(min(thr),max(thr))))
		#print(paste("range =",itemRange))


				
		breaks <- seq(from = itemRange[1], to = itemRange[2], length.out = 25)
		
		#print(breaks)
		if(min(thr, na.rm = TRUE) < min(breaks))
			breaks <- c(min(thr, na.rm = TRUE),breaks)
			
		if(max(thr, na.rm = TRUE) > max(breaks))
			breaks <- c(breaks,max(thr, na.rm = TRUE))
		
		return(breaks)
	}

	binItems <- function(level, labelMat, cutMat) {

		paste(sort(labelMat[cutMat == level]), collapse = thr.lab.sep)

	}
	
	thr <- as.matrix(thr)

	nI <- dim(thr)[1]
	nL <- dim(thr)[2]
	
	if(is.null(yRange)) {
		yRange <- c(min(thr, na.rm = TRUE),max(thr, na.rm = TRUE))
		yA <- (yRange[2] - yRange[1])*.1
		yRange <- yRange + c(-yA,yA )
	}
	par(oma = oma)	
	par(mgp = c(1, 0.2, 0))

	plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, ylab = "", ylim = yRange, xlim = c(0.5, nI + 
		0.5), cex.lab = .8, font.lab = 3)

	box(bty = "o")
	usr <- par("usr")
	par(mgp = c(3, 1, 0))
	
	if(show.axis.logits != FALSE) {
				
		axis.l <- function(side = 4, las = 1, cex.axis = 0.7, font.axis = 2, ...) {
			return(list(side = side, las = las, cex.axis = cex.axis, font.axis = font.axis, ...))
		}
		
		if(show.axis.logits == "L") {
			axis.logits.par$side == 2
		}
		
		axis.logits.par <- do.call(axis.l, axis.logits.par)
		do.call(axis, axis.logits.par)
		
		logits.text <- function(text = axis.logits, side = axis.logits.par$side, line = 1.5, cex = 0.8, font = 3, ...) {
			return(list(text = text, side = side, line = line, cex = cex, font = font, ...))
		}
		
		logits.text.par <- do.call(logits.text, logits.text.par)
		do.call(mtext,logits.text.par)
		
	}
	

	if(!is.null(cutpoints)) {
		cutLines(cutpoints,...)
	}
	
	item.hist <- hist(thr, plot = FALSE, breaks = Nbins(thr,yRange))
	#print(item.hist$mids)
	#stop()

	itemBinLocations <- item.hist$mids
	#print(cbind(0, itemBinLocations))
	bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
	item.hist <- data.frame(xleft = item.hist$mids - (bin.size/2), ybottom = item.hist$mids * 0, xright = item.hist$mids + 
		(bin.size/2), ytop = item.hist$counts)

	if(is.null(label.items) || label.items == FALSE)
		label.items <- matrix(rep(formatC(1:nI, digits = 1, format = "d", flag = "0"), nL), ncol = nL)
	else if(label.items == TRUE)
		label.items <- matrix(rownames(thr))
	else
		label.items <- matrix(label.items)
	if(is.null(label.steps) || label.steps == FALSE) {
		label.steps <- c(1:nL)
	} else if(label.steps == TRUE) {
		if(!is.null(colnames(thr)))
			label.steps <- colnames(thr)
		else
			label.steps <- c(1:nL)
	}
	if(nL > 1){
		item.labels <- t(apply(label.items, 1, paste, label.steps, sep = label.sep))
	}
	
	#print(c(item.hist[, 1], tail(item.hist[, 3], 1)))
	#print(Nbins(thr,yRange))

	binnedItems <- matrix(cut(thr, breaks = Nbins(thr,yRange), labels = c(1:length(item.hist[, 
		1] + 1))), ncol = nL)
	
	#print(binnedItems)

	binnedList <- unlist(lapply(1:length(itemBinLocations), binItems, item.labels, binnedItems))

	thr.lab <- function(x = 0, pos = 4, offset = 1 * 15/nI, cex = .65, ...) {
		return(list(x = x, y = itemBinLocations, labels = binnedList, pos = pos, offset = offset, cex = cex, ...))
	}
	
	thr.lab.par <- do.call(thr.lab,thr.lab.par)
	do.call(text,thr.lab.par)
	
}
