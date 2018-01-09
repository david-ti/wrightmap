itemModern <- function(thr, yRange = NULL, axis.items = "Items", show.thr.sym = TRUE, thr.sym.par = list(), show.thr.lab = TRUE, 
	thr.lab.par = list(), label.items.rows = 1, label.items.ticks = TRUE, label.items.par = list(), axis.logits = "Logits", 
	show.axis.logits = "R", oma = c(0, 0, 0, 3), cutpoints = NULL, vertLines = TRUE, thr.sym.cex = 0.8, thr.sym.lwd = 1, 
	thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3), thr.lab.pos = c(2, 4), thr.lab.text = NULL, 
	thr.lab.col = "black", thr.lab.cex = 0.5, thr.lab.font = 2, label.items.srt = 0, label.items = NULL, label.items.cex = 0.6, 
	...) {

	thr <- as.matrix(thr)

	nI <- dim(thr)[1]

	if (is.null(yRange)) 
		yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))


	if (is.null(label.items)) {
		if (!is.null(rownames(thr))) 
			label.items <- rownames(thr)
		else label.items <- c(paste("Item", seq(1:nI)))
	}
	par(oma = oma)
	#par(mar = c(0,0,0,0))
	par(mgp = c(2, 0.2, 0))


	plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, ylab = "", ylim = yRange, xlim = c(0.5, nI + 
		0.5), cex.lab = 0.8, font.lab = 3)


	if (!is.null(cutpoints)) {
		cutLines(cutpoints, ...)
	}

	box(bty = "o")
	usr <- par("usr")
	par(mgp = c(3, 1, 0))

	if (show.axis.logits == "R" | show.axis.logits == TRUE) {
		axis(4, las = 1, cex.axis = 0.7, font.axis = 2)
		mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
	} else if (show.axis.logits == "L") {
		axis(2, las = 1, cex.axis = 0.7, font.axis = 2)
		mtext(axis.logits, side = 2, line = 1.5, cex = 0.8, font = 3)
	}


	#############
	
	if (vertLines == TRUE) {

		vertLines.data <- cbind(cbind(1:nrow(thr), 1:nrow(thr)), cbind(apply(thr, 1, min, na.rm = TRUE), apply(thr, 1, 
			max, na.rm = TRUE)))

		vertLines <- function(x, ...) {

			lines(c(x[1], x[2]), c(x[3], x[4]), ...)

		}

		apply(vertLines.data, 1, vertLines, col = "grey90")


	}

	if (show.thr.sym) {

		thr.sym <- function(cex = thr.sym.cex, lwd = thr.sym.lwd, pch = thr.sym.pch, lend = "butt", ljoin = "mitre", col = thr.sym.col.fg, 
			bg = thr.sym.col.bg, x = row(thr), y = thr, ...) {

			return(list(cex = cex, lwd = lwd, pch = as.matrix(pch), lend = lend, ljoin = ljoin, col = as.matrix(col), bg = as.matrix(bg), x = x, y = y,
				...))
		}

		thr.sym.par <- do.call(thr.sym, thr.sym.par)
		do.call(points, args = thr.sym.par)

	}

	if (show.thr.lab) {

		thr.lab <- function(labels = thr.lab.text, col = thr.lab.col, pos = thr.lab.pos, cex = thr.lab.cex, font = thr.lab.font, x = row(thr), y = thr,
			...) {

			if (is.null(labels)) {
				if (!is.null(colnames(thr))) 
					labels <- as.data.frame(matrix(rep(colnames(thr), each = nI), nrow = nI))
				else labels = col(thr)
			}

			if (show.thr.sym & length(pos) != length(thr)) {
				pos <- matrix(rep(rep_len(pos, ncol(thr)), nI), byrow = TRUE, ncol = ncol(thr))
				pos <- t(sapply(1:nI, function(x) pos[x, rank(thr[x, ], ties.method = "random")]))
			}

			return(list(labels = as.matrix(labels), col = as.matrix(col), pos = pos, cex = cex, font = font, x = x, y = y,...))

		}

		thr.lab.par <- do.call(thr.lab, thr.lab.par)
		do.call(text, args = thr.lab.par)

	}


	##############
	ticks <- -.5 * 1:label.items.rows
	x.axis.y <- par("usr")[3] + par("cxy")[2] * ticks
	
	x.axis <- function(labels = label.items, srt = label.items.srt, adj = ifelse(srt == 0, c(.5,1),1), xpd = NA, cex = label.items.cex, x = 1:nI, y = x.axis.y, ...) {

		return(list(labels = labels, srt = srt, xpd = xpd, cex = cex, y = y, adj = adj, x = x,...))
	}

	label.items.par <- do.call(x.axis, label.items.par)

	do.call(what = text, args = label.items.par)
	
	if (label.items.ticks) {

		suppressWarnings(do.call(what = mapply, args = list(FUN = axis, side = 1, at = label.items.par$x, labels = FALSE, tcl = (label.items.par$y - par("usr")[3])/(par("cxy")[2]) +.2)))
	}

	

}
