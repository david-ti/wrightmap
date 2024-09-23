itemHist <- function(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", 
                     show.axis.logits = "R", axis.logits.cex = 0.7, oma = c(0, 0, 0, 3), 
                     cutpoints = NULL, axis.logits.par = list(), logits.text.par = list(), 
                     cutpoints.par = list(), ...) {

    # Helper function to define breaks for histogram bins
    Nbins <- function(x) {
        itemRange <- range(x)
        return(seq(from = itemRange[1], to = itemRange[2], length.out = 25))
    }
    
    # Ensure that 'thr' is a matrix
    thr <- as.matrix(thr)
    nI <- dim(thr)[1]
    
    # Set y-axis range if not provided
    if (is.null(yRange)) {
        yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))
    }

    # Create histogram data for the item thresholds
    item.hist <- hist(thr, plot = FALSE, breaks = Nbins(yRange))
    bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
    item.hist <- data.frame(
        yleft = item.hist$mids - (bin.size / 2), 
        xbottom = item.hist$mids * 0, 
        yright = item.hist$mids + (bin.size / 2), 
        xtop = item.hist$counts
    )
    
    # Set graphical parameters
    par(oma = oma)
    par(mgp = c(1, 0.2, 0))

    # Create the base plot without axes (note swapped x and y axes for horizontal bars)
    plot(c(min(item.hist[, 2]), max(item.hist[, 4])), 
         c(min(item.hist[, 1]), max(item.hist[, 3])), 
         xlim = c(0, max(item.hist[, 4])), ylim = yRange, 
         type = "n", axes = FALSE, xlab = axis.items, ylab = "", 
         cex.lab = .8, font.lab = 3)

    # Add cutpoints if provided (using customized parameters)
    if (!is.null(cutpoints)) {
        cutpoints.default <- list(col = "black", lwd = 1, lty = 2)
        cutpoints.par <- modifyList(cutpoints.default, cutpoints.par)
        abline(v = cutpoints, col = cutpoints.par$col, lwd = cutpoints.par$lwd, lty = cutpoints.par$lty)
    }

    # Add box around the plot
    box(bty = "o")
    usr <- par("usr")
    par(mgp = c(3, 1, 0))

    # Handle axis customization for logits (using `axis.logits.par`)
    axis.default <- list(side = if (show.axis.logits == "L") 2 else 4, 
                         las = 1, cex.axis = axis.logits.cex, font.axis = 2)
    axis.logits.par <- modifyList(axis.default, axis.logits.par)
    do.call(axis, axis.logits.par)
    
    # Customize the text for the logits axis (using `logits.text.par`)
    logits.text.default <- list(text = axis.logits, side = axis.logits.par$side, 
                                line = 1.5, cex = 0.8, font = 3)
    logits.text.par <- modifyList(logits.text.default, logits.text.par)
    do.call(mtext, logits.text.par)

    # Reset graphical parameters and draw rectangles representing the histogram (swapped x and y axes)
    par(mgp = c(0, 0.2, 0))
    rect(item.hist$xbottom, item.hist$yleft, item.hist$xtop, item.hist$yright)
}