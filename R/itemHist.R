itemHist <- function(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", 
                     show.axis.logits = "R", axis.logits.cex = 0.7,
                     oma = c(0, 0, 0, 3), cutpoints = NULL, ...) {

    # Function to plot histograms of item thresholds (thr) with optional cutpoints and logit axis

    # Helper function to define breaks for histogram bins
    Nbins <- function(x) {
        itemRange <- range(x)
        round((itemRange[2] - itemRange[1]) / 0.2, 0)
        return(seq(from = itemRange[1], to = itemRange[2], length.out = 25))
    }
    
    # Ensure that thr is a matrix
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
        xleft = item.hist$mids - (bin.size / 2), 
        ybottom = item.hist$mids * 0, 
        xright = item.hist$mids + (bin.size / 2), 
        ytop = item.hist$counts
    )
    
    # Set graphical parameters
    par(oma = oma)
    par(mgp = c(1, 0.2, 0))

    # Create the base plot without axes
    plot(c(min(item.hist[, 1]), max(item.hist[, 3])), 
         c(min(item.hist[, 2]), max(item.hist[, 4])), 
         ylim = yRange, xlim = c(0, max(item.hist[, 4])), 
         type = "n", axes = FALSE, ylab = "", xlab = axis.items, 
         cex.lab = .8, font.lab = 3)

    # Add cutpoints if provided
    if (!is.null(cutpoints)) {
        cutLines(cutpoints, ...)
    }

    # Add box around the plot
    box(bty = "o")
    usr <- par("usr")
    par(mgp = c(3, 1, 0))

    # Draw the logit axis on the right or left based on user input
    if (show.axis.logits %in% c("R", TRUE)) {
        axis(4, las = 1, cex.axis = axis.logits.cex, font.axis = 2) 
        mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
    } else if (show.axis.logits == "L") {
        axis(2, las = 1, cex.axis = axis.logits.cex, font.axis = 2)  
        mtext(axis.logits, side = 2, line = 1.5, cex = 0.8, font = 3)
    }

    # Reset graphical parameters and draw rectangles representing the histogram
    par(mgp = c(0, 0.2, 0))
    rect(item.hist$xleft, item.hist$ybottom, item.hist$xright, item.hist$ytop)
}