itemClassic <- function(thr, yRange = NULL, axis.items = "Items", 
                        axis.logits = "Logits", show.axis.logits = "R", 
                        axis.logits.cex = 0.7, oma = c(0, 0, 0, 3), 
                        cutpoints = NULL, label.items = NULL, ...) {
  
  # Helper function to generate breaks for the histogram
  Nbins <- function(thr, itemRange) {
    breaks <- seq(from = itemRange[1], to = itemRange[2], length.out = 25)
    
    if (min(thr, na.rm = TRUE) < min(breaks)) 
      breaks <- c(min(thr, na.rm = TRUE), breaks)
    if (max(thr, na.rm = TRUE) > max(breaks)) 
      breaks <- c(breaks, max(thr, na.rm = TRUE))
    
    return(breaks)
  }

  # Helper function to bin items by level
  binItems <- function(level, labelMat, cutMat) {
    paste(sort(labelMat[cutMat == level]), collapse = " | ")
  }
  
  thr <- as.matrix(thr)
  nI <- dim(thr)[1]
  nL <- dim(thr)[2]
  
  # Use provided label.items or default to 1:nI
  if (is.null(label.items)) {
    label.items <- formatC(1:nI, digits = 1, format = "d", flag = "0")
  }
  
  # Ensure all labels are of equal length by padding them with underscores
  maxLabelLength <- max(nchar(label.items))
  label.items <- sapply(label.items, function(label) {
    # Use sprintf and replace spaces with underscores
    gsub(" ", "_", sprintf(paste0("%-", maxLabelLength, "s"), label))
  })
  
  # Handle multiple levels if nL > 1
  item.labels <- matrix(rep(label.items, nL), ncol = nL)
  if (nL > 1) {
    item.labels <- t(apply(item.labels, 1, paste, c(1:nL), sep = "."))
  }

  # Set yRange if not provided
  if (is.null(yRange)) {
    yRange <- c(min(thr, na.rm = TRUE), max(thr, na.rm = TRUE))
    yA <- (yRange[2] - yRange[1]) * 0.1
    yRange <- yRange + c(-yA, yA)
  }
  
  par(oma = oma)
  par(mgp = c(3, 1, 0))  # Increase mgp[2] to move the tick labels slightly

  plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, 
       ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = 0.8, 
       font.lab = 3)
  
  box(bty = "o")
  
  # Show logit axis based on the provided position
  if (show.axis.logits == "R" | show.axis.logits == TRUE) {
    axis(4, las = 1, cex.axis = axis.logits.cex, font.axis = 2)
    mtext(axis.logits, side = 4, line = 1.5, cex = 0.8, font = 3)
  } else if (show.axis.logits == "L") {
    par(mgp = c(3, 1, 0))  # Move left axis tick labels even more to the right
    axis(2, las = 1, cex.axis = axis.logits.cex, font.axis = 2)
    mtext(axis.logits, side = 2, line = 1.5, cex = 0.8, font = 3)
  }
  
  # Plot cutpoints if provided
  if (!is.null(cutpoints)) {
    cutLines(cutpoints, ...)
  }
  
  # Create histogram data
  item.hist <- hist(thr, plot = FALSE, breaks = Nbins(thr, yRange))
  itemBinLocations <- item.hist$mids
  bin.size <- abs(item.hist$breaks[1] - item.hist$breaks[2])
  
  item.hist <- data.frame(xleft = item.hist$mids - (bin.size / 2), 
                          ybottom = item.hist$mids * 0,
                          xright = item.hist$mids + (bin.size / 2), 
                          ytop = item.hist$counts)
  
  binnedItems <- matrix(cut(thr, breaks = Nbins(thr, yRange), 
                            labels = c(1:length(item.hist[, 1] + 1))), 
                        ncol = nL)
  
  binnedList <- unlist(lapply(1:length(itemBinLocations), binItems, 
                              item.labels, binnedItems))
  
  # Force labels to use a monospace font and apply the underscore padding
  text(cbind(0, itemBinLocations), labels = binnedList, pos = 4, 
       offset = 1 * 15 / nI, cex = 0.65, family = "mono")
}