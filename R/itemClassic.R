itemClassic <- function(thr, yRange = NULL, axis.items = "Items", 
                        axis.logits = "Logits", show.axis.logits = "R", 
                        axis.logits.cex = 0.7, oma = c(0, 0, 0, 3), 
                        cutpoints = NULL, label.items = NULL, label.steps = NULL, 
                        label.sep = ".", thr.lab.sep = " | ", thr.lab.par = list(), 
                        axis.logits.par = list(), logits.text.par = list(), 
                        pad.char = "_", font.family = "mono", ...) {
  
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
    paste(sort(labelMat[cutMat == level]), collapse = thr.lab.sep)
  }
  
  thr <- as.matrix(thr)
  nI <- nrow(thr)
  nL <- ncol(thr)
  
  # Set yRange if not provided
  if (is.null(yRange)) {
    yRange <- range(thr, na.rm = TRUE)
    yA <- (yRange[2] - yRange[1]) * 0.1
    yRange <- yRange + c(-yA, yA)
  }

  # Set up outer margins and adjust axis label positions
  par(oma = oma)
  par(mgp = c(3, 1, 0))

  # Plot the empty canvas for the histogram
  plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, 
       ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = 0.8, 
       font.lab = 3)
  
  box(bty = "o")

  # Show logit axis based on the provided position and add customization
  if (show.axis.logits != FALSE) {
    axis.l <- function(side = 4, las = 1, cex.axis = axis.logits.cex, font.axis = 2, ...) {
      return(list(side = side, las = las, cex.axis = cex.axis, font.axis = font.axis, ...))
    }
    
    if (show.axis.logits == "L") {
      axis.logits.par$side <- 2
    } else {
      axis.logits.par$side <- 4
    }

    # Ensure cex.axis defaults to axis.logits.cex if not provided
    if (is.null(axis.logits.par$cex.axis)) {
      axis.logits.par$cex.axis <- axis.logits.cex
    }
    
    axis.logits.par <- do.call(axis.l, axis.logits.par)
    do.call(axis, axis.logits.par)
    
    logits.text <- function(text = axis.logits, side = axis.logits.par$side, 
                            line = 1.5, cex = 0.8, font = 3, ...) {
      return(list(text = text, side = side, line = line, cex = cex, font = font, ...))
    }
    
    logits.text.par <- do.call(logits.text, logits.text.par)
    do.call(mtext, logits.text.par)
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

  # Handle labels for items (padding them with selected character)
  if (is.null(label.items)) {
    label.items <- formatC(1:nI, digits = 1, format = "d", flag = "0")
  }
  
  # Pad all item labels to the same length using the selected character
  maxLabelLength <- max(nchar(label.items))
  label.items <- sapply(label.items, function(label) {
    gsub(" ", pad.char, sprintf(paste0("%-", maxLabelLength, "s"), label))
  })
  
  label.items <- matrix(label.items, ncol = 1)
  
  # Handle step labels for thresholds
  if (is.null(label.steps)) {
    label.steps <- 1:nL
  } else if (is.logical(label.steps) && label.steps) {
    label.steps <- if (!is.null(colnames(thr))) colnames(thr) else 1:nL
  }

  # Generate item labels if multiple levels
  if (nL > 1) {
    item.labels <- t(apply(label.items, 1, paste, label.steps, sep = label.sep))
  } else {
    item.labels <- label.items
  }

  # Bin the items
  binnedItems <- matrix(cut(thr, breaks = Nbins(thr, yRange), labels = c(1:length(item.hist[, 1] + 1))), 
                        ncol = nL)
  
  binnedList <- unlist(lapply(1:length(itemBinLocations), binItems, item.labels, binnedItems))

  # Customizable threshold labels with optional font family
  thr.lab <- function(x = 0, pos = 4, offset = 1 * 15 / nI, cex = 0.65, family = font.family, ...) {
    return(list(x = x, y = itemBinLocations, labels = binnedList, pos = pos, 
                offset = offset, cex = cex, family = family, ...))
  }
  
  thr.lab.par <- do.call(thr.lab, thr.lab.par)
  do.call(text, thr.lab.par)
  
}