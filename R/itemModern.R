itemModern <- function(thr, yRange = NULL, axis.items = "Items", 
                       show.thr.sym = TRUE, thr.sym.cex = .8, 
                       thr.sym.lwd = 1, thr.sym.pch = 23, 
                       thr.sym.col.fg = rgb(0, 0, 0, 0.3), 
                       thr.sym.col.bg = rgb(0, 0, 0, 0.3), 
                       show.thr.lab = TRUE, thr.lab.pos = c(2, 4), 
                       thr.lab.text = NULL, thr.lab.col = "black", 
                       thr.lab.cex = .5, thr.lab.font = 2,
                       label.items.rows = 1, label.items.srt = 0, 
                       label.items = NULL, label.items.cex = 0.6, 
                       label.items.ticks = TRUE, axis.logits = "Logits", 
                       show.axis.logits = "R", axis.logits.cex = 0.7, 
                       oma = c(0, 0, 0, 3), cutpoints = NULL, 
                       vertLines = FALSE, vertLines.par = list(), 
                       thr.sym.par = list(), thr.lab.par = list(), 
                       label.items.par = list(), axis.logits.par = list(), 
                       logits.text.par = list(), ...) {
    
    # Ensure 'thr' is a matrix
    thr <- as.matrix(thr)
    nI <- nrow(thr)
    
    # Set y-axis range based on threshold values if not provided
    if (is.null(yRange)) {
        yRange <- range(thr, na.rm = TRUE)
    }
    
    # Set default threshold labels if not provided
    if (is.null(thr.lab.text)) {
        if (!is.null(colnames(thr))) {
            thr.lab.text <- matrix(rep(colnames(thr), each = nI), nrow = nI)
        } else {
            thr.lab.text <- matrix(rep(seq_len(ncol(thr)), each = nI), nrow = nI)
        }
    }
    
    # Set default item labels if not provided
    if (is.null(label.items)) {
        if (!is.null(rownames(thr))) {
            label.items <- rownames(thr)
        } else {
            label.items <- paste("Item", seq_len(nI))
        }
    }
    
    # Set global plot margins and axis parameters
    par(oma = oma, mgp = c(2, 1, 0))
    
    # Create base plot without axes
    plot(seq_len(nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, 
         ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = .8, 
         font.lab = 3)
    
    # Add cutpoints if provided
    if (!is.null(cutpoints)) {
        cutLines(cutpoints, ...)
    }
    
    # Draw logit axis based on user choice and customize using `axis.logits.par`
    if (show.axis.logits %in% c("R", TRUE)) {
        axis.logits.default <- list(side = 4, las = 1, cex.axis = axis.logits.cex, font.axis = 2)
        axis.logits.par <- modifyList(axis.logits.default, axis.logits.par)
        do.call(axis, axis.logits.par)
        
        logits.text.default <- list(text = axis.logits, side = 4, line = 1.5, cex = axis.logits.cex, font = 3)
        logits.text.par <- modifyList(logits.text.default, logits.text.par)
        do.call(mtext, logits.text.par)
    } else if (show.axis.logits == "L") {
        axis.logits.default <- list(side = 2, las = 1, cex.axis = axis.logits.cex, font.axis = 2)
        axis.logits.par <- modifyList(axis.logits.default, axis.logits.par)
        do.call(axis, axis.logits.par)
        
        logits.text.default <- list(text = axis.logits, side = 2, line = 1.5, cex = axis.logits.cex, font = 3)
        logits.text.par <- modifyList(logits.text.default, logits.text.par)
        do.call(mtext, logits.text.par)
    }
    
    # Draw vertical lines between min and max thresholds if 'vertLines' is TRUE
    if (vertLines == TRUE) {
        # Default vertical line style
        vertLines.default <- list(col = "grey90", lty = 1, lwd = 1)
        vertLines.par <- modifyList(vertLines.default, vertLines.par)
        
        # Ensure proper handling of NA values and matching lengths
        vertLines.data <- cbind(seq_len(nI), t(apply(thr, 1, function(row) range(row, na.rm = TRUE))))
        
        # Ensure no mismatch in dimensions
        if (nrow(vertLines.data) == nI) {
            apply(vertLines.data, 1, function(x) {
                if (!is.na(x[2]) && !is.na(x[3])) {
                    lines(c(x[1], x[1]), c(x[2], x[3]), col = vertLines.par$col, 
                          lty = vertLines.par$lty, lwd = vertLines.par$lwd)
                }
            })
        }
    }
    
    # Plot threshold symbols (using the additional flexibility from 'thr.sym.par')
    if (show.thr.sym) {
        thr.sym.default <- list(cex = thr.sym.cex, lwd = thr.sym.lwd, pch = thr.sym.pch, 
                                col = thr.sym.col.fg, bg = thr.sym.col.bg)
        thr.sym.par <- modifyList(thr.sym.default, thr.sym.par)
        points(row(thr), thr, type = "p", cex = thr.sym.par$cex, lwd = thr.sym.par$lwd, 
               pch = as.matrix(thr.sym.par$pch), col = as.matrix(thr.sym.par$col), 
               bg = as.matrix(thr.sym.par$bg))
    }
    
    # Add threshold labels (using the flexibility from 'thr.lab.par')
    if (show.thr.lab) {
        thr.lab.default <- list(labels = as.matrix(thr.lab.text), col = thr.lab.col, 
                                pos = thr.lab.pos, cex = thr.lab.cex, font = thr.lab.font)
        thr.lab.par <- modifyList(thr.lab.default, thr.lab.par)
        text(row(thr), thr, labels = thr.lab.par$labels, col = thr.lab.par$col, 
             pos = thr.lab.par$pos, cex = thr.lab.par$cex, font = thr.lab.par$font)
    }
    
    # Add item labels (using the flexibility from 'label.items.par')
    if (label.items.rows == 1) {
        label.items.default <- list(labels = label.items, srt = label.items.srt, 
                                    cex = label.items.cex, adj = if (label.items.srt != 0) c(1, 1) else c(0.5, 2))
        label.items.par <- modifyList(label.items.default, label.items.par)
        text(seq_len(nI), y = par("usr")[3] - 0.05, labels = label.items.par$labels, 
             srt = label.items.par$srt, adj = label.items.par$adj, 
             xpd = TRUE, cex = label.items.par$cex)
        if (label.items.ticks) {
            axis(1, at = seq_len(nI), labels = FALSE, line = NA, tcl = -0.35)
        }
    }
    
    # Add item labels for multiple rows (using flexibility from 'label.items.par')
    if (label.items.rows == 2) {
        odd_rows <- seq(1, nI, 2)
        even_rows <- seq(2, nI, 2)
        text(odd_rows, y = par("usr")[3], labels = label.items[odd_rows], adj = c(0.5, 2.4), 
             xpd = TRUE, cex = label.items.cex)
        text(even_rows, y = par("usr")[3], labels = label.items[even_rows], adj = c(0.5, 4.0), 
             xpd = TRUE, cex = label.items.cex)
        if (label.items.ticks) {
            axis(1, at = odd_rows, labels = FALSE, line = NA, tcl = -0.35)
            axis(1, at = even_rows, labels = FALSE, line = NA, tcl = -0.9)
        }
    }
}
