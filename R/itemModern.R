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
                       axis.logits.size = 0.7,  
                       oma = c(0, 0, 0, 3), cutpoints = NULL, 
                       vertLines = FALSE, ...) {
    
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
    plot(seq_len(nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, ylab = "", 
         ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = .8, font.lab = 3)
    
    # Add cutpoints if provided
    if (!is.null(cutpoints)) {
        cutLines(cutpoints, ...)
    }
    
    # Draw logit axis on the right or left based on user choice
    if (show.axis.logits %in% c("R", TRUE)) {
        axis(4, las = 1, cex.axis = axis.logits.size, font.axis = 2)  
        mtext(axis.logits, side = 4, line = 1.5, cex = axis.logits.cex, font = 3)  
    } else if (show.axis.logits == "L") {
        axis(2, las = 1, cex.axis = axis.logits.size, font.axis = 2)  
        mtext(axis.logits, side = 2, line = 1.5, cex = axis.logits.cex, font = 3)  
    }
    
    # Draw vertical lines between min and max thresholds if 'vertLines' is TRUE
    if (vertLines) {
        vertLines.data <- cbind(seq_len(nI), apply(thr, 1, range, na.rm = TRUE))
        apply(vertLines.data, 1, function(x) lines(rep(x[1], 2), x[2:3], col = "grey90"))
    }
    
    # Plot threshold symbols
    if (show.thr.sym) {
        points(row(thr), thr, type = "p", cex = thr.sym.cex, lwd = thr.sym.lwd, 
               pch = as.matrix(thr.sym.pch), col = as.matrix(thr.sym.col.fg), 
               bg = as.matrix(thr.sym.col.bg))
    }
    
    # Add threshold labels
    if (show.thr.lab) {
        pos <- thr.lab.pos
        if (length(pos) != length(thr)) {
            pos <- matrix(rep(rep_len(pos, ncol(thr)), nI), byrow = TRUE, ncol = ncol(thr))
            pos <- t(sapply(seq_len(nI), function(x) pos[x, rank(thr[x, ], ties.method = "random")]))
        }
        text(row(thr), thr, labels = as.matrix(thr.lab.text), col = as.matrix(thr.lab.col), 
             pos = pos, cex = thr.lab.cex, font = thr.lab.font)
    }
    
    # Add item labels (1 row case)
    if (label.items.rows == 1) {
        adj <- if (label.items.srt != 0) c(1, 1) else c(0.5, 2)
        text(seq_len(nI), y = par("usr")[3] - 0.05, labels = label.items, srt = label.items.srt, 
             adj = adj, xpd = TRUE, cex = label.items.cex)
        if (label.items.ticks) {
            axis(1, at = seq_len(nI), labels = FALSE, line = NA, tcl = -0.35)
        }
    }
    
    # Add item labels for multiple rows (2 rows case)
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