wrightMap.default <-
function(thetas, thresholds, use.hist = TRUE, main.title = "Wright Map", axis.logits = "Logits", axis.persons = "Respondents", axis.items = "Items", label.items = NULL, label.items.rows = 1, label.items.srt = 0, label.items.ticks = TRUE, show.thr.lab = TRUE, show.thr.sym = TRUE, thr.lab.text = NULL, thr.lab.col = "black", thr.lab.pos = c(2, 4), thr.lab.font = 2, thr.lab.cex = 0.85, thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3), thr.sym.cex = 1.2, thr.sym.lwd = 1, dim.names = NULL, dim.color = NULL, dim.lab.side = 3, dim.lab.adj = 0.5, hist.nclass = "FD", min.logit.pad = 0.25, max.logit.pad = 0.25, item.prop = 0.8,return.thresholds = TRUE,new.quartz= FALSE,...) {
    
    
    ## Helper Functions
    
    theta.dens <- function(thetas, use.hist, hist.nclass) {
        
        if (use.hist == FALSE) {
            
            densList <- apply(thetas, 2, density, na.rm = TRUE)
            
            densExt <- function(densElem) {
                
                xDim <- densElem["y"][[1]]
                yDim <- densElem["x"][[1]]
                
                xDim <- xDim/max(xDim)
                
                densInfo <- cbind(xDim, yDim)
                return(densInfo)
                
            }
            
            distInfo <- lapply(densList, densExt)
            
        } else {
            
            densList <- apply(thetas, 2, hist, plot = FALSE, nclass = hist.nclass)
            
            
            densExt <- function(densElem) {
                
                bin.size <- abs(densElem$breaks[1] - densElem$breaks[2])
                
                thetaHist <- data.frame(xleft = densElem$mids - (bin.size/2), ybotton = densElem$mids * 0, xright = densElem$mids + (bin.size/2), ytop = densElem$counts)
                
                return(thetaHist)
                
            }
            
            distInfo <- lapply(densList, densExt)
            
        }
        
        return(distInfo)
        
    }
    
    
    personPlot <- function(distInfo, use.hist, yRange, xRange, dim.lab.side, dim.lab.adj, dims.col, p.cex.lab, p.font.lab, p.lwd, p.las, p.cex.axis, p.font.axis, p.tcl) {
        
        if (use.hist == FALSE) {
            
            plot(distInfo, ylim = yRange, xlim = xRange, type = "l", axes = FALSE, ylab = "", xlab = "", cex.lab = p.cex.lab, font.lab = p.font.lab, lwd = p.lwd, 
                col = attr(distInfo, "dim.color"))
            
        } else {
            # print( distInfo)
            #print(attr(distInfo, "dim.color"))
            plot(c(min(distInfo[, 1]), max(distInfo[, 3])), c(min(distInfo[, 2]), max(distInfo[, 4])), ylim = yRange, xlim = c(max(distInfo[, 4]), 0), 
                type = "n", axes = FALSE, ylab = "", xlab = "", cex.lab = p.cex.lab, font.lab = p.font.lab, lwd = p.lwd, col = attr(distInfo, "dim.color"))
            
            rect(distInfo[, 4], distInfo[, 1], distInfo[, 2], distInfo[, 3], col = attr(distInfo, "dim.color"))
            
        }
        
        mtext(attr(distInfo, "dim.name"), side = dim.lab.side, line = -1, cex = 0.6, font = 1, adj = dim.lab.adj)
        
        box(bty = "c")
        
    }
    
    
    # Preparing Data
    
    thetas <- as.matrix(thetas)
    thr <- as.matrix(thresholds)
    
    # Setting plot parameters
    
    nD <- ncol(thetas)
    nI <- dim(thr)[1]
    nL <- dim(thr)[2]
    
    if (is.null(label.items)) {
        label.items <- c(paste("Item", seq(1:nI)))
    }
    
    
    min.theta <- quantile(thetas, probs = c(0.01), na.rm = TRUE)
    max.theta <- quantile(thetas, probs = c(0.99), na.rm = TRUE)
    
    min.l <- min(c(min.theta, thr), na.rm = TRUE) - min.logit.pad
    max.l <- max(c(max.theta, thr), na.rm = TRUE) + max.logit.pad
    
    yRange <- c(min.l, max.l)
    xRange <- c(1, 0)
    
    item.side <- round((nD * item.prop)/(1 - item.prop))
    layout.wm <- c(seq(1:nD), rep(nD + 1, item.side))

    if ( is.null(dim.color)){

        if ( use.hist) { dim.color <- "white"}
        if (!use.hist) { dim.color <- "black"}

    } 
    
    # Creating default matrices if info not provided
    
    if (is.null(dim.names)) {
        dim.names <- c(paste("Dim", seq(1:nD), sep = ""))
    }
    
    
    if (is.null(thr.lab.text)) {
        thr.lab.text = col(thr)
    }
    
    
    if (ncol(thetas) > 1 & length(dim.color) == 1) {
        
        dim.color <- rep(dim.color, ncol(thetas))
        
    }
    
    # Generating Full Map
    

    if(new.quartz)
    	dev.new(width = 9, height = 5)
    par(oma = c(0, 5, 0, 5))
    
    layout(matrix(layout.wm, nrow = 1), widths = c(rep((1 - item.prop)/nD, nD), rep(item.prop/item.side, item.side)), heights = 0.8)
    
    ## Generating Person Side
    
    
    par(mar = c(5, 0.1, 4, 0) + 0.1)
    par(mgp = c(2.7, 1, 0))
    
    distInfo <- theta.dens(thetas, use.hist, hist.nclass)
    
    for (i in 1:nD) {
        
        attr(distInfo[[i]], "dim.name") <- dim.names[i]
        attr(distInfo[[i]], "dim.color") <- dim.color[i]
        
    }
    
    lapply(distInfo, FUN = personPlot, use.hist = use.hist, yRange = yRange, xRange = xRange, dim.lab.side = dim.lab.side, dim.lab.adj = dim.lab.adj, p.cex.lab = 1.3, 
        p.font.lab = 3, p.lwd = 2, p.las = 1, p.cex.axis = 1.1, p.font.axis = 2, p.tcl = -0.5)
    
    ## Generating Item Side
    
    plot(seq(1:nI), rep(0, nI), type = "n", axes = FALSE, xlab = axis.items, ylab = "", ylim = yRange, xlim = c(0.5, nI + 0.5), cex.lab = 1.3, font.lab = 3)
    
    box(bty = "o")
    
    usr <- par("usr")
    
    axis(4, las = 1, cex.axis = 1.2, font.axis = 2)
    par(mgp = c(0, 0.2, 0))
    
    if (show.thr.sym == TRUE) {
        
        points(row(thr), thr, ylim = yRange, type = "p", cex = thr.sym.cex, lwd = thr.sym.lwd, pch = as.matrix(thr.sym.pch), col = as.matrix(thr.sym.col.fg), 
            bg = as.matrix(thr.sym.col.bg))
        
    }
    
    
    if (show.thr.lab == TRUE) {
        
        if (show.thr.sym == TRUE){
            pos <- matrix(rep(rep_len(thr.lab.pos, ncol(thr)), nI), byrow = TRUE, ncol = ncol(thr))
            pos <- t(sapply(1:nrow(thr), function(x) pos[x, rank(thr[x, ])]))
            text(row(thr), thr, labels = as.matrix(thr.lab.text), col = as.matrix(thr.lab.col), pos = pos, cex = thr.lab.cex, font = thr.lab.font)
        } else{

            text(row(thr), thr, labels = as.matrix(thr.lab.text), col = as.matrix(thr.lab.col), cex = thr.lab.cex, font = thr.lab.font)

        }
        
    }
    
    
    par(mgp = c(3, 1, 0))
    
    
    if (label.items.rows == 1) {
        
        if (label.items.srt != 0){ 
            text.adj = c(1.1,1.1)
        } else {
            text.adj = c(0.5, 2)
        }

        text(seq(1:nrow(thr)), y = par("usr")[3], labels = label.items, srt = label.items.srt, adj = text.adj, xpd = TRUE, cex = 0.9)
        
        
        if (label.items.ticks == TRUE) {
            
            axis(1, at = 1:nI, labels = FALSE, line = NA, tcl = -0.35)
            
        }
        
    }
    
    if (label.items.rows == 2) {
        
        
        text(seq(from = 1, to = nrow(thr), by = 2), y = par("usr")[3], labels = label.items[seq(from = 1, to = nrow(thr), by = 2)], adj = c(0.5, 1.9), 
            xpd = TRUE, cex = 0.9)
        
        
        text(seq(from = 2, to = nrow(thr), by = 2), y = par("usr")[3], labels = label.items[seq(from = 2, to = nrow(thr), by = 2)], adj = c(0.5, 3.1), 
            xpd = TRUE, cex = 0.9)
        
        
        if (label.items.ticks == TRUE) {
            
            axis(1, at = seq(from = 1, to = nI, by = 2), labels = FALSE, line = NA, tcl = -0.35)
            axis(1, at = seq(from = 2, to = nI, by = 2), labels = FALSE, line = NA, tcl = -0.9)
            
        }
        
    }
    
    if (label.items.rows == 3) {
        
        
        text(seq(from = 1, to = nrow(thr), by = 3), y = par("usr")[3], labels = label.items[seq(from = 1, to = nrow(thr), by = 3)], adj = c(0.5, 1.9), 
            xpd = TRUE, cex = 0.9)
        
        
        text(seq(from = 2, to = nrow(thr), by = 3), y = par("usr")[3], labels = label.items[seq(from = 2, to = nrow(thr), by = 3)], adj = c(0.5, 3.1), 
            xpd = TRUE, cex = 0.9)
        
        
        text(seq(from = 3, to = nrow(thr), by = 3), y = par("usr")[3], labels = label.items[seq(from = 3, to = nrow(thr), by = 3)], adj = c(0.5, 4.3), 
            xpd = TRUE, cex = 0.9)
        
        
        if (label.items.ticks == TRUE) {
            
            axis(1, at = seq(from = 1, to = nI, by = 3), labels = FALSE, line = NA, tcl = -0.35)
            axis(1, at = seq(from = 2, to = nI, by = 3), labels = FALSE, line = NA, tcl = -0.9)
            axis(1, at = seq(from = 3, to = nI, by = 3), labels = FALSE, line = NA, tcl = -1.4)
            
        }
        
    }
    
    
    mtext(axis.logits, side = 4, line = 2.5, outer = TRUE, cex = 0.9, font = 3)
    mtext(axis.persons, side = 2, line = 1, outer = TRUE, cex = 0.9, font = 3)
    par(oma = c(0, 0, 3, 0))
    mtext(main.title, side = 3, line = 1, outer = TRUE, font = 2)
    if(return.thresholds) {
    return(thresholds)
    }
    
}
