\name{item.side}
\alias{item.side}
\alias{itemClassic}
\alias{itemModern}
\alias{itemHist}

\title{Wright Map item sides}
\description{Draw the item side of a Wright Map in a variety of styles. Intended to be primarily called by \code{\link{wrightMap}}, but also available for use on their own.
}
\usage{
itemModern(thr, yRange = NULL, axis.items = "Items", show.thr.sym = TRUE, thr.sym.cex = 0.8, thr.sym.lwd = 1, thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3), show.thr.lab = TRUE, thr.lab.pos = c(2, 4), thr.lab.text = NULL, thr.lab.col = "black", thr.lab.cex = 0.5, thr.lab.font = 2, label.items.rows = 1, label.items.srt = 0, label.items = NULL, label.items.cex = 0.6, label.items.ticks = TRUE, axis.logits = "Logits", show.axis.logits = "R", oma = c(0, 0, 0, 3), cutpoints = NULL, ...)

itemClassic(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", show.axis.logits = "R", oma = c(0, 0, 0, 3), cutpoints = NULL, ...)

itemHist(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits", show.axis.logits = "R", oma = c(0, 0, 0, 3), cutpoints = NULL,...)
}

\arguments{
	
General arguments:
  \item{thr}{vector or matrix of threshold parameters. If a matrix, items should be in the rows and steps in the columns.}
  \item{yRange}{ vector with 2 elements specifying the lower and upper limits of the plot's y-axis.}
  \item{axis.items}{title of the x-axis.}
  \item{axis.logits}{title of the y-axis.}
  \item{show.axis.logits}{ if equal to "R" or "L", draws a logit axis on the right or left. Will also draw an axis on the right if the value is 
  	code{TRUE}. If any other value, the axis is not drawn.}
  \item{oma}{values to use for the \code{oma} parameter (see \code{\link{par}})}
  \item{cutpoints}{values at which to draw horizontal lines (see \code{\link{cutLines}})}
  \item{\dots}{additional arguments to \code{\link{cutLines}}}
  	
 itemModern arguments:
 
     \item{show.thr.sym}{
     	logical. If \code{TRUE} (default), the plot will show symbols for the item thresholds.
     	}
  \item{thr.sym.cex}{
  	an integer, vector or matrix of numerical values giving the amount by which the threshold symbols should be magnified relative to the default.
  	}
    \item{thr.sym.lwd}{
    	 an integer, vector or matrix of positive numbers specifying the width of the lines used in the threshold symbols.
    	 }
    \item{thr.sym.pch}{
    	 an integer, vector or matrix of integers specifying a symbol or a single character to be used to represent the item thresholds.
    	 }
  \item{thr.sym.col.fg}{
  	 an integer, vector or matrix of numerical values indicating the foreground color to be used in the thresholds labels. 
  	 }
   \item{thr.sym.col.bg}{
   	 an integer, vector or matrix of numerical values indicating the background color to be used in the thresholds labels. 
   	 }
  \item{show.thr.lab}{
  	 logical. If \code{TRUE} (default), the plot will show labels for the item thresholds. 
  	 }
  \item{thr.lab.pos}{
  	 an integer, vector or matrix containing the position in which to display the label for each threshold label. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates. 
  	 }
  \item{thr.lab.text}{
  	  a matrix containing the labels to display for each threshold. In the matrix each row represents an item and each column represents a level.
  	  }
   \item{thr.lab.col}{
   	 a matrix containing the color to display for each threshold label. In the matrix each row represents an item and each column represents a level. 
   	 }
  \item{thr.lab.cex}{
  	 an integer, vector or matrix of numerical values giving the amount by which the threshold labels should be magnified relative to the default.
}
  \item{thr.lab.font}{
an integer, vector or matrix which specifies which font to use for threshold labels. 1 corresponds to plain text , 2 to bold face (the default), 3 to italic and 4 to bold italic.
}
  \item{label.items.rows}{
an integer indicating the number of rows used to display the item labels. Can take values 1 (default), 2 and 3. Useful when item labels are overlaping.
}
  \item{label.items.srt}{
angle of rotation for item labels. It only works if \code{label.item.rows} is set to 1.
}
  \item{label.items}{
a vector of strings containing the labels identifying the items.
}
  \item{label.items.cex}{
  an integer, vector or matrix of numerical values giving the amount by which the threshold labels should be magnified relative to the default.
}
  \item{label.items.ticks}{
  logical. If \code{TRUE} (default), ticks are drawn in the x-axis of the item side.
}
}
\details{
These functions are designed as helper functions for \code{\link{wrightMap}} to draw the item side of a map. When called outside of that function, they can be used to create more customized maps. Possible uses inlcude:

\itemize{
	\item draw an item map on its own
	\item compare two item maps in a single figure
	\item draw a Wright Map with the item side on the left and the person side on the right
	\item etc.
}

The \code{itemClassic} style draws a stacked plot, similar to the Wright Maps available in ConQuest text output files. The \code{itemModern} style is the default style for \code{\link{wrightMap}} which plots each item as a column of difficulty parameters. The \code{itemHist} style plots a histogram.

}

\author{Rebecca Freund and David Torres Irribarra}
\note{
When combining with a \code{\link{person.side}} function, note that those functions use \code{\link{split.screen}}, which are incompatible with \code{\link{layout}} and some other plotting functions. Note also that all graphs on a single plot should usually have their yRange explicitly specified to ensure that values are comparable across plots. To plot data from ConQuest output, use \code{\link{itemData}} first to extract the data table.
}


\seealso{
\code{\link{person.side}}
\code{\link{itemData}}
\code{\link{wrightMap}}
}
\examples{
	
	#As a call from wrightMap:
	
	## Setting up labels, colors and symbols

  thresholds.labels <- data.frame(
    l1 = paste('Lev',rep(1,20),sep = ''),
    l2 = paste('Lev',rep(2,20),sep = ''),
    l3 = paste('Lev',rep(3,20),sep = ''),
    l4 = paste('Lev',rep(4,20),sep = ''))

  thresholds.colors <- data.frame(
    l1 = rep(  'green',20),
    l2 = rep(    'red',20),
    l3 = rep( 'yellow',20),
    l4 = rep(   'blue',20))

  thresholds.symbols <- data.frame(
    l1 = rep( 15,20),
    l2 = rep( 16,20),
    l3 = rep( 17,20),
    l4 = rep( 18,20))

wrightMap( uni.proficiency, thresholds
  , thr.lab.text = thresholds.labels
  , thr.lab.col = thresholds.colors
  , thr.sym.pch = thresholds.symbols
  )
	
	#As  direct call:
	
	## Plotting results of a unidimensional Rating Scale Model

  items.loc <- sort( rnorm( 10))
  thresholds <- data.frame(
    l1 = items.loc - 0.5 ,
    l2 = items.loc - 0.25,
    l3 = items.loc + 0.25,
    l4 = items.loc + 0.5)

	itemModern(thresholds)
	itemClassic(thresholds)
	itemHist(thresholds)
	
	## Plotting ConQuest results
	
	fpath <- system.file("extdata", package="WrightMap")
	model1 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"))
	m1.item <- itemData(model1)
	
	itemModern(m1.item, label.items.srt= 90, oma = c(3,0,0,3)) #control of oma allows us to give more space to longer item names
	itemClassic(m1.item)
	itemHist(m1.item)
	
	## Creating a Wright Map with item side on the left
	
	multi.proficiency <- data.frame(
    d1 = rnorm(1000, mean =  -0.5, sd = 1),
    d2 = rnorm(1000, mean =   0.0, sd = 1),
    d3 = rnorm(1000, mean =  +0.5, sd = 1))
    
    # split.screen: Set up a split screen with the left side 80 percent of the screen
    # yRange = c(-3,4): Set the yRange to be the same for both sides
    # axis.logits.side = "L": Move the item logit axis to the left
    # oma = c(0,0,0,2): Adjust the spacing between the graphs
    # mtext("Wright Map", side = 3, font = 2, line = 1): add a title
    # screen(2): Start drawing on the second screen
    
    split.screen(figs = matrix(c(0,.8,0,1,.8,1,0,1),ncol = 4, byrow = TRUE)) 
    itemModern(thresholds, yRange = c(-3,4), show.axis.logits = "L", oma = c(0,0,0,2))
    mtext("Wright Map", side = 3, font = 2, line = 1)
    screen(2)
    personHist(multi.proficiency, axis.persons = "",yRange = c(-3,4), axis.logits = "Persons", show.axis.logits = FALSE)
    
    
    ## Creating a multidimensional Wright Map with each dimension separate
    
    ## Mock results
  
    d1 = rnorm(1000, mean =  -0.5, sd = 1)
    d2 = rnorm(1000, mean =   0.0, sd = 1)

	dim1.diff <- rnorm(5)
	dim2.diff <- rnorm(5)

  
	split.screen(figs = matrix(c(0,.1,0,1,
  								.12,.6,0,1,
  								.5,.6,0,1,
  								.5,1,0,1),ncol = 4,byrow = TRUE))
  							  
	personDens(d1,yRange = c(-3,3),show.axis.logits = FALSE,axis.logits = "")
	screen(2)
	itemModern(dim1.diff,yRange = c(-3,3),show.axis.logits = FALSE)
	mtext("Wright Map", side = 3, font = 2, line = 1)
	screen(3)
	personDens(d2,yRange = c(-3,3),show.axis.logits = FALSE,axis.logits = "",axis.persons = "",dim.names = "Dim2")
	screen(4)
	itemModern(dim2.diff,yRange = c(-3,3),show.axis.logits = FALSE,label.items = paste("Item",6:10))
	
	

	
}
