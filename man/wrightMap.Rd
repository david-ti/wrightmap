\name{wrightMap}
\alias{wrightMap}
\alias{wrightMap.default}
\alias{wrightMap.CQmodel}
\alias{wrightMap.character}
\alias{plot.CQmodel}
\title{
wrightMap: item-person maps
}
\description{
This function allows the easy generation of `Wright Maps' (named after Ben Wright), also known as item-person maps to display unidimensional and multidimensional assessment results.These maps represent simultaneously the proficiency distribution of respondents and the item difficulty parameters as estimated by a model of the Rasch Family.
}
\usage{
wrightMap(thetas, \dots)
\method{wrightMap}{default}(thetas, thresholds, use.hist = TRUE, 
main.title = "Wright Map", axis.logits = "Logits", axis.persons = "Respondents", 
axis.items = "Items", label.items = NULL, label.items.rows = 1, label.items.srt = 0,
 label.items.ticks = TRUE, show.thr.lab = TRUE, show.thr.sym = TRUE, thr.lab.text = NULL,
  thr.lab.col = "black", thr.lab.pos = c(2, 4), thr.lab.font = 2, thr.lab.cex = 0.85, 
  thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), 
  thr.sym.col.bg = rgb(0, 0, 0, 0.3), thr.sym.cex = 1.2, thr.sym.lwd = 1, 
  dim.names = NULL, dim.color = NULL, dim.lab.side = 3, dim.lab.adj = 0.5, 
  breaks = "FD", min.logit.pad = 0.25, max.logit.pad = 0.25, min.l = NULL, 
  max.l = NULL, item.prop = 0.8, return.thresholds = TRUE, new.quartz = FALSE, ...)
\method{wrightMap}{CQmodel}(thetas, item.table = NULL, interactions = NULL, 
step.table = NULL, type = "default", label.items = NULL, main.title = NULL, 
thr.lab.text = NULL, dim.names = NULL, \dots)
\method{plot}{CQmodel}(x, \dots)
\method{wrightMap}{character}(thetas, thresholds, p.type = NULL, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

wrightMap parameters: 

  \item{thetas}{
    a vector, matrix or data frame of person parameter estimates. Can also be a character string specifying a ConQuest output file of person parameter estimates, or a CQmodel object.
}
  \item{thresholds}{
    matrix or data frame of item parameter estimates. Can also be a character string specifying a ConQuest show file.
}
  \item{use.hist}{
    logical. If \code{TRUE} (default), the person distribution is represented using a histogram. If \code{FALSE} a density is used.
}
  \item{main.title}{
  title of the Wright Map.
}
  \item{axis.logits}{
  title of the y-axis on the items side.
}
  \item{axis.persons}{
  title of the y-axis on the person side.
}
  \item{axis.items}{
  title of the x-axis on the item side.
}
  \item{label.items}{
  a vector of strings containing the labels identifying the items.
}
  \item{label.items.rows}{
  an integer indicating the number of rows used to display the item labels. Can take values 1 (default), 2 and 3. Useful when item labels are overlaping.
}
  \item{label.items.srt}{
  angle of rotation for item labels. It only works if \code{label.item.rows} is set to 1.
}
  \item{label.items.ticks}{
  logical. If \code{TRUE} (default), ticks are drawn in the x-axis of the item side.
}
  \item{show.thr.lab}{
  logical. If \code{TRUE} (default), the Wirght Map will show labels for the item thresholds.
}
  \item{show.thr.sym}{
  logical. If \code{TRUE} (default), the Wirght Map will show symbols for the item thresholds.
}
  \item{thr.lab.text}{
  a matrix containing the labels to display for each threshold. In the matrix each row represents an item and each column represents a level.
}
  \item{thr.lab.col}{
  a matrix containing the color to display for each threshold label. In the matrix each row represents an item and each column represents a level.
}
  \item{thr.lab.pos}{
  an integer, vector or matrix containing the position in which to display the label for each threshold label. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
}
  \item{thr.lab.font}{
  an integer, vector or matrix which specifies which font to use for threshold labels. 1 corresponds to plain text , 2 to bold face (the default), 3 to italic and 4 to bold italic. 
}
  \item{thr.lab.cex}{
  an integer, vector or matrix of numerical values giving the amount by which the threshold labels should be magnified relative to the default.
}
  \item{thr.sym.pch}{
  an integer, vector or matrix of integer specifying a symbol or a single character to be used to represent the item thresholds.
}
  \item{thr.sym.col.fg}{
  an integer, vector or matrix of numerical values indicating the foreground color to be used in the thresholds labels.
}
  \item{thr.sym.col.bg}{
  an integer, vector or matrix of numerical values indicating the background color to be used in the thresholds labels.
}
  \item{thr.sym.cex}{
  an integer, vector or matrix of numerical values giving the amount by which the threshold symbols should be magnified relative to the default.
}
  \item{thr.sym.lwd}{
  an integer, vector or matrix of positive numbers specifying the width of the lines used in the threshold symbols.
}
  \item{dim.names}{
  a string or a vector of strings containing the names of each one of the dimensions.
}
  \item{dim.color}{
  a numerical value or vector indicating the colors to be used for representing the density curves for each dimension. It only works when \code{use.hist} is set to \code{FALSE}.
}
  \item{dim.lab.side}{
  an integer specifyng in which side to plot the dimension names. Values of 1, 2, 3 (default) and 4, respectively indicate positions below, to the left of, above and to the right of the person distributions.
}
  \item{dim.lab.adj}{
  a numerical value adjusting the position of the dimension names.
}
  \item{breaks}{
  See \code{\link{hist}}). This argument is passed directly to hist, so it will accept all the options detailed that functions' manual.
}
  \item{min.logit.pad}{
  numeric value indicating how much of the lower end of the logit scale should be included in the plot.
}
  \item{max.logit.pad}{
  numeric value indicating how much of the upper end of the logit scale should be included in the plot.
}
  \item{min.l}{
  numeric value for fixing the lower end of the logit scale. It overrides the automatic detection of the range and the \code{min.logit.pad} correction.
}
  \item{max.l}{
  numeric value for fixing the upper end of the logit scale. It overrides the automatic detection of the range and the \code{max.logit.pad} correction.
}
  \item{item.prop}{
  numeric value greater than 0 and smaller than 1 indicating the proportion of the plot to be allocated to the item part of the Wright Map.
}
  \item{return.thresholds}{
  logical. Determines whether the to return or not the numeric values used to position the parameters on the item side of the Wright Map. Enabled by default.
  }
  \item{new.quartz}{
  logical. Determines whether the wrightMap will be created on a new graphical device or if it will reuse one already open. By default is set to \code{FALSE} to avoid creating new devices.
}
  \item{\dots}{
  Additional arguments to pass to wrightMap.
}

Parameters for creating a WrightMap directly from ConQuest output files:

  \item{item.table}{
  Name of RMP table to use for the main effect of the item parameters.
}
  \item{interactions}{
  Name of RMP interaction table to use in addition to \code{item.table}.
}
  \item{step.table}{
  Name of RMP table to use in addition to \code{item.table}.
}
  \item{type}{
  Indicates wether to use \code{thresholds} or \code{deltas} for generating the Wright Map.
}
  \item{p.type}{
  Type of person parameter file
}
  
 wrightMap can also be called by passing a CQmodel object to plot:
 
 \item{x}{ CQmodel object to pass to plot}



}
\details{
Note that the \code{item.table}, \code{interactions}, and \code{step.table} parameters must be the exact name of specific RMP tables, and you can't specify an interactions table or a step table without also specifying an item table (although JUST an item table is fine). If your model equation is more complicated, you will have to either use a GIN table or specify in the function call which tables to use for what. A model of the form item + item * step + booklet, for example, will not run unless there is a GIN table or you have defined at least the item.table.}

\references{
  Wilson, M. (2005). Constructing measures: An item response modeling approach. 
  Wright, B. D., \& Stone, M. H. (1979). \emph{Best test design.} Chicago: Mesa Press. 
}

\author{David Torres Irribarra and Rebecca Freund}


\examples{

# Plotting results of a unidimensional Rasch Model

## Mock results
  uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)
  difficulties  <- sort( rnorm( 20))

## Default map
wrightMap( uni.proficiency, difficulties)

## Density version
wrightMap( uni.proficiency, difficulties, use.hist = FALSE)

## Generating a Wirght Map from Conquest output
\dontrun{model1 <- CQmodel("ex2.eap","ex2.shw")}
\dontrun{wrightMap(model1)}
\dontrun{wrightMap("ex2.eap", "ex2.shw")}

# Plotting results of a multidimensional Rasch Model

## Mock results
  multi.proficiency <- data.frame(
    d1 = rnorm(1000, mean =  -0.5, sd = 1),
    d2 = rnorm(1000, mean =   0.0, sd = 1),
    d3 = rnorm(1000, mean =  +0.5, sd = 1))

  difficulties  <- sort( rnorm( 20))

wrightMap( multi.proficiency, difficulties)

# Plotting results of a unidimensional Rating Scale Model

## Mock results
  uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)

  items.loc <- sort( rnorm( 20))
  thresholds <- data.frame(
    l1 = items.loc - 0.5 ,
    l2 = items.loc - 0.25,
    l3 = items.loc + 0.25,
    l4 = items.loc + 0.5)

wrightMap( uni.proficiency, thresholds)

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
  
    ####ConQuest integration###
  
  	fpath <- system.file("extdata", package="WrightMap")

 # Partial credit model
	model1 <- CQmodel(p.est = file.path(fpath,"ex2.eap"), show = file.path(fpath,"ex2.shw"))
	wrightMap(model1) # Using GIN tables (partial credit model)
	
	model2 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"))
	
  # Making deltas if there are no GIN tables (partial credit model)
  wrightMap(model2, label.items.row = 2) 

  # Making thresholds if there are no GIN tables (partial credit model)
	wrightMap(model2, type = "thresholds", label.items.row = 2) 
	
	model3 <- CQmodel(file.path(fpath,"ex2b.eap"), file.path(fpath,"ex2b-2.shw"))
	wrightMap(model3, label.items.row = 2) # Rating scale model
	
  # Complex model
	model4 <- CQmodel(file.path(fpath,"ex4a.mle"), file.path(fpath,"ex4a.shw")) 
	wrightMap(model4, min.logit.pad = -29, use.hist = FALSE)
	wrightMap(model4, item.table = "rater")
	wrightMap(model4, item.table = "rater", interactions = "rater*topic", 
  step.table = "topic")
	
  ### Skip CQmodel
	wrightMap(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"), 
  label.items.row = 3) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}

