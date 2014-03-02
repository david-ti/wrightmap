\name{wrightMap}
\alias{wrightMap}
\alias{wrightMap.default}
\alias{wrightMap.CQmodel}
\alias{wrightMap.character}
\alias{plot.CQmodel}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wrightMap(x, \dots)
\method{wrightMap}{default}(thetas, thresholds, use.hist = TRUE, main.title = "Wright Map", axis.logits = "Logits", axis.persons = "Respondents", axis.items = "Items", label.items = NULL, label.items.rows = 1, label.items.srt = 0, label.items.ticks = TRUE, show.thr.lab = TRUE, show.thr.sym = TRUE, thr.lab.text = NULL, thr.lab.col = "black", thr.lab.pos = c(2, 4), thr.lab.font = 2, thr.lab.cex = 0.85, thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3), thr.sym.cex = 1.2, thr.sym.lwd = 1, dim.names = NULL, dim.color = "black", dim.lab.side = 3, dim.lab.adj = 0.5, hist.nclass = "FD", min.logit.pad = 0.25, max.logit.pad = 0.25, item.prop = 0.2)
\method{wrightMap}{CQmodel}(model, tables = NULL, label.items = NULL, main.title = NULL, thr.lab.text = NULL, dim.names = NULL, \dots)
\method{plot}{CQmodel}(model, \dots)
\method{wrightMap}{character}(show, p.est, p.type = NULL, \dots)

}

\arguments{

wrightMap parameters: 
  \item{thetas}{
    a vector, matrix or data frame of person parameter estimates.
}
  \item{thresholds}{
    matrix or data frame of item parameter estimates.
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
  \item{hist.nclass}{
  See \code{\link{hist}}). Algorithm used to generate the brakes when \code{use.hist} is \code{TRUE}. Accepts ``FD'' / ``Freedman-Diaconis'' (default), ``Scott'' and ``Sturges''.
}
  \item{min.logit.pad}{
  numeric value indicating how much of the lower end of the logit scale should be included in the plot.
}
  \item{max.logit.pad}{
  numeric value indicating how much of the upper end of the logit scale should be included in the plot.
}
  \item{item.prop}{
  numeric value greater than 0 and smaller than 1 indicating the proportion of the plot to be allocated to the item part of the Wright Map.
}

Additional parameters when creating a Wright Map from a CQmodel object:

  \item{model}{ CQmodel object containing both item and person parameters. }
  \item{tables}{ Name of RMP table(s) to use for the item parameters. If no table is specified, the function will use the GIN table if present, or look for a table called $item. }
  
Parameters for creating a WrightMap directly from ConQuest output files:

  \item{show}{ ConQuest show file. }
  \item{p.est}{ Conquest person parameters file (EAPs, MLEs, etc.). }

}

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

## Histogram version
wrightMap( uni.proficiency, difficulties, use.hist = TRUE)

# Plotting results of a multidimensional Rasch Model

## Mock results
  multi.proficiency <- data.frame(
    d1 = rnorm(1000, mean =  -0.5, sd = 1)
    d2 = rnorm(1000, mean =   0.0, sd = 1)
    d3 = rnorm(1000, mean =  +0.5, sd = 1))

  difficulties  <- sort( rnorm( 20))

wrightMap( multi.proficiency, difficulties)

# Plotting results of a unidimensional Rating Scale Model

## Mock results
  uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)

  items.loc <- sort( rnorm( 20))
  thresholds <- data.frame(
    l1 = items.loc - 0.5
    l2 = items.loc - 0.25
    l3 = items.loc + 0.25
    l4 = items.loc + 0.5)

wrightMap( uni.proficiency, thresholds)

## Setting up labels, colors and symbols

  thresholds.labels <- data.frame(
    l1 = paste('Lev',rep(1,20),sep = '')
    l2 = paste('Lev',rep(2,20),sep = '')
    l3 = paste('Lev',rep(3,20),sep = '')
    l4 = paste('Lev',rep(4,20),sep = ''))

  thresholds.colors <- data.frame(
    l1 = rep(  'green',20)
    l2 = rep(    'red',20)
    l3 = rep( 'yellow',20)
    l4 = rep(   'blue',20))

  thresholds.symbols <- data.frame(
    l1 = rep( 15,20)
    l2 = rep( 16,20)
    l3 = rep( 17,20)
    l4 = rep( 18,20))

wrightMap( uni.proficiency, thresholds
  , thr.lab.text = thresholds.labels
  , thr.lab.col = thresholds.colors
  , thr.sym.pch = thresholds.symbols
  )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Wright Map}
\keyword{IRT}
\keyword{Item Response Models}
\keyword{Rasch Model}

