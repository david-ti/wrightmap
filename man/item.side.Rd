\name{item.side}
\alias{item.side}
\alias{itemClassic}
\alias{itemModern}
\alias{itemHist}

\title{Wright Map Item Sides}
\description{Draw the item side of a Wright Map in a variety of styles. Intended to be primarily called by \code{\link{wrightMap}}, but also available for use on their own.}
\usage{
itemModern(thr, yRange = NULL, axis.items = "Items", show.thr.sym = TRUE
, thr.sym.cex = 0.8, thr.sym.lwd = 1, thr.sym.pch = 23
, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3)
, show.thr.lab = TRUE, thr.lab.pos = c(2, 4), thr.lab.text = NULL
, thr.lab.col = "black", thr.lab.cex = 0.5, thr.lab.font = 2, label.items.rows = 1
, label.items.srt = 0, label.items = NULL, label.items.cex = 0.6
, label.items.ticks = TRUE, axis.logits = "Logits", show.axis.logits = "R"
, axis.logits.cex = 0.7, oma = c(0, 0, 0, 3), cutpoints = NULL
, vertLines = FALSE, vertLines.par = list(), cutpoints.par = list()
, thr.sym.par = list(), thr.lab.par = list(), label.items.par = list()
, axis.logits.par = list(), logits.text.par = list(), ...)

itemClassic(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits"
, show.axis.logits = "R", axis.logits.cex = 0.7, oma = c(0, 0, 0, 3)
, cutpoints = NULL, cutpoints.par = list(), label.items = NULL, label.steps = NULL
, label.sep = ".", thr.lab.sep = " | ", thr.lab.par = list(), axis.logits.par = list()
, logits.text.par = list(), pad.char = "_", font.family = "mono", ...)

itemHist(thr, yRange = NULL, axis.items = "Items", axis.logits = "Logits"
, show.axis.logits = "R", axis.logits.cex = 0.7, oma = c(0, 0, 0, 3)
, cutpoints = NULL, axis.logits.par = list(), logits.text.par = list()
, cutpoints.par = list(), ...)
}
\arguments{
General arguments:
  \item{thr}{A vector or matrix of threshold parameters. If a matrix, items should be in rows and steps in columns.}
  \item{yRange}{A vector with 2 elements specifying the lower and upper limits of the plot's y-axis.}
  \item{axis.items}{The title of the x-axis.}
  \item{axis.logits}{The title of the y-axis.}
  \item{show.axis.logits}{If set to "R" or "L", draws a logit axis on the right or left. Will also draw an axis on the right if the value is \code{TRUE}. If any other value, the axis is not drawn.}
  \item{oma}{Values to use for the \code{oma} parameter (see \code{\link{par}}).}
  \item{cutpoints}{Values at which to draw horizontal lines (see \code{\link{cutLines}}).}
  \item{\dots}{Additional arguments passed to \code{\link{cutLines}}.}

Specific to \code{itemModern}:
  \item{show.thr.sym}{Logical. If \code{TRUE} (default), the plot will show symbols for the item thresholds.}
  \item{thr.sym.cex}{An integer, vector, or matrix of numerical values indicating the magnification of the threshold symbols.}
  \item{thr.sym.lwd}{An integer, vector, or matrix of positive numbers specifying the width of the lines used in the threshold symbols.}
  \item{thr.sym.pch}{An integer, vector, or matrix specifying a symbol or single character used to represent item thresholds.}
  \item{thr.sym.col.fg}{An integer, vector, or matrix of numerical values indicating the foreground color for the threshold symbols.}
  \item{thr.sym.col.bg}{An integer, vector, or matrix of numerical values indicating the background color for the threshold symbols.}
  \item{show.thr.lab}{Logical. If \code{TRUE} (default), the plot will show labels for the item thresholds.}
  \item{thr.lab.pos}{An integer, vector, or matrix indicating the position for displaying the threshold labels. Values 1, 2, 3, and 4 indicate below, left, above, and right, respectively.}
  \item{thr.lab.text}{A matrix containing the text labels for the item thresholds, where each row represents an item, and each column represents a level.}
  \item{thr.lab.col}{A matrix specifying the color for each threshold label.}
  \item{thr.lab.cex}{A numerical value indicating the magnification factor for the threshold labels.}
  \item{thr.lab.font}{An integer or matrix specifying the font type for the threshold labels. 1 = plain, 2 = bold, 3 = italic, 4 = bold italic.}
  \item{label.items.rows}{An integer specifying the number of rows used to display the item labels (default: 1). Useful when item labels overlap.}
  \item{label.items.srt}{Angle of rotation for item labels. Only applicable if \code{label.items.rows} is set to 1.}
  \item{label.items}{A vector of strings representing the item labels.}
  \item{label.items.cex}{A numerical value specifying the magnification factor for the item labels.}
  \item{label.items.ticks}{Logical. If \code{TRUE} (default), ticks are drawn on the x-axis for the item side.}
  \item{vertLines}{Logical. If \code{TRUE}, vertical lines are drawn to mark all symbols associated with an item.}
  \item{vertLines.par}{A list of parameters (e.g., \code{col}, \code{lwd}, \code{lty}) to control the appearance of the vertical lines.}
  \item{cutpoints.par}{A list of parameters (e.g., \code{col}, \code{lwd}, \code{lty}) to customize the cutpoint lines.}
  \item{thr.sym.par}{A list of parameters to further customize threshold symbols.}
  \item{thr.lab.par}{A list of parameters to further customize threshold labels.}
  \item{label.items.par}{A list of parameters to further customize item labels.}
  \item{axis.logits.par}{A list of parameters for customizing the logit axis.}
  \item{logits.text.par}{A list of parameters for customizing the logit axis text.}

Specific to \code{itemClassic}:
  \item{label.items}{A vector of strings representing the item labels.}
  \item{label.steps}{A vector specifying the labels for the threshold steps.}
  \item{label.sep}{A string indicating the separator used between item labels and step labels.}
  \item{thr.lab.sep}{A string indicating the separator used between threshold labels for different items.}
  \item{pad.char}{A character used to pad item labels to a fixed width.}
  \item{font.family}{A string specifying the font family for the item labels (default: \code{mono}).}

Specific to \code{itemHist}:
  \item{axis.logits.par}{A list of parameters to customize the logit axis in the item histogram.}
  \item{logits.text.par}{A list of parameters to customize the text of the logit axis in the item histogram.}
  \item{cutpoints.par}{A list of parameters to customize the cutpoint lines in the item histogram.}
}
\details{
These functions are designed as helper functions for \code{\link{wrightMap}} to draw the item side of a map. When called outside of that function, they can be used to create more customized maps. Possible uses include:

\itemize{
  \item Draw an item map on its own.
  \item Compare two item maps in a single figure.
  \item Draw a Wright Map with the item side on the left and the person side on the right.
}
}

\author{Rebecca Freund and David Torres Irribarra}
\note{
When combining with a \code{\link{person.side}} function, note that those functions use \code{\link{split.screen}}, which are incompatible with \code{\link{layout}} and some other plotting functions. Ensure that all graphs on a single plot have their \code{yRange} explicitly specified to maintain comparable values across plots.
}

\seealso{
\code{\link{wrightMap}}, \code{\link{person.side}}, \code{\link{itemData}}
}
\examples{

#As a call from wrightMap:

## Mock results
uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)

items.loc <- sort(rnorm(20))
thresholds <- data.frame(
  l1 = items.loc - 0.5,
  l2 = items.loc - 0.25,
  l3 = items.loc + 0.25,
  l4 = items.loc + 0.5
)

## Setting up labels, colors, and symbols
thresholds.labels <- data.frame(
  l1 = paste('Lev', rep(1, 20), sep = ''),
  l2 = paste('Lev', rep(2, 20), sep = ''),
  l3 = paste('Lev', rep(3, 20), sep = ''),
  l4 = paste('Lev', rep(4, 20), sep = '')
)

thresholds.colors <- data.frame(
  l1 = rep('green', 20),
  l2 = rep('red', 20),
  l3 = rep('yellow', 20),
  l4 = rep('blue', 20)
)

thresholds.symbols <- data.frame(
  l1 = rep(15, 20),
  l2 = rep(16, 20),
  l3 = rep(17, 20),
  l4 = rep(18, 20)
)

wrightMap(uni.proficiency, thresholds,
  thr.lab.text = thresholds.labels,
  thr.lab.col = thresholds.colors,
  thr.sym.pch = thresholds.symbols
)

# Direct call:

## Plotting results of a unidimensional Rating Scale Model
items.loc <- sort(rnorm(20))
thresholds <- data.frame(
  l1 = items.loc - 0.5,
  l2 = items.loc - 0.25,
  l3 = items.loc + 0.25,
  l4 = items.loc + 0.5
)

itemModern(thresholds)
itemClassic(thresholds)
itemHist(thresholds)
}