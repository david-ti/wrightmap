\name{person.side}
\alias{person.side}
\alias{personHist}
\alias{personDens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wright Map person sides
}
\description{
Draw the person side of a Wright Map in a variety of styles. Intended to be primarily called by \code{\link{wrightMap}}, but also available for use on their own.
}
\usage{
personHist(thetas, yRange = NULL, breaks = "FD", dim.lab.cex = 0.6, dim.lab.side = 3
  , dim.lab.adj = 0.5, dim.names = NULL, dim.color = "white", person.points = NULL
  , person.range = NULL, p.point.col = "gray45", p.range.col = "gray75"
  , axis.persons = "Respondents", show.axis.persons = FALSE, axis.persons.par = list(), oma = c(0, 5, 0, 5), axis.logits = "Logits"
  , show.axis.logits = TRUE,...)

personDens(thetas, yRange = NULL, dim.lab.cex = 0.6, dim.lab.side = 3, dim.lab.adj = 0.5
  ,dim.names = NULL,dim.color = "black",person.points = NULL, person.range = NULL
  , p.point.col = "black", p.range.col = "gray70", oma = c(0, 5, 0, 5)
  , axis.logits = "Logits", show.axis.logits = TRUE, axis.persons = "Respondents",...)
}

\arguments{
  \item{thetas}{
vector or matrix of person parameters. If a matrix, persons should be the rows and dimensions the columns.
}
  \item{yRange}{
vector with 2 elements specifying the lower and upper limits of the plot's y-axis.
}
  \item{dim.lab.cex}{
An integer specifying the amount the dimension labels should be magnified relative to the default.
}
  \item{dim.lab.side}{
An integer specifying which side to plot the dimension names. Values of 1, 2, 3 (default), and 4, respectively, indicate positions below, to the left of, above, and to the right of the person distributions.
}
  \item{dim.lab.adj}{
A numerical value adjusting the position of the dimension names.
}
  \item{dim.names}{
A string or a vector of strings containing the names of each dimension.
}
  \item{dim.color}{
A numerical value or vector indicating the colors to be used for representing each dimension.
}
  \item{person.points}{
A vector of individual values to highlight.
}
  \item{person.range}{
A pair of values, an even-length vector, or a matrix with two rows. Values indicate the start and endpoints of ranges to highlight. If a matrix, the first row should be lower bounds and the second row upper bounds of the ranges. If a vector, the values should alternate: (lower1, upper1, lower2, upper2, ...).
}
  \item{p.point.col}{
A string or vector of strings indicating the color to use for the highlighted points.
}
  \item{p.range.col}{
A string or vector of strings indicating the color to use for the highlighted ranges.
}
  \item{axis.persons}{
The title of the y-axis on the left side.
}
  \item{show.axis.persons}{
Logical indicating whether to show the persons axis on the left side. Default is \code{FALSE}.
}
  \item{axis.persons.par}{
A list of parameters to customize the appearance of the persons axis. This can include options such as \code{cex.axis}, \code{las}, and others as supported by the \code{\link{axis}} function.
}
  \item{oma}{
Values to use for the \code{oma} parameter (see \code{\link{par}}).
}
  \item{show.axis.logits}{
Logical indicating whether to show the logit axis.
}
  \item{axis.logits}{
The title of the y-axis on the right side.
}
  \item{\dots}{
Additional arguments passed to other methods (e.g., \code{\link{cutLines}}).
}

For \code{personHist}:
  \item{breaks}{
A parameter passed to the \code{\link{hist}} function to define the binning of the histogram.
}
}
\details{
These functions are designed as helper functions for \code{\link{wrightMap}} and \code{\link{ppPlot}} to draw the person side of a map. When called outside of those functions, they can be used to create more customized maps. Possible uses include:

\itemize{
  \item Drawing a person map on its own
  \item Comparing two person maps in a single figure
  \item Drawing a Wright Map with the item side on the left and the person side on the right
}

The \code{personHist} style, the default, draws the person distribution as a histogram. The \code{personDens} style draws a density plot.

The \code{person.points}, \code{person.range}, \code{p.point.col}, and \code{p.range.col} parameters can highlight specific values or ranges, either when called directly or via \code{\link{ppPlot}}.
}

\author{
Rebecca Freund and David Torres Irribarra
}

\seealso{
\code{\link{item.side}}
\code{\link{personData}}
\code{\link{wrightMap}}
\code{\link{ppPlot}}
}

\examples{

  # Creating a Wright Map with item side on the left

  multi.proficiency <- data.frame(
    d1 = rnorm(1000, mean =  -0.5, sd = 1),
    d2 = rnorm(1000, mean =   0.0, sd = 1),
    d3 = rnorm(1000, mean =  +0.5, sd = 1))

    
  items.loc <- sort( rnorm( 20))
  thresholds <- data.frame(
    l1 = items.loc - 0.5 ,
    l2 = items.loc - 0.25,
    l3 = items.loc + 0.25,
    l4 = items.loc + 0.5)
    
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
    personHist(multi.proficiency, axis.persons = "", yRange = c(-3,4), axis.logits = "Persons", show.axis.logits = FALSE)
    
    ## Creating a multidimensional Wright Map with each dimension separate

    ## Mock results

    d1 = rnorm(1000, mean =  -0.5, sd = 1)
    d2 = rnorm(1000, mean =   0.0, sd = 1)

    dim1.diff <- rnorm(5)
    dim2.diff <- rnorm(5)
  
    split.screen(figs = matrix(c(0,.1,0,1, .12,.6,0,1, .5,.6,0,1, .5,1,0,1),ncol = 4,byrow = TRUE))
                  
    personDens(d1, yRange = c(-3,3), show.axis.logits = FALSE, axis.logits = "")
    screen(2)
    itemModern(dim1.diff, yRange = c(-3,3), show.axis.logits = FALSE)
    mtext("Wright Map", side = 3, font = 2, line = 1)
    screen(3)
    personDens(d2, yRange = c(-3,3), show.axis.logits = FALSE, axis.logits = "", axis.persons = "", dim.names = "Dim2")
    screen(4)
    itemModern(dim2.diff, yRange = c(-3,3), show.axis.logits = FALSE, label.items = paste("Item", 6:10))

}