\name{ppPlot}
\alias{ppPlot}
\alias{cutLines}
\alias{kidmap}

\title{Person probability plots
}
\description{
Plots a Wright Map for a single person (similar to a kidmap). On the person side, highlights their estimated ability and a range of one standard error. On the item side, draws lines representing item difficulties at which they are expected to have a 20\%, 40\%, 50\%, 60\%, and 80\% chance of success.}
\usage{
ppPlot(thetas, thresholds, est, SE, main.title = "Person Probability Plot", cut.left = 0, cut.right = .94, cut.lab.adj = c(1,.5),\dots)

cutLines(cutpoints = NULL,cut.left = 0, cut.right = 1, cut.lab.text = NULL, cut.lab.adj = c(0,1),\dots)
}
\arguments{
  \item{thetas}{
    a vector, matrix or data frame of person parameter estimates. Can also be a character string specifying a ConQuest output file of person parameter estimates, or a CQmodel object. Will be sent to the function \code{\link{personData}}.
}
  \item{thresholds}{
    matrix or data frame of item parameter estimates. Can also be a character string specifying a ConQuest show file. Will be sent to the function \code{\link{itemData}}.
 }
 \item{est}{
 	estimated ability of the person
 }
 \item{SE}{
 	standard error of the estimate
 }
 \item{main.title}{
  title of the Person Probability Plot.
}
\item{cut.left}{
	value between 0 and 1 describing where to place the lefthand side of the cutpoints, as a fraction of the item plot.
 }
 \item{cut.right}{
	value between 0 and 1 describing where to place the righthand side of the cutpoints, as a fraction of the item plot.
 }
 \item{cut.lab.adj}{
 	similar to the \code{adj} parameter in \code{\link{text}}, describes where to place the text for the cutpoints as a pair of values between 0 and 1 in terms of left-right and up-down alignment. Left-right alignment is 0 for the left side of the item plot and 1 for the right side, while up-down alignment is 0 for below the line and 1 for above the line.
 }
 \item{cutpoints}{
 	argument to \code{cutLines} when called through \code{wrightMap} or one of the \code{\link{item.side}} functions. Specifies locations of cutlines. When \code{cutLines} is called through \code{ppPlot}, the cutpoints are calculated rather than specified. 
 }
 \item{cut.lab.text}{
 	argument to \code{cutLines} when called through \code{wrightMap} or one of the \code{\link{item.side}} functions. Specifies text to appear for each cut line. When \code{cutLines} is called through \code{ppPlot}, the text is always the percent chance of success given the estimated ability level and difficulty location. 	
 }
 
 \item{\dots}{
 	additional arguments to pass to \code{\link{wrightMap}} or its associated functions.
 }
}
\details{
	The \code{ppPlot} function is a wrapper for \code{\link{wrightMap}} that is specifically designed for person probability plots, and as such has access to all the parameters of wrightMap and its associated functions. It uses the \code{person.points}, \code{person.range}, \code{p.point.col}, and \code{p.range.col} parameters on the \code{\link{person.side}} function to draw a range of one standard error around the estimated ability level. On the item side, it calculates at what item difficulty the respondent is expected to have a 20\%, 40\%, 50\%, 60\%, and 80\% chance of success and then uses the \code{cutLines} function to illustrate these cutpoints. The cutLines function should not be called on its own and may be hidden in future versions. It is included here to show the available parameters, which can be included in a call to \code{wrightMap} or any of the \code{item.side} functions.
	}


\author{
David Torres Irribarra and Rebecca Freund}

\seealso{
 \code{\link{wrightMap}}
}
\examples{
	
	fpath <- system.file("extdata", package="WrightMap")
	model1 <- CQmodel(p.est = file.path(fpath,"ex2.eap"), show = file.path(fpath,"ex2.shw"))
	
	#Person histogram, modern item
	ppPlot(model1,est = 0, SE = 1) 
	
	#Person density, classic item
	ppPlot(model1,est = 0, SE = 1, person.side = personDens,item.side = itemClassic) 
	
}
