\name{fitgraph}
\alias{fitgraph}
\alias{fitgraph.default}
\alias{fitgraph.CQmodel}
\alias{fitgraph.character}
\title{
Item Fit Graphs
}
\description{
	This function creates a graphical summary of the item fit information.
}
\usage{

\method{fitgraph}{default}(fitEst, fitLB, fitUB, itemLabels, mainTitle = "Fit Plot", 
pch = 18, fitColours = c("gray70", "gray60", "gray50", "gray40", "gray0"), 
xlab = "Items", cex = 1.25, ...)
\method{fitgraph}{numeric}(fitEst, fitLB, fitUB, itemLabels, mainTitle = "Fit Plot", 
pch = 18, fitColours = c("gray70", "gray60", "gray50", "gray40", "gray0"), 
xlab = "Items", cex = 1.25, ...)
\method{fitgraph}{CQmodel}(fitEst, table = NULL, fit.type = "W", itemLabels = NULL, ...)
\method{fitgraph}{character}(fitEst, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	
fitgraph arguments: 	
	
  \item{fitEst}{
	vector of item fit estimates. Could also be a CQmodel object or name of a ConQuest show file.
}
  \item{fitLB}{
	vector of lower bounds for critical intervals for each item.
}
  \item{fitUB}{
	vector of upper bounds for critical intervals for each item.}
  \item{itemLabels}{
	vector of item labels.
}
  \item{mainTitle}{
	string containing the title of the plot.
}
  \item{pch}{
	number or vector indicating the type of symbols to be used for each item.
}
  \item{fitColours}{
	Color that will be used to shade the critical inteval area.
}
  \item{xlab}{
	Label of the x-axis. The default is `items`.
}
  \item{cex}{
	Size of the x-axis label.
}

  \item{\dots}{
	Additional parameters.
}

Argument to use when passing a CQmodel object:

  \item{table}{
	Name of the RMP table that for which the fit will be plotted. By default \code{fitgraph} will plot the first RMP table of the CQmodel object, this argument overrides this default.
}

  \item{fit.type}{
	Type of fit estimate that will be used, it can be \code{W} for Weighted Fit (i.e. Infit, the default), or \code{U} for Unweighted Fit (i.e. Outfit). Called \code{type} in previous versions; use of that parameter is deprecated to avoid collision with the \code{type} parameter in the \code{link{plot}} function.
}

}

\references{
	Wilson, M. (2005). Constructing measures: An item response modeling approach. 
	}
\author{
	David Torres Irribarra and Rebecca Freund.
}

\examples{

# Generating mock data
sampleLabels <- paste('item ',1:10)

fitBounds <- (abs(rnorm(10, mean = 0, sd = .05)) * 2)
fitEst <- rnorm(10, mean = 1, sd = .1)
fitBounds <- (abs(rnorm(10, mean = 0, sd = .05)) * 2)
fitLB <- 1 - fitBounds
fitUB <- 1 + fitBounds

# running fitgraph
fitgraph(fitEst,fitLB,fitUB,itemLabels=sampleLabels)

#From ConQuest output:
	
	fpath <- system.file("extdata", package="WrightMap")

	fitgraph(file.path(fpath,"ex2.shw"))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
