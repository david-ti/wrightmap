\name{WrightMap}
\alias{WrightMap-package}
\alias{WrightMap}
\docType{package}
\title{
Wright Map: IRT Item-Person Map
}
\description{
This package allows the easy generation of `Wright Maps' (named after Ben Wright), also known as item-person maps to display unidimensional and multidimensional assessment results. These maps represent simultaneously the proficiency distribution of respondents and the item difficulty parameters as estimated by a model of the Rasch Family. The package contains several other functions for graphing common IRT statistics.

Additionally, the package contains the \code{CQmodel} function, which reads output files created using ConQuest software and creates a set of data frames for easy data manipulation, bundled in a CQmodel object. The wrightMap function can take a CQmodel object as input or it can be used to create Wright Maps directly from data frames of person and item parameters.
}
\details{
\tabular{ll}{
Package: \tab WrightMap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-02\cr
License: \tab BSD_3_clause | LICENSE \cr
}
The wrightMap function relies on two main inputs: (a) \code{thetas}: a vector or matrix of respondent proficiences, and (b) \code{thresholds}: a vector or matrix of item thresholds. In the simplest case, say for a unidimensional Rasch model, \code{thetas} can be a vector of person proficiencies and \code{thresholds} a vector of item difficulties. 

To plot multiple dimensions of person proficiency, simply provide them as a matrix were the results for each dimension is stored in a different column, such that for a 3-dimensional model with 1,000 persons, \code{theta} is a matrix of 1000 rows and 3 columns. 

To plot polytomous items, the thresholds for each level must be passed to the functions through the \code{thresholds} matrix, where each row represents an item and each column represents a level. For instance, if the results of a Rating Scale model with 5 response categories and 10 items is being plotted, the \code{thresholds} matrix will have 10 rows and 4 columns ( column one represents the thresholds between the 1 and 2 response category, column 2 the threshold between categories 2 and 3, etc.).

Alternatively, wrightMap can read directly the \code{.shw} and \code{.eap/.mle/.wle} output files from a Conquest analysis, and will automatically generate the \code{thetas} and \code{thresholds} matrices.

}
\author{
David Torres Irribarra and Rebecca Freund

Maintainer: David Torres Irribarra <dti@berkeley.edu> and Rebecca Freund <rlfreund@berkeley.edu>
}
\references{
	Andrich, D. (1978). A rating formulation for ordered response categories. \emph{Psychometrika}, \emph{43}(4), 561--573. 
	Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika}, \emph{47}(2), 149--174. 
	Wilson, M. (2005). Constructing measures: An item response modeling approach. 
	Wright, B. D., \& Stone, M. H. (1979). \emph{Best test design.} Chicago: Mesa Press. 
}

\keyword{Wright Map}
\keyword{IRT}
\keyword{Item Response Models}
\keyword{Rasch Model}

\seealso{

}
\examples{

# Plotting results of a unidimensional Rasch Model

## Mock results
	uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)
	difficulties  <- sort( rnorm( 20))

## Default map
wrightMap( uni.proficiency, difficulties)

## Density version
wrightMap( uni.proficiency, difficulties, person.side = personDens)

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


