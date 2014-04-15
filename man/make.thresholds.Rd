\name{make.thresholds}
\alias{make.thresholds}
\alias{make.thresholds.default}
\alias{make.thresholds.CQmodel}
\alias{make.thresholds.character}
\title{
Calculate Thurstonian thresholds.
}
\description{
This function accepts a matrix of delta parameters and converts them to thresholds (using a threshold of .5). It can also take as input a CQmodel object or a filename of a ConQuest show file.}
\usage{
make.thresholds(item.params, ...)
\method{make.thresholds}{character}(item.params,design.matrix="normal",...)
\method{make.thresholds}{CQmodel}(item.params,item.table = NULL, 
interactions = NULL,step.table = NULL,design.matrix = "normal",...)
\method{make.thresholds}{default}(item.params,design.matrix = "normal", 
theta.interval=c(-10,10),...)
}

\arguments{
  \item{item.params}{The item parameters. Can either be a matrix, a CQmodel object, or a path to a ConQuest show file}
  \item{design.matrix}{Can be "normal" or "ConQuest". Note that for a CQmodel object or ConQuest file, should be normal, NOT ConQuest.}
\item{item.table}{If item.params is a CQmodel object or a path to a ConQuest show file, item.table is the name of the items table. Commonly "item" but can be any string representing the name of a table in the ConQuest show file. This identifies what variable will form the rows of the thresholds matrix. If not specified, will be the first variable mentioned in the model equation.}
\item{interactions}{If item.params is a CQmodel object or a path to a ConQuest show file, item.table is the name of the table with the interactions (if present). Commonly "item*step" but can be any string containing "*" that is the name of a table in the ConQuest show file. Should be the product of the item.table variable and the step.table variable (if present). If not specified, will be the product term of the model equation.}
\item{step.table}{If item.params is a CQmodel object or a path to a ConQuest show file, step.table is the name of the steps table (if present). Commonly "step" but can be any string representing the name of a table in the ConQuest show file. This identifies what variable will form the columns of the thresholds matrix. If not specified, will be the second variable mentioned in the model equation.}
\item{theta.interval}{If item.params is a matrix, theta.interval specifies over what interval to search for the parameters.}

  \item{\dots}{
Additional parameters.
}
}

\value{
A matrix of threshold parameters.
}
\author{
Daniel Coulter Furr, Rebecca Freund, & David Torres Irribarra
}

\seealso{
 \code{\link{make.deltas}}
 \code{\link{CQmodel}}
 \code{\link{wrightMap}}
}
\examples{
fpath <- system.file("extdata", package="WrightMap")

# Partial credit model
model1 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw")) 
deltas <- make.deltas(model1)
make.thresholds(deltas)
make.thresholds(model1)
}

