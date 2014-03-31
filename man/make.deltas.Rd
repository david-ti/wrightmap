\name{make.deltas}
\alias{make.deltas.default}
\alias{make.deltas.CQmodel}
\alias{make.deltas.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Master's Delta parameters.
}
\description{
This function takes as its input a CQmodel object or the name of a ConQuest show file. It adds together the parameters as specified by the user, or if no tables are specified it reads the model equation to determine the appropriate tables to sum. This function is used by wrightMap to draw the item side of the map when a CQmodel is passed to wrightMap.
}
\usage{
make.deltas(item.params, ...)
}
\method{make.deltas}{character}(item.params, ...)
\method{make.deltas}{CQmodel}(item.params, item.table = NULL, interactions = NULL, step.table = NULL, item.sign = NULL, inter.sign = NULL, 
	step.sign = NULL, ...)
\method{make.deltas}{default}(item.params, cross.params = 0, step.params = 0, item.sign = 1, step.sign = 1, inter.sign = 1, ...)
\arguments{
  \item{item.params}{
The item parameters. Can either be a vector, a CQmodel object, or a path to a ConQuest show file}
\item{item.table}{If item.params is a CQmodel object or a path to a ConQuest show file, item.table is the name of the items table. Commonly "item" but can be any string representing the name of a table in the ConQuest show file. This identifies what variable will form the rows of the delta matrix. If not specified, will be the first variable mentioned in the model equation.}
\item{interactions}{If item.params is a CQmodel object or a path to a ConQuest show file, item.table is the name of the table with the interactions (if present). Commonly "item*step" but can be any string containing "*" that is the name of a table in the ConQuest show file. Should be the product of the item.table variable and the step.table variable (if present). If not specified, will be the product term of the model equation.}
\item{step.table}{If item.params is a CQmodel object or a path to a ConQuest show file, step.table is the name of the steps table (if present). Commonly "step" but can be any string representing the name of a table in the ConQuest show file. This identifies what variable will form the columns of the delta matrix. If not specified, will be the second variable mentioned in the model equation.}
\item{item.sign}{Can be 1 or -1. Indicates whether the item parameters should be added or subtracted.}
\item{inter.sign}{Can be 1 or -1. Indicates whether the interaction parameters should be added or subtracted.}
\item{step.sign}{Can be 1 or -1. Indicates whether the step parameters should be added or subtracted.}
\item{cross.params}{If item.params is a vector, use this parameter to pass a matrix of interaction parameters.}
\item{step.param}{If item.params is a vector, use this parameter to pass a matrix of step parameters.}
  \item{\dots}{Additional parameters
}
}
\details{
This function reshapes the tables in the ConQuest show file and adds the step parameters to the appropriate items. The vector version of this is rarely called by the user. 
}
\value{
A matrix in which each row is an item and each column is a step
}

\author{
Rebecca Freund & David Torres Irribarra
}


\seealso{
 \code{\link{make.thresholds}}
 \code{\link{CQmodel}}
 \code{\link{wrightMap}}
}
\examples{
fpath <- system.file("extdata", package="WrightMap")
model1 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.eap")) # Partial credit model
make.deltas(model1)

model2 <- CQmodel(file.path(fpath,"ex2b.eap"), file.path(fpath,"ex2b-2.shw")) # Rating scale model
make.deltas(model2)

model3 <- CQmodel(file.path(fpath,"ex4a.mle"), file.path(fpath,"ex4a.shw")) # Raters, criteria, topics
make.deltas(model3, item.table = "rater")
make.deltas(model3, item.table = "rater", interactions = "rater*topic", step.table = "topic")
}

