\name{item.person.data}
\alias{item.person.data}

\alias{itemData}
\alias{itemData.default}
\alias{itemData.CQmodel}
\alias{itemData.character}

\alias{personData}
\alias{personData.default}
\alias{personData.CQmodel}
\alias{personData.character}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Processing CQmodel objects for plotting
}
\description{
The \code{itemData} and \code{personData} functions take CQmodel objects (or ConQuest output files) as inputs and return a vector or matrix. They were originally developed for use by \code{\link{wrightMap}}, but are separated out here to allow the outputs to be sent to other plotting functions.
}
\usage{
itemData(thresholds, \dots)
\method{itemData}{default}(thresholds, item.type = "deltas",\dots)
\method{itemData}{character}(thresholds, equation = NULL, \dots)
\method{itemData}{CQmodel}(thresholds, item.table = NULL, interactions = NULL, step.table = NULL, item.type = "default", throld = 0.5, \dots)

personData(thetas,\dots)
\method{personData}{default}(thetas,\dots)
\method{personData}{character}(thetas, p.type = NULL,\dots)
\method{personData}{CQmodel}(thetas,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	
itemData arguments:
  \item{thresholds}{
  	Usually, a CQmodel object or the name of a ConQuest show file. Will also accept a matrix, but this is only really for use within other functions. In general \code{\link{make.thresholds}} should be used instead.
  	}
  	
  	\item{item.type}{
Indicates whether to use \code{thresholds} or \code{deltas}.
}

  \item{equation}{
  	 string giving the model equation, if the Summary of Estimation table was not included in the show file.
  	 }

\item{item.table}{
  Name of RMP table to use for the main effect of the item parameters.
}
  \item{interactions}{
  Name of RMP interaction table to use in addition to \code{item.table}.
}
  \item{step.table}{
  Name of RMP table to use in addition to \code{item.table}.
}
  
  \item{throld}{
The probability level to be used for calculating thresholds.
}
  \item{\dots}{
Additional parameters to pass to \code{\link{make.thresholds}}.
}

personData arguments:

\item{thetas}{a CQModel object or the name of the Conquest person parameters file (EAPs, MLEs, etc.)}
 \item{p.type}{ Type of person parameter estimate (EAP, MLE or WLE). }
 \item{\dots}{Not used.}

}

\details{
	The itemData and personData functions are usually called by \code{\link{wrightMap}}. They can also be called directly.
	
For the itemData function, note that the \code{item.table}, \code{interactions}, and \code{step.table} parameters must be the exact name of specific RMP tables. You cannot specify an interactions table or a step table without also specifying an item table (although JUST an item table is fine). If your model equation is more complicated, you will have to either use a GIN table or specify in the function call which tables to use for what. A model of the form item + item * step + booklet, for example, will not run unless there is a GIN table or you have defined at least the item.table.
}



\value{
The itemData functions return a vector of item parameters, or a matrix in which the rows are items and the columns are steps. The personData functions return a vector of person paramenters, or a matrix in which the rows are persons and the columns are dimensions.
}

\author{
Rebecca Freund and David Torres Irribarra }

\seealso{
\code{\link{item.side}}
\code{\link{person.side}}
\code{\link{make.thresholds}}
\code{\link{make.deltas}}
\code{\link{wrightMap}}
}
\examples{
	
#As a call from wrightMap:

fpath <- system.file("extdata", package="WrightMap")

model1 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"))
# Making thresholds if there are no GIN tables (partial credit model)
	wrightMap(model1, type = "thresholds") 
	
#Complex model:

model2 <- CQmodel(file.path(fpath,"ex4a.mle"), file.path(fpath,"ex4a.shw")) 
wrightMap(model2, item.table = "rater")
	wrightMap(model2, item.table = "rater", interactions = "rater*topic", 
  step.table = "topic")


# Plotting item results
	
	fpath <- system.file("extdata", package="WrightMap")
	model3 <- CQmodel(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"))
	m3.item <- itemData(model3)
	
	itemModern(m3.item, label.items.srt= 90, oma = c(3,0,0,3)) #control of oma allows us to give more space to longer item names
	itemClassic(m3.item)
	itemHist(m3.item)
	
	m3.person <- personData(model3)
	personHist(m3.person)
	personDens(m3.person)
  }
