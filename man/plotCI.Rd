\name{plotCI}
\alias{difplot}
\alias{difplot.default}
\alias{difplot.CQmodel}
\alias{difplot.character}
\alias{plotCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting confidence intervals and DIF
}
\description{The \code{plotCI} function is intended for graphing confidence intervals. The \code{difplot} function is a wrapper for \code{plotCI} specifcally intended for examining Differential Item Functioning from ConQuest output.
}
\usage{
plotCI(ests, errors, labels = "", zeroline = TRUE, incol = "gray", outcol = "blue"
, main.title = "Statistical Significance Plot", axes = FALSE, xlab = "", pch = 16, \dots)
\method{difplot}{default}(data, grouptype = NULL, group = NULL, item.names = NULL
, ylim = c(-1, 1), ylab = NULL, \dots)
\method{difplot}{CQmodel}(data, table.name = NULL, grouptype = NULL
, group = NULL, \dots)
\method{difplot}{character}(data, equation, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	
	\code{plotCI} parameters:
	
	\item{ests}{vector of point estimates.}
	\item{errors}{vector of standard errors.}
	\item{labels}{vector of labels for the items.}
	\item{zeroline}{logical indicating whether to draw a line at zero.}
	\item{incol}{color of intervals containing zero.}
	\item{outcol}{color of intervals not containing zero.}
	\item{main.title}{title of the plot.}
	\item{axes,xlab,pch}{parameters passed to \code{\link{plot}}.}

	\code{difplot} parameters:
	
  \item{data}{A \code{CQmodel} object or the name of a ConQuest show file. Can also be a table of parameters taken from ConQuest output.}
  \item{table.name}{The RMP table to use for parameters. Should be an interactions table.}
  \item{grouptype}{The name of the demographic variable (e.g. ``gender'').}
  \item{group}{The name of the group to test for DIF (e.g. ``male'').}
  \item{item.names}{vector of item names.}
  \item{equation}{string specifying the model equation, if the Summary of Estimation table was not included in the show file. }
  
  \item{ylim,ylab}{more parameters passed to \code{\link{plot}}.}
  
  \item{\dots}{additional parameters to pass to \code{plot}.
}
}
\details{
The \code{plotCI} function takes point estimates and standard errors as inputs and plots confidence intervals in relation to a zero-line. By default, it colors the intervals gray if they include zero, and blue if they do not. The \code{difplot} function is a wrapper for \code{plotCI} specifcally intended for examining Differential Item Functioning from ConQuest output and expects tables formatted exactly like ConQuest output to work correctly. For plotting DIF from other statistical packages, it is recommended to use \code{plotCI} directly.
}

\author{David Torres Irribarra and Rebecca Freund}

\examples{
	#Plotting confidence intervals
	
	ests <- rnorm(10,sd = .5)
	errors <- runif(10,min = .1,max = .5)
	plotCI(ests,errors,ylim = c(-3,3))
	
	#DIF plot:
	
	fpath <- system.file("extdata", package="WrightMap")

	# equation must be specified because there is no summary of estimation 
	# table in this example
	difplot(file.path(fpath,"ex6a.shw"), equation = "item-gender+item*gender") 

}
