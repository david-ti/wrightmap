\name{wrightMap}
\alias{wrightMap}
\alias{plot.CQmodel}
\title{
wrightMap: item-person maps
}
\description{
This function allows the easy generation of `Wright Maps' (named after Ben Wright), also known as item-person maps. They are used to to display unidimensional and multidimensional assessment results. These maps represent simultaneously the proficiency distribution of respondents and the item difficulty parameters as estimated by a model of the Rasch family.
}
\usage{
wrightMap(thetas, thresholds = NULL, item.side = itemModern, person.side = personHist,  main.title = "Wright Map", min.logit.pad = 0.25, max.logit.pad = 0.25, min.l = NULL, max.l = NULL, item.prop = 0.8, return.thresholds = TRUE, new.quartz = FALSE, use.hist = NULL,...) 
\method{plot}{CQmodel}(x, \dots)
}

\arguments{

The parameters documented here do not include many of the options included in the Wright Map family of functions. For graphical parameters, see \code{\link{item.side}} and \code{\link{person.side}}. For data handling, see \code{\link{item.person.data}} and \code{\link{CQmodel}}.

wrightMap parameters:


  \item{thetas}{
    a vector, matrix or data frame of person parameter estimates. Can also be a character string specifying a ConQuest output file of person parameter estimates, or a CQmodel object. Will be sent to the function \code{\link{personData}}.
}
  \item{thresholds}{
    matrix or data frame of item parameter estimates. Can also be a character string specifying a ConQuest show file. Will be sent to the function \code{\link{itemData}}.
}
  \item{item.side}{
   function to use to draw the item side of the map. Currently included options are itemModern (default), itemClassic (for ConQuest-style Wright Maps) and itemHist. See \code{\link{item.side}} for details.
}
  \item{person.side}{
   function to use to draw the person side of the map. Currently included options are personHist (default), to draw the person distribution as a histogram, and personDens, which draws a density plot. See \code{\link{person.side}} for details.
}

  \item{main.title}{
  title of the Wright Map.
}
  \item{min.logit.pad}{
  numeric value indicating how much of the lower end of the logit scale should be included in the plot.
}
  \item{max.logit.pad}{
  numeric value indicating how much of the upper end of the logit scale should be included in the plot.
}
  \item{min.l}{
  numeric value for fixing the lower end of the logit scale. It overrides the automatic detection of the range and the \code{min.logit.pad} correction.
}
  \item{max.l}{
  numeric value for fixing the upper end of the logit scale. It overrides the automatic detection of the range and the \code{max.logit.pad} correction.
}
  \item{item.prop}{
  numeric value greater than 0 and smaller than 1 indicating the proportion of the plot to be allocated to the item part of the Wright Map.
}
  \item{return.thresholds}{
  logical. Determines whether the to return or not the numeric values used to position the parameters on the item side of the Wright Map. Enabled by default.
  }
  \item{new.quartz}{
  logical. Determines whether the wrightMap will be created on a new graphical device or if it will reuse one already open. By default is set to \code{FALSE} to avoid creating new devices.
}
  \item{use.hist}{
    deprecated. Use the \code{\link{person.side}} parameter instead
}
  \item{\dots}{
  Additional arguments to pass to \code{\link{person.data}}, \code{\link{item.data}}, \code{\link{person.side}}, or \code{\link{item.side}}
}

  
 wrightMap can also be called by passing a CQmodel object to plot:
 
 \item{x}{ CQmodel object to pass to plot}



}

\references{
  Wilson, M. (2005). Constructing measures: An item response modeling approach. 
  Wright, B. D., \& Stone, M. H. (1979). \emph{Best test design.} Chicago: Mesa Press. 
}

\author{David Torres Irribarra and Rebecca Freund}
\seealso{
\code{\link{person.side}}
\code{\link{item.side}}
\code{\link{personData}}
\code{\link{itemData}}}
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
    d1 = rnorm(1000, mean =  -0.5, sd = 1),
    d2 = rnorm(1000, mean =   0.0, sd = 1),
    d3 = rnorm(1000, mean =  +0.5, sd = 1))

  difficulties  <- sort( rnorm( 20))

wrightMap( multi.proficiency, difficulties)

# Plotting results of a unidimensional Rating Scale Model

## Mock results
  uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)

  items.loc <- sort( rnorm( 20))
  thresholds <- data.frame(
    l1 = items.loc - 0.5 ,
    l2 = items.loc - 0.25,
    l3 = items.loc + 0.25,
    l4 = items.loc + 0.5)

wrightMap( uni.proficiency, thresholds)


  
    ####ConQuest integration###
  
  	fpath <- system.file("extdata", package="WrightMap")
  	
  	#Partial credit model:

	model1 <- CQmodel(p.est = file.path(fpath,"ex2.eap"), show = file.path(fpath,"ex2.shw"))
	wrightMap(model1) 
	
	# Rating scale model:
	model2 <- CQmodel(file.path(fpath,"ex2b.eap"), file.path(fpath,"ex2b-2.shw"))
	wrightMap(model3, label.items.row = 2) 
		
  # Complex model
	model3 <- CQmodel(file.path(fpath,"ex4a.mle"), file.path(fpath,"ex4a.shw")) 
	wrightMap(model3, min.logit.pad = -29, person.side = personDens)
	
	
  ### Skip CQmodel
	wrightMap(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"), 
  label.items.row = 3) 

}


