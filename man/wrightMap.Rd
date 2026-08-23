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
wrightMap(thetas, thresholds = NULL, item.side = itemModern, person.side = personHist
,  main.title = "Wright Map", min.logit.pad = 0.25, max.logit.pad = 0.25, min.l = NULL
, max.l = NULL, item.prop = 0.8, return.thresholds = TRUE, new.quartz = FALSE
, use.hist = NULL, item.groups = NULL, item.group.cex = 0.6, axis.items.cex = 0.7, ...)
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
  title of the Wright Map. Set to \code{NULL} or \code{""} to remove the title entirely and reclaim the space at the top of the plot.
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
  \item{item.groups}{
    optional vector or factor specifying group membership for each item (e.g., content strands like "Algebra", "Probability", "Calculus"). When provided, the item side of the Wright Map is split into multiple panels, one for each unique group. Each panel displays the group name as its x-axis label, with a shared "Items" label below all panels. The length of this vector must match the number of rows in \code{thresholds}. If a factor is provided, the order of levels determines the order of panels; otherwise, panels appear in the order groups first appear in the vector. Items do not need to be contiguous in the dataset; they will be correctly assigned to their respective panels based on group membership while preserving their original names. When using \code{item.groups}, the \code{cutpoints} parameter can accept either a numeric vector (applied to all panels) or a named list to specify different cutpoints for each panel (e.g., \code{cutpoints = list(Algebra = c(-1, 0), Probability = c(0, 1))}). Groups not included in the list will have no cutpoints.
}
  \item{item.group.cex}{
    numeric value specifying the font size magnification for the item group names when using \code{item.groups}. Default is 0.6.
}
  \item{axis.items.cex}{
    numeric value specifying the font size magnification for the shared "Items" label when using \code{item.groups}. Default is 0.7.
}
  \item{\dots}{
  Additional arguments to pass to \code{\link{personData}}, \code{\link{itemData}}, \code{\link{person.side}}, or \code{\link{item.side}}
}

  
 wrightMap can also be called by passing a CQmodel object to plot:
 
 \item{x}{ CQmodel object to pass to plot}



}

\references{
  Wilson, M. (2005). Constructing measures: An item response modeling approach. 
  Wright, B. D., & Stone, M. H. (1979). \emph{Best test design.} Chicago: Mesa Press. 
}

\author{David Torres Irribarra and Rebecca Freund}
\seealso{
\code{\link{person.side}}
\code{\link{item.side}}
\code{\link{personData}}
\code{\link{itemData}}
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

dev.new(width=10, height=10)
wrightMap( multi.proficiency, difficulties)

# Plotting results of a unidimensional Rating Scale Model
# Thresholds without column names will use default labels: 1, 2, 3, etc.

## Mock results
  uni.proficiency <- rnorm(1000, mean =  -0.5, sd = 1)

  items.loc <- sort( rnorm( 20))
  thresholds <- cbind(
    items.loc - 0.5,
    items.loc - 0.25,
    items.loc + 0.25,
    items.loc + 0.5)
  rownames(thresholds) <- paste0("Item", 1:20)

wrightMap( uni.proficiency, thresholds)

## With transition labels (0/1, 1/2, 2/3, etc.)
wrightMap( uni.proficiency, thresholds, thr.lab.type = "transition")

# Plotting with item groups (multiple item panels by content strand)

## Mock results with content strands
  uni.proficiency <- rnorm(1000, mean = -0.5, sd = 1)

  # Create items with different strands (matrix without column names for default 1, 2, 3 labels)
  n.items <- 15
  items.loc <- sort(rnorm(n.items))
  thresholds <- cbind(
    items.loc - 0.5,
    items.loc,
    items.loc + 0.5)
  rownames(thresholds) <- paste0("Item", 1:n.items)

  # Assign items to content strands
  strands <- c(rep("Algebra", 5), rep("Probability", 5), rep("Calculus", 5))

## Wright Map with multiple item panels
wrightMap(uni.proficiency, thresholds, item.groups = strands)

## Wright Map with no title (reclaims space at top)
wrightMap(uni.proficiency, thresholds, item.groups = strands, main.title = NULL)

## Wright Map with adjusted font sizes for group labels
wrightMap(uni.proficiency, thresholds, item.groups = strands,
          item.group.cex = 0.8, axis.items.cex = 0.9)

## Wright Map with different cutpoints per panel
wrightMap(uni.proficiency, thresholds, item.groups = strands,
          cutpoints = list(
            Algebra = c(-1, 0),
            Probability = c(0, 1),
            Calculus = c(-0.5, 0.5)
          ))


    ####ConQuest integration###
  
  	fpath <- system.file("extdata", package="WrightMap")
  	
  	#Partial credit model:

	model1 <- CQmodel(p.est = file.path(fpath,"ex2.eap"), show = file.path(fpath,"ex2.shw"))
	wrightMap(model1) 
	
	# Rating scale model:
	model2 <- CQmodel(file.path(fpath,"ex2b.eap"), file.path(fpath,"ex2b-2.shw"))
	wrightMap(model2, label.items.row = 2) 
		
  # Complex model
	model3 <- CQmodel(file.path(fpath,"ex4a.mle"), file.path(fpath,"ex4a.shw")) 
	wrightMap(model3, min.logit.pad = -29, person.side = personDens)
	
	
  ### Skip CQmodel
	wrightMap(file.path(fpath,"ex2a.eap"), file.path(fpath,"ex2a.shw"), 
  label.items.row = 3) 

}


