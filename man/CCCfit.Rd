\name{CCCfit}
\alias{CCCfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Empirical category characteristic curve plot for the Partial Credit Model
}
\description{
The \code{CCCfit} function is intended for contrasting a Rasch model's expected category characteristic curve against the empirical data from observed responses. The \code{CCCfit} function displays the expected probability asociated with all response categories and plots the observed response proportions for all non-zero response categories.
}
\usage{
CCCfit(itemNumber, observedResponses, personEstimates, 
    itemParameters, xlim = c(-4, 4), method = "Quantile", NQtiles = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemNumber}{
The position of the item in the test. This position is used to select the column of observed responses and the item difficulty among the item parameters.
}
  \item{observedResponses}{
Data frame or matrix with observed responses. The data frame or matrix should be of size  N * I, where N is the number of respondents and I is the number of items in the model.
}
  \item{personEstimates}{
A vector of length N containing the model based person estimates or predictions.
}
  \item{itemParameters}{
A data frame or matrix with I rows (one for each item) and M columns, where M is equal to the maximum number of item scores minus 1. This matrix contains the model based estimates for the step parameters (deltas), where column 1 contains the parameter associated with the step between category 0 versus category 1, column 2 the step parameters of category 1 versus category 2, and so on.
}
  \item{xlim}{
Vector with two values indicating the minimum and maximum values to be used when plotting the item characteristic curve.
}
  \item{method}{
Selects the \code{Quantile} method to group the respondents (see `Details').
}
  \item{NQtiles}{
This value controls how many grouping will be used: 4 groups cases groups respondents by quartiles, 5 by quintiles, 10 by deciles, etc.
}
}
\details{
The function uses the step difficulty parameters to generate the model based curve. The observed responses are then grouped using the \code{Quantile} method in order to contrast the model predicted response probability with the observed proportion (this is the only method implemented so far). By default the function uses deciles to generate the respondent groups.
}
% \value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
% }
% \references{
%% ~put references to the literature/web site here ~
% }
\author{
David Torres Irribarra
}
% \note{
%%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ICCfit}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (itemNumber, observedResponses, personEstimates, itemParameters, 
    xlim = c(-4, 4), method = "Quantile", NQtiles = 10) 
{
    curve.cols <- paste(RColorBrewer::brewer.pal(n = 8, name = "Dark2"), 
        "40", sep = "")
    points.cols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
    deltas <- itemParameters[itemNumber, ]
    deltas <- deltas[!is.na(deltas)]
    maxCat <- length(deltas)
    probCCC <- function(theta, deltas) {
        original.length <- length(deltas) + 1
        deltas <- deltas[!is.na(deltas)]
        deltas <- c(0, deltas)
        lN <- length(deltas)
        M <- matrix(rep(NA, lN), ncol = lN)
        CM <- matrix(rep(NA, lN), ncol = lN)
        M[, 1] <- 0
        CM[, 1] <- 1
        for (k in 2:lN) {
            M[, k] <- M[, (k - 1)] + theta - deltas[k]
            CM[, k] <- CM[, (k - 1)] + exp(M[, k])
        }
        output <- exp(M)/CM[, k]
        length(output) <- original.length
        output
    }
    categoryProbs <- sapply(seq(xlim[1], xlim[2], length = 100), 
        probCCC, deltas = deltas)
    plot(seq(xlim[1], xlim[2], length = 100), categoryProbs[1, 
        ], type = "n", axes = FALSE, xlab = "Proficiency", ylab = "Proportion", 
        ylim = c(0, 1))
    axis(2, las = 1)
    axis(1)
    lines(seq(xlim[1], xlim[2], length = 100), categoryProbs[1, 
        ], type = "l", lwd = 3, lty = 1, col = "grey80")
    nCats <- length(deltas) + 1
    for (i in 2:nCats) {
        lines(seq(xlim[1], xlim[2], length = 100), categoryProbs[i, 
            ], lwd = 3, col = curve.cols[i - 1])
    }
    if (method == "Quantile") {
        agg.data <- list()
        size.data <- list()
        for (i in 1:maxCat) {
            recodedResponses <- observedResponses == i
            cutPoints <- quantile(personEstimates, seq(0, 1, 
                length = NQtiles + 1))
            agg.data[[i]] <- aggregate(recodedResponses, by = list(cut(personEstimates, 
                cutPoints)), FUN = mean, na.rm = TRUE)
            breakMeans <- aggregate(personEstimates, by = list(cut(personEstimates, 
                cutPoints)), FUN = mean, na.rm = TRUE)
            agg.data[[i]][, 1] <- breakMeans[, 2]
            agg.data[[i]][, -1][agg.data[[i]][, -1] == 1] <- 0.999
            agg.data[[i]][, -1][agg.data[[i]][, -1] == 0] <- 0.001
            size.data[[i]] <- aggregate(is.na(recodedResponses), 
                by = list(cut(personEstimates, cutPoints)), FUN = length)
            size.data[[i]][, 1] <- breakMeans[, 2]
            points(agg.data[[i]][, 1], agg.data[[i]][, itemNumber + 
                1], type = "b", pch = i, cex = 0.75, col = points.cols[i], 
                lwd = 2)
        }
    }
    legend("right", horiz = FALSE, legend = paste("Cat", seq(1:maxCat)), 
        col = points.cols[1:maxCat], pch = 1:maxCat, cex = 0.8, 
        bty = "n")
    title(paste("Item", itemNumber))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
