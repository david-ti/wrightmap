\name{ICCfit}
\alias{ICCfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Empirical item characteristic curve plot for the Rasch Model~
}
\description{
The \code{ICCfit} function is intended for contrasting a Rasch model's expected item characteristic curve against the empirical data from dichotomous responses. The \code{ICCfit} function displays a confidence interval for the model based curve and plots the confidence interval for the empirical proportions.
}
\usage{
ICCfit(itemNumber, observedResponses, personEstimates, 
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
A data frame or matrix of dimensions I * 2 containing the model based item difficulty estimates in the firs column and the parameter standard error in the second column.
}
  \item{xlim}{
Vector with two values indicating the minimum and maximum values to be used when plotting the item characteristic curve.
}
  \item{method}{
Selects the method used to group the respondents: \code{Quantile} (default), \code{ByPersonEstimate}, and \code{Histogram} (see `Details').
}
  \item{NQtiles}{
When using the \code{Quantile} method this value controls how many grouping will be used: 4 groups cases groups respondents by quartiles, 5 by quintiles, 10 by deciles, etc.
}
}
\details{
The function uses the item difficulty parameter to generate the model based curve and the item difficulty parameter standard error to plot a confidence interval around the curve. The observed responses are then grouped using the selected method in order to contrast the model predicted response probability with the observed proportion. By default the function uses deciles to generate the respondent groups. The function allows the method \code{ByPersonEstimate} in order to make a different group for each observed person estimate (potentially useful when analyzing test data with large numbers with no missing data), and the \code{Histogram} method, which uses the Freedman-Diaconis algorithm to select the width of the bands used for grouping.
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
 \code{\link{CCCfit}}
}
\examples{

# Simulate 1000 persons and 5 items with 2 levels (Rasch model)
set.seed(123)
simulated_data <- sim.PCM(pN = 1000, iN = 5, lN = 2)

# Extract simulated item responses and abilities
observed_responses <- simulated_data$resp - 1
person_abilities <- simulated_data$theta
item_params <- cbind(simulated_data$delta[,2],rep(0.08),5) 
# Defining a 0.08 of error for the item parameters, 
# this usually would be estimated.

# Plot the ICC for the first item using the Quantile method
ICCfit(itemNumber = 1, 
       observedResponses = observed_responses, 
       personEstimates = person_abilities, 
       itemParameters = item_params, 
       method = "Quantile", 
       NQtiles = 10)




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
