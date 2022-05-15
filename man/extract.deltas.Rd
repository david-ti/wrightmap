\name{extract.deltas}
\alias{extract.deltas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Master's Delta parameters from a TAM model.
}
\description{
This function takes as its input a TAM object. It adds reads the TAM item parameters and organizes them into a matrix that can be used as input in the \code{\link{CCCfit}} function.
}
\usage{
extract.deltas(tamObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamObject}{
  TAM object containing the results of a a Rasch model or Partial Credit model.
}
}
\details{
This function organizes the item parameter results into a matrix where each row is contains the parameters associated with an item and each columns is contains the parameters associated with a specific step (score 0 vs score 1, score 1 vs score 2, etc.). The resulting matrix will have as many rows as items and as many columns as the maximum number of steps among the items.
}
\value{
A matrix in which each row is an item and each column is a step
}
\references{
Masters, G. N. (1982). A Rasch model for partial credit scoring. \emph{Psychometrika}, 47(2), 149-174. 
}
\author{
David Torres Irribarra
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{CCCfit}}
 \code{\link{make.thresholds}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tamObject) 
{
    delta.long <- tamObject$xsi
    n.deltas <- apply(tamObject$B, 1, max)
    delta.mat <- matrix(NA, nrow = length(n.deltas), ncol = max(n.deltas))
    matCoords.row <- rep(1:length(n.deltas), n.deltas)
    matCoords.col <- c()
    for (i in 1:length(n.deltas)) {
        for (j in 1:n.deltas[i]) {
            matCoords.col <- c(matCoords.col, j)
        }
    }
    delta.long$matCoords.row <- matCoords.row
    delta.long$matCoords.col <- matCoords.col
    for (k in 1:nrow(delta.long)) {
        delta.mat[delta.long$matCoords.row[k], delta.long$matCoords.col[k]] <- delta.long$xsi[k]
    }
    delta.mat
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
