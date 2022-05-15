\name{extract.deltas}
\alias{extract.deltas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.deltas(tamObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tamObject}{
%%     ~~Describe \code{tamObject} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
