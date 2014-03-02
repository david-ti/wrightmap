\name{print.CQmodel}
\alias{print.CQmodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.CQmodel(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, ...) 
{
    catnull <- function(x, y, ...) {
        if (!is.null(x)) 
            cat(y, ...)
    }
    cat("\n")
    cat("ConQuest Output Summary:\n")
    cat("========================\n")
    catnull(x$title, x$title, "\n\n")
    catnull(x$equation, "The item model:", x$equation, "\n")
    catnull(x$nDim, x$nDim, ifelse(x$nDim == 1, "dimension", 
        "dimensions"), "\n")
    catnull(x$participants, x$participants, "participants\n")
    catnull(x$deviance, "Deviance: ", x$deviance, " (", x$parameters, 
        " parameters)\n\n", sep = "")
    cat("Additional information available:\n")
    catnull(x$SOE, "Summary of estimation: $SOE\n")
    catnull(x$RMP, "Response model parameter estimates: $RMP\n")
    catnull(x$reg.coef, "Regression coefficients: $reg.coef\n")
    catnull(x$variances, "Variances: $variances\n")
    catnull(x$cor.matrix, "Correlation matrix: $cor.matrix\n")
    catnull(x$cov.matrix, "Covariance matrix: $cor.matrix\n")
    catnull(x$rel.coef, "Reliabilities: $rel.coef\n")
    catnull(x$GIN, "GIN tables: $GIN\n")
    catnull(x$p.est, paste(x$p.est.type, " table: $p.est\n", 
        sep = ""))
    catnull(x$run.details, "Additional details: $run.details\n")
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
