\name{print.SOE}
\alias{print.SOE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.SOE(x, ...)
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
    scipen <- getOption("scipen")
    options(scipen = 999)
    cat("\nSummary of estimation\n\n")
    cat("Estimation method:", x$method, "\n")
    cat("Assumed population distribution:", x$distribution, "\n")
    cat("Constraint:", x$constraint, "\n\n")
    cat("Termination criteria:\n     ", x$max.iterations, "iterations\n     ", 
        x$parameter.change, "change in parameters\n     ", x$deviance.change, 
        "change in deviance\n     ", x$max.iterations.no.improvement, 
        "iterations without a deviance improvement\n     ", x$max.steps, 
        "Newton steps in M-step\n")
    m <- regexec("Iterations terminated because ", x$termination.reason)
    reason <- paste(unlist(regmatches(x$termination.reason, m, 
        invert = TRUE)), collapse = "")
    cat("Estimation terminated after ", x$iterations, " iterations because ", 
        reason, ".\n\n", sep = "")
    cat("Random number generation seed:", x$seed, "\n")
    cat(x$PV.nodes, "nodes used for drawing", x$n.plausible.values, 
        "plausible values", "\n")
    cat(x$fit.nodes, "nodes used when computing fit", "\n")
    cat("Value for obtaining finite MLEs for zero/perfects:", 
        x$zero.perfect.value, "\n")
    cat("\n")
    options(scipen = scipen)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
