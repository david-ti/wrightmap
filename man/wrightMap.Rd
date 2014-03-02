\name{wrightMap}
\alias{wrightMap}
\alias{wrightMap.default}
\alias{wrightMap.CQmodel}
\alias{wrightMap.character}
\alias{plot.CQmodel}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wrightMap(x, \dots)
\method{wrightMap}{default}(thetas, thresholds, use.hist = TRUE, main.title = "Wright Map", axis.logits = "Logits", axis.persons = "Respondents", axis.items = "Items", label.items = NULL, label.items.rows = 1, label.items.srt = 0, label.items.ticks = TRUE, show.thr.lab = TRUE, show.thr.sym = TRUE, thr.lab.text = NULL, thr.lab.col = "black", thr.lab.pos = c(2, 4), thr.lab.font = 2, thr.lab.cex = 0.85, thr.sym.pch = 23, thr.sym.col.fg = rgb(0, 0, 0, 0.3), thr.sym.col.bg = rgb(0, 0, 0, 0.3), thr.sym.cex = 1.2, thr.sym.lwd = 1, dim.names = NULL, dim.color = "black", dim.lab.side = 3, dim.lab.adj = 0.5, hist.nclass = "FD", min.logit.pad = 0.25, max.logit.pad = 0.25, item.prop = 0.2)
\method{wrightMap}{CQmodel}(model, tables = NULL, label.items = NULL, main.title = NULL, thr.lab.text = NULL, dim.names = NULL, \dots)
\method{plot}{CQmodel}{model, \dots}
\method{wrightMap}{character}(show, p.est, p.type = NULL, \dots)

}

\arguments{

wrightMap parameters:	
    \item{thetas}{
%%     ~~Describe \code{thetas} here~~
}
  \item{thresholds}{
%%     ~~Describe \code{thresholds} here~~
}
  \item{use.hist}{
%%     ~~Describe \code{use.hist} here~~
}
  \item{main.title}{
%%     ~~Describe \code{main.title} here~~
}
  \item{axis.logits}{
%%     ~~Describe \code{axis.logits} here~~
}
  \item{axis.persons}{
%%     ~~Describe \code{axis.persons} here~~
}
  \item{axis.items}{
%%     ~~Describe \code{axis.items} here~~
}
  \item{label.items}{
%%     ~~Describe \code{label.items} here~~
}
  \item{label.items.rows}{
%%     ~~Describe \code{label.items.rows} here~~
}
  \item{label.items.srt}{
%%     ~~Describe \code{label.items.srt} here~~
}
  \item{label.items.ticks}{
%%     ~~Describe \code{label.items.ticks} here~~
}
  \item{show.thr.lab}{
%%     ~~Describe \code{show.thr.lab} here~~
}
  \item{show.thr.sym}{
%%     ~~Describe \code{show.thr.sym} here~~
}
  \item{thr.lab.text}{
%%     ~~Describe \code{thr.lab.text} here~~
}
  \item{thr.lab.col}{
%%     ~~Describe \code{thr.lab.col} here~~
}
  \item{thr.lab.pos}{
%%     ~~Describe \code{thr.lab.pos} here~~
}
  \item{thr.lab.font}{
%%     ~~Describe \code{thr.lab.font} here~~
}
  \item{thr.lab.cex}{
%%     ~~Describe \code{thr.lab.cex} here~~
}
  \item{thr.sym.pch}{
%%     ~~Describe \code{thr.sym.pch} here~~
}
  \item{thr.sym.col.fg}{
%%     ~~Describe \code{thr.sym.col.fg} here~~
}
  \item{thr.sym.col.bg}{
%%     ~~Describe \code{thr.sym.col.bg} here~~
}
  \item{thr.sym.cex}{
%%     ~~Describe \code{thr.sym.cex} here~~
}
  \item{thr.sym.lwd}{
%%     ~~Describe \code{thr.sym.lwd} here~~
}
  \item{dim.names}{
%%     ~~Describe \code{dim.names} here~~
}
  \item{dim.color}{
%%     ~~Describe \code{dim.color} here~~
}
  \item{dim.lab.side}{
%%     ~~Describe \code{dim.lab.side} here~~
}
  \item{dim.lab.adj}{
%%     ~~Describe \code{dim.lab.adj} here~~
}
  \item{hist.nclass}{
%%     ~~Describe \code{hist.nclass} here~~
}
  \item{min.logit.pad}{
%%     ~~Describe \code{min.logit.pad} here~~
}
  \item{max.logit.pad}{
%%     ~~Describe \code{max.logit.pad} here~~
}
  \item{item.prop}{
%%     ~~Describe \code{item.prop} here~~
}

  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

Additional parameters when creating a Wright Map from a CQmodel object:

  \item{model}{ CQmodel object containing both item and person parameters. }
  \item{tables}{ Name of RMP table(s) to use for the item parameters. If no table is specified, the function will use the GIN table if present, or look for a table called $item. }
  
Parameters for creating a WrightMap directly from ConQuest output files:

  \item{show}{ ConQuest show file. }
  \item{p.est}{ Conquest person parameters file (EAPs, MLEs, etc.). }

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
\author{David Torres Irribarra and Rebecca Freund}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
