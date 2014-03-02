\name{CQmodel}
\alias{CQmodel}
\alias{print.CQmodel}
\alias{print.SOE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CQmodel(show = NULL, p.est = NULL, p.type = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{show}{
%%     ~~Describe \code{show} here~~
}
  \item{p.est}{
%%     ~~Describe \code{p.est} here~~
}
  \item{p.type}{
%%     ~~Describe \code{p.type} here~~
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
function (show = NULL, p.est = NULL, p.type = NULL) 
{
    breakup <- function(data, starts, titles) {
        ends = c(starts[-1], length(data))
        mapply(breakdown, titles, starts, ends, list(data), SIMPLIFY = FALSE)
    }
    breakdown <- function(title, m_sec1, m_sec2, shw) {
        shw[m_sec1:m_sec2]
    }
    RMP <- function(table, parts) {
        RMP.lengths = c(11, 8, 8, 6, 6, 6, 8, 6, 5, 7)
        RMP.titles = c("est", "error", "U.fit", "U.Low", "U.High", 
            "U.T", "W.fit", "W.Low", "W.High", "W.T")
        out = grep("^ +[0-9]", table, value = TRUE)
        out = gsub("[\\(\\)\\*,]", " ", out)
        out <- split.right(out, sum(RMP.lengths))
        titles <- as.list(as.data.frame(rbind(paste("n_", parts, 
            sep = ""), parts), stringsAsFactors = FALSE))
        titles[parts == "step"] <- "step"
        titles <- unlist(titles)
        left.table <- read.table(tempify(out[1]), col.names = titles, 
            stringsAsFactors = FALSE)
        right.table <- read.fwf(tempify(out[2]), RMP.lengths, 
            col.names = RMP.titles, stringsAsFactors = FALSE)
        cbind(left.table, right.table)
    }
    split.right <- function(table, right) {
        left = nchar(table[1]) - right
        tf <- tempfile()
        write(table, tf)
        out <- read.fwf(tf, c(left, right))
        out
    }
    tempify <- function(list) {
        tf <- tempfile()
        write(as.matrix(list), tf)
        tf
    }
    by.item <- function(item, ids, values, how.long) {
        vals <- values[ids == item]
        if (length(vals) < how.long) {
            extras <- c((length(vals) + 1):how.long)
            vals[extras] <- NA
        }
        vals
    }
    get.names <- function(type, table) {
        return(unique(table[type]))
    }
    split.by <- function(table, split) {
        out <- read.table(tempify(table), sep = split, stringsAsFactors = FALSE, 
            strip.white = TRUE)
        row.names(out) <- t(rename(t(out[1])))
        out <- out[out$V2 != "", ]
        out <- as.list(as.data.frame(t(out[2]), stringsAsFactors = FALSE))
        out
    }
    trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    safe.remove <- function(table, line.at) {
        if (length(line.at) > 0) {
            table <- table[-line.at]
        }
        table
    }
    make.label <- function(lab, dim) {
        return(paste(lab, " (", dim, ")", sep = ""))
    }
    make.labels <- function(labs, dims) {
        return(unlist(as.list(t(sapply(labs, make.label, dims)))))
    }
    reliabilities <- function(table) {
        table <- table[-grep("^Dimension:", table)]
        colons.at <- grep(":", table)
        colons <- table[colons.at]
        out <- split.by(colons, ":")
        out
    }
    numify <- function(table) {
        out <- table
        nums <- suppressWarnings(!is.na(as.numeric(table)))
        out[nums] <- as.numeric(table[nums])
        return(out)
    }
    rename <- function(titles) {
        titles[titles == "SUMMARY OF THE ESTIMATION"] <- "SOE"
        titles[titles == "TABLES OF RESPONSE MODEL PARAMETER ESTIMATES"] <- "RMP"
        titles[titles == "TABLES OF POPULATION MODEL PARAMETER ESTIMATES"] <- "PMP"
        titles[grepl("MAP OF .+ AND RESPONSE MODEL PARAMETER ESTIMATES", 
            titles)] <- "MRM"
        titles[grepl("MAP OF .+ AND THRESHOLDS", titles)] <- "MTH"
        titles[titles == "TABLES OF GIN Thresholds"] <- "GIN"
        titles[titles == "Estimation method was"] <- "method"
        titles[titles == "Assumed population distribution was"] <- "distribution"
        titles[titles == "Constraint was"] <- "constraint"
        titles[titles == "The format"] <- "format"
        titles[titles == "The item model"] <- "equation"
        titles[titles == "Sample size"] <- "participants"
        titles[titles == "Final Deviance"] <- "deviance"
        titles[titles == "Total number of estimated parameters"] <- "parameters"
        titles[titles == "The number of iterations"] <- "iterations"
        titles[titles == "Termination criteria"] <- "criteria"
        titles[titles == "Random number generation seed"] <- "seed"
        titles[titles == "Number of nodes used when drawing PVs"] <- "PV.nodes"
        titles[titles == "Number of nodes used when computing fit"] <- "fit.nodes"
        titles[titles == "Number of plausible values to draw"] <- "n.plausible.values"
        titles[titles == "Maximum number of iterations without a deviance improvement"] <- "max.iterations.no.improvement"
        titles[titles == "Maximum number of Newton steps in M-step"] <- "max.steps"
        titles[titles == "Value for obtaining finite MLEs for zero/perfects"] <- "zero.perfect.value"
        titles[titles == "The regression model"] <- "regression"
        titles[titles == "key 1 scored as 1"] <- "key"
        titles[titles == "The Data File"] <- "data.file"
        titles[titles == "Max iterations"] <- "max.iterations"
        titles[titles == "Parameter Change"] <- "parameter.change"
        titles[titles == "Deviance Change"] <- "deviance.change"
        titles[titles == "REGRESSION COEFFICIENTS"] <- "reg.coef"
        titles[titles == "COVARIANCE/CORRELATION MATRIX"] <- "cov.cor"
        titles[titles == "RELIABILITY COEFFICIENTS"] <- "rel.coef"
        titles
    }
    model <- list()
    if (!(is.null(show))) {
        shw <- readLines(show)
        shw.starts = grep("^\f==", shw)
        shw.titles = rename(shw[shw.starts + 2])
        model <- breakup(shw, shw.starts, shw.titles)
        date.pattern <- "(?:(Sun|Mon|Tue|Wed|Thu|Fri|Sat)\\s+)?(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+(0[1-9]|[1-2]?[0-9]|3[01])\\s+(2[0-3]|[0-1][0-9]):([0-5][0-9])(?::(60|[0-5][0-9]))?\\s+(19[0-9]{2}|[2-9][0-9]{3})+$"
        date.at <- grep(date.pattern, model$SOE)
        title.date <- model$SOE[date.at]
        model$SOE <- safe.remove(model$SOE, date.at)
        m <- regexpr(date.pattern, title.date)
        model$run.details <- list()
        model$run.details$date <- strptime(regmatches(title.date, 
            m), format = "\%a \%b \%d \%H:\%M \%Y")
        model$title <- trim(paste(unlist(regmatches(title.date, 
            m, invert = TRUE)), collapse = ""))
        file.at <- grep("The Data File: ", model$SOE)
        file.line <- model$SOE[file.at]
        model$SOE <- safe.remove(model$SOE, file.at)
        m <- regexpr("The Data File: ", file.line)
        model$run.details$data.file <- trim(paste(unlist(regmatches(file.line, 
            m, invert = TRUE)), collapse = ""))
        colons.at <- grep(":", model$SOE)
        other.lines <- model$SOE[-colons.at]
        colons <- model$SOE[colons.at]
        two.colons.at <- grep(":.+:", colons)
        other.lines <- append(other.lines, colons[two.colons.at])
        colons <- safe.remove(colons, two.colons.at)
        SOE <- split.by(colons, ":")
        other.lines <- other.lines[-grep("===", other.lines)]
        other.lines <- other.lines[other.lines != ""]
        other.lines <- other.lines[other.lines != "SUMMARY OF THE ESTIMATION"]
        other.lines <- trim(other.lines)
        reason.at <- grep("Iterations terminated because", other.lines)
        SOE$termination.reason <- other.lines[reason.at]
        other.lines <- safe.remove(other.lines, reason.at)
        deviance.line <- grep("Deviance Change=", other.lines, 
            value = TRUE)
        other.lines <- other.lines[-grep("Deviance Change=", 
            other.lines)]
        termination.criteria <- c(unlist(strsplit(SOE$criteria, 
            ",")), deviance.line)
        SOE <- c(SOE, split.by(termination.criteria, "="))
        SOE$criteria <- NULL
        model$run.details$format <- SOE$format
        model$run.details$key <- SOE$key
        model$run.details <- c(model$run.details, other.lines)
        SOE <- numify(SOE)
        model <- c(SOE["equation"], SOE["participants"], SOE["deviance"], 
            SOE["parameters"], model)
        class(SOE) <- "SOE"
        model$SOE <- SOE
        additive.parts = unlist(strsplit(SOE$equation, "[+|-]"))
        parts = strsplit(additive.parts, "\\*")
        RMP.tables <- breakup(model$RMP, grep("TERM ", model$RMP), 
            additive.parts)
        model$RMP = mapply(RMP, RMP.tables, parts, SIMPLIFY = FALSE)
        model$run.details$names <- mapply(get.names, parts[parts == 
            additive.parts], model$RMP[parts == additive.parts])
        PMP.starts <- grep("^===+$", model$PMP)
        PMP.titles <- rename(trim(paste(model$PMP[PMP.starts + 
            1], model$PMP[PMP.starts + 2], sep = "")))
        PMP <- breakup(model$PMP, PMP.starts, PMP.titles)
        PMP$variances = as.numeric(unlist(strsplit(grep("Variance", 
            PMP$cov.cor, value = TRUE), "\\s+"))[-1])
        PMP$nDim = length(PMP$variances)
        PMP$dimensions = "Main dimension"
        if (PMP$nDim > 1) {
            PMP$cov.cor <- read.fwf(tempify(PMP$cov.cor[8:(8 + 
                PMP$nDim - 1)]), widths = c(25, rep(9, PMP$nDim)), 
                row.names = 1, strip.white = TRUE, stringsAsFactors = FALSE)
            PMP$dimensions <- row.names(PMP$cov.cor)
            names(PMP$cov.cor) <- PMP$dimensions
            PMP$cor.matrix <- PMP$cov.cor
            PMP$cor.matrix[upper.tri(PMP$cor.matrix)] = t(PMP$cor.matrix)[upper.tri(t(PMP$cor.matrix))]
            diag(PMP$cor.matrix) = 1
            PMP$cov.matrix <- PMP$cov.cor
            PMP$cov.matrix[lower.tri(PMP$cov.matrix)] = t(PMP$cov.matrix)[lower.tri(t(PMP$cov.matrix))]
            diag(PMP$cov.matrix) <- PMP$variances
        }
        PMP$cov.cor <- NULL
        PMP$reg.coef = gsub("[\\(\\),]", " ", PMP$reg.coef)
        start <- grep("CONSTANT", PMP$reg.coef)
        end <- grep("^--+$", PMP$reg.coef) - 1
        PMP$reg.coef <- read.table(tempify(PMP$reg.coef[start:end]), 
            stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1)
        if (ncol(PMP$reg.coef) == PMP$nDim) {
            names(PMP$reg.coef) <- PMP$dimensions
        }
        else if (ncol(PMP$reg.coef) == 2 * PMP$nDim) {
            names(PMP$reg.coef) <- unlist(as.list(as.data.frame(rbind(PMP$dimensions, 
                "S. errors"), stringsAsFactors = FALSE)))
        }
        PMP$reg.coef <- t(PMP$reg.coef)
        rel.starts <- grep("Dimension:", PMP$rel.coef)
        PMP$rel.coef <- breakup(PMP$rel.coef, rel.starts, PMP$dimensions)
        PMP$rel.coef <- sapply(PMP$rel.coef, reliabilities)
        PMP$rel.coef <- numify(PMP$rel.coef)
        PMP$rel.coef[PMP$rel.coef == "Unavailable"] <- NA
        PMP$rel.coef <- t(PMP$rel.coef)
        model$PMP <- NULL
        model <- c(model, PMP)
        if (!is.null(model$GIN)) {
            GIN <- model$GIN[7:(length(model$GIN) - 1)]
            GIN <- gsub("^[0-9]+\\.", "", GIN)
            GIN <- gsub("\t", " ", GIN)
            GIN <- read.table(tempify(GIN), col.names = c("thrId", 
                "thrVal", "item", "itemId", "itemLab"))
            items <- as.vector(unique(GIN$itemLab))
            model$GIN <- lapply(c(1:length(items)), by.item, 
                GIN$itemId, GIN$thrVal, max(GIN$thrId))
            names(model$GIN) <- items
            model$GIN <- t(as.data.frame(model$GIN))
        }
        class(model) <- "CQmodel"
    }
    if (!is.null(p.est)) {
        if (is.null(p.type)) {
            p.type <- toupper(unlist(strsplit(p.est, "[.]"))[-1])
        }
        p.est <- na.omit(read.table(p.est, stringsAsFactors = FALSE, 
            fill = TRUE))
        if (p.type == "EAP") 
            colperdim = 3
        else colperdim = 4
        model$nDim = floor(length(p.est)/colperdim)
        if (is.null(model$dimensions)) 
            model$dimensions <- paste("d", c(1:model$nDim), sep = "")
        if (p.type == "EAP") {
            pp_lab_t <- make.labels(c("est", "error", "pop"), 
                model$dimensions)
        }
        else {
            pp_lab_t <- c(make.labels(c("sscore", "max"), model$dimensions), 
                make.labels(c("est", "error"), model$dimensions))
        }
        if (length(p.est)\%\%colperdim == 1) {
            names(p.est) <- c("casenum", pp_lab_t)
        }
        else {
            names(p.est) <- c("casenum", "pid", pp_lab_t)
        }
        model$p.est <- p.est
        model$p.est.type <- p.type
    }
    class(model) <- "CQmodel"
    return(model)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
