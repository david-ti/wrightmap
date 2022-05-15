	ICCfit <- function( itemNumber, observedResponses, personEstimates, itemParameters
		, xlim = c(-4,4), method = "Quantile", NQtiles = 10){

		propCI <- function(propVector, nVector, alpha = .05){

			propSE <- sqrt(propVector*(1-propVector)/nVector)

			propLB <- propVector - (propSE * qnorm(1-(alpha/2)))
			propUB <- propVector + (propSE * qnorm(1-(alpha/2)))

			data.frame(propSE = propSE, propLB = propLB, propUB = propUB)

		}

		plotICC <- function(difficulty, range = xlim){

			invlogit <- function(x) 
			{
				1/(1 + exp(-x))
			}

			params <- data.frame(ability = seq(-4,4, length.out = 1000), difficulty = difficulty)

			probs <- invlogit(params[,1]-params[,2])

			lines(params[,1],probs)

		}


		plotICCerrors <- function(difficulty, dSE, range = xlim){

			invlogit <- function (x) 
			{
				1/(1 + exp(-x))
			}

			params <- data.frame(ability = seq(-4,4, length.out = 1000)
				, lb  = difficulty - 1.96 * dSE
				, ub  = difficulty + 1.96 * dSE)

			probslb <- invlogit(params[,1]-params[,2])
			probsub <- invlogit(params[,1]-params[,3])

			xCoords <- c(ability = seq(-4,4, length.out = 1000),ability = seq(4,-4, length.out = 1000))
			yCoords <- c(probslb,rev(probsub) )

			polygon(xCoords,yCoords, col = "grey75", border = NA)

		}

		if(method == "ByPersonEstimate"){

			aggdata <- aggregate(observedResponses,
				by=list(round(personEstimates,2)), FUN = mean, na.rm = TRUE)

			aggdata[,-1][aggdata[,-1] == 1] <- .999
			aggdata[,-1][aggdata[,-1] == 0] <- .001

			sampleSizeAggdata <- aggregate(is.na(observedResponses), 
				by=list(round(personEstimates,2)), FUN = length)

		}

		if(method == "Quantile"){

			cutPoints <- quantile(personEstimates,seq(0,1,length = NQtiles + 1))

			aggdata <-aggregate(observedResponses, by=list(cut(personEstimates,cutPoints)), 
			  FUN=mean, na.rm=TRUE)

			breakMeans <-aggregate(personEstimates, by=list(cut(personEstimates,cutPoints)), 
			  FUN=mean, na.rm=TRUE)

			aggdata[,1] <- breakMeans[,2]

			aggdata[,-1][aggdata[,-1] == 1] <- .999
			aggdata[,-1][aggdata[,-1] == 0] <- .001

			sampleSizeAggdata <- aggregate(is.na(observedResponses), 
				by=list(cut(personEstimates,cutPoints)),  FUN = length)

			sampleSizeAggdata[,1] <- breakMeans[,2]

		}


		if(method == "Histogram"){

			histData <- hist(personEstimates, breaks = "FD", plot = FALSE)

			cutPoints <- histData$breaks

			aggdata <-aggregate(observedResponses, by=list(cut(personEstimates,cutPoints)), 
			  FUN=mean, na.rm=TRUE, drop = FALSE)

			breakMeans <-aggregate(personEstimates, by=list(cut(personEstimates,cutPoints)), 
			  FUN=mean, na.rm=TRUE, drop = FALSE)

			aggdata[,1] <- histData$mids

			aggdata[,-1][aggdata[,-1] == 1] <- .999
			aggdata[,-1][aggdata[,-1] == 0] <- .001

			sampleSizeAggdata <- aggregate(is.na(observedResponses), 
				by=list(cut(personEstimates,cutPoints)),  FUN = length, drop = FALSE)

			sampleSizeAggdata[,1] <- histData$mids

		}

		plot(aggdata[,1],aggdata[,itemNumber + 1]
			, type = "n", axes = FALSE,
			, ylab = "Proportion"
			, xlab = "Proficiency"
			, ylim = c(0,1)
			, xlim = xlim)

		plotICCerrors(itemParameters[itemNumber,1], itemParameters[itemNumber,2])

		plotICC(itemParameters[itemNumber,1])

		cbind(aggdata[,1],propCI(aggdata[,itemNumber + 1],sampleSizeAggdata[,itemNumber + 1]))

		apply(cbind(aggdata[,1],propCI(aggdata[,itemNumber + 1],sampleSizeAggdata[,itemNumber + 1]))
			,1,
			function(x) segments(x0 = x[1], y0 = x[3], x1 = x[1], y1 = x[4]
					, col = "#31333450"))

		points(aggdata[,1],aggdata[,itemNumber + 1], pch = 18, cex = .75, col = "#31333450")

		axis(2, las = 1)
		axis(1)

		title(paste("Item", rownames(itemParameters)[itemNumber]) )

		print(sampleSizeAggdata)

	}