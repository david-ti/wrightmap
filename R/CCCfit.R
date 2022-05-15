	CCCfit <- function(itemNumber, observedResponses, personEstimates, itemParameters
			, xlim = c(-4,4), method = "Quantile", NQtiles = 10){

			curve.cols <- paste(RColorBrewer::brewer.pal(n = 8, name = "Dark2"),"40", sep = "")
			points.cols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")

			deltas <- itemParameters[itemNumber,]

			deltas <- deltas[!is.na(deltas)]

			maxCat <- length(deltas)


			probCCC <- function(theta, deltas){

			   original.length <- length(deltas) + 1

			   # delete trailing NAs in deltas vector
			   deltas <- deltas[!is.na(deltas)]

			   deltas <- c(0,deltas)

			   lN <- length(deltas)

			   M  <- matrix( rep(NA,  lN), ncol = lN)
			   CM <- matrix( rep(NA,  lN), ncol = lN)

			   M[,1]  <- 0
			   CM[,1] <- 1

			   for ( k in 2:lN ){

			       M[  , k] <- M[  , (k-1) ] + theta - deltas[k]
			       CM[ , k] <- CM[ , (k-1) ] + exp( M[  , k] )

			   }

			   output <- exp(M)/CM[,k]
			   
			   length(output) <- original.length
			   
			   output

			  }


			categoryProbs <- sapply(seq(xlim[1],xlim[2],length=100)
				, probCCC, deltas = deltas)

			plot(seq(xlim[1],xlim[2],length=100), categoryProbs[1,]
				, type = "n", axes = FALSE
				, xlab = "Proficiency"
				, ylab = "Proportion"
				, ylim = c(0,1))

			axis(2, las = 1)
			axis(1)

			lines(seq(xlim[1],xlim[2],length=100), categoryProbs[1,]
			, type = "l", lwd = 3, lty = 1, col = "grey80")

			nCats <- length(deltas) + 1
			
			for (i in 2:nCats) {

				lines(seq(xlim[1],xlim[2],length=100),
					categoryProbs[i,], lwd = 3, col = curve.cols[i-1])

			}


			if(method == "Quantile"){

				agg.data <- list()

				size.data <- list()

				for (i in 1:maxCat) {

					recodedResponses <- observedResponses == i

					cutPoints <- quantile(personEstimates,seq(0,1,length = NQtiles + 1))

					agg.data[[i]] <-aggregate(recodedResponses, by=list(cut(personEstimates,cutPoints)), 
					  FUN=mean, na.rm=TRUE)

					breakMeans <-aggregate(personEstimates, by=list(cut(personEstimates,cutPoints)), 
					  FUN=mean, na.rm=TRUE)

					agg.data[[i]][,1] <- breakMeans[,2]

					agg.data[[i]][,-1][agg.data[[i]][,-1] == 1] <- .999
					agg.data[[i]][,-1][agg.data[[i]][,-1] == 0] <- .001

					size.data[[i]] <- aggregate(is.na(recodedResponses), 
						by=list(cut(personEstimates,cutPoints)),  FUN = length)

					size.data[[i]][,1] <- breakMeans[,2]


					points(agg.data[[i]][,1],agg.data[[i]][,itemNumber + 1], 
						type = "b", pch = i, cex = .75, col = points.cols[i], lwd = 2)



				}

			}


			legend("right", horiz = FALSE, legend = paste("Cat",seq(1:maxCat))
			, col = points.cols[1:maxCat]
			, pch = 1:maxCat
			, cex = .8
			, bty = "n" )

			title(paste("Item", itemNumber) )

	}