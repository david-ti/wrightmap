	extract.deltas <- function(tamObject){

		delta.long <- tamObject$xsi

		n.deltas <- apply(tamObject$B,1,max)

		delta.mat <- matrix(NA, nrow = length(n.deltas), ncol = max(n.deltas))

		matCoords.row <- rep(1:length(n.deltas), n.deltas)

		matCoords.col <- c()

		for(i in 1:length(n.deltas)){

			for (j in 1:n.deltas[i]) {

				matCoords.col <- c(matCoords.col, j)

			}

		}

		delta.long$matCoords.row <- matCoords.row
		delta.long$matCoords.col <- matCoords.col

		for (k in 1:nrow(delta.long)) {

			delta.mat[delta.long$matCoords.row[k],delta.long$matCoords.col[k]] <- delta.long$xsi[k]

		}

		delta.mat

	}