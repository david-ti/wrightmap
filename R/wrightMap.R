wrightMap <-
function(thetas, thresholds = NULL, item.side = itemModern, person.side = personHist,  main.title = "Wright Map", min.logit.pad = 0.25, max.logit.pad = 0.25, min.l = NULL, max.l = NULL, item.prop = 0.8, return.thresholds = TRUE, new.quartz = FALSE, use.hist = NULL, item.groups = NULL, item.group.cex = 0.6, axis.items.cex = 0.7, ...) {

	## Helper Functions


	if(is.null(thresholds))
		thresholds <- thetas

	thetas <- personData(thetas)

	thresholds <- itemData(thresholds,...)

	# Setting plot parameters
	thresholds <- as.matrix(thresholds)

	nD <- ncol(thetas)

	min.theta <- quantile(thetas, probs = c(0.01), na.rm = TRUE)
	max.theta <- quantile(thetas, probs = c(0.99), na.rm = TRUE)

	if (is.null(min.l)) {
		min.l <- min(c(min.theta, thresholds), na.rm = TRUE) - min.logit.pad
	}

	if (is.null(max.l)) {
		max.l <- max(c(max.theta, thresholds), na.rm = TRUE) + max.logit.pad
	}

	yRange <- c(min.l, max.l)

	# Generating Full Map

	if (new.quartz)
		dev.new(width = 9, height = 5)

	op <- par("oma", "mar", "mgp")

	#par(oma = op$oma + c(0, 5, 0, 5))
	par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
	#par(mgp = c(op$mar[1] - 2.4, 1, 0))

	par(mar = c(op$mar[1], 0.2, op$mar[3], 0.1))
	left.marg <- 0.05
	right.marg <- .1
	plots <- 1 - (left.marg + right.marg)
	divider <- plots * (1 - item.prop) + left.marg
	person.screen <- c(left.marg,divider,0,1)
	item.screen <- c(divider,1 - right.marg,0,1)
	screens <- matrix(c(item.screen,person.screen),ncol = 4,byrow = TRUE)
	old.screens <- split.screen()
	split.screen(screens)
	item.screen <- screen()
	dots <- list(...)
	dots[c("yRange","oma")] <- NULL

	# Handle item groups for multiple panels
	if (!is.null(item.groups)) {
		# Convert to factor to get unique groups and preserve order
		if (!is.factor(item.groups)) {
			item.groups <- factor(item.groups, levels = unique(item.groups))
		}
		group.levels <- levels(item.groups)
		nG <- length(group.levels)
		nI <- nrow(thresholds)

		# Define parameters that need to be subset by item rows
		# Matrix parameters: rows = items, columns = steps/levels
		matrix.params <- c("thr.lab.text", "thr.sym.pch", "thr.sym.cex", "thr.sym.lwd",
		                   "thr.sym.col.fg", "thr.sym.col.bg", "thr.lab.col",
		                   "thr.lab.font", "thr.lab.pos")
		# Vector parameters: one element per item
		vector.params <- c("label.items")

		# Split item screen into multiple sub-panels (one per group)
		item.old.screens <- split.screen()
		split.screen(c(1, nG))
		first.item.screen <- screen()

		# Determine which panel is the middle one for the "Items" label
		middle.panel <- ceiling(nG / 2)

		for (i in seq_len(nG)) {
			group.name <- group.levels[i]
			group.idx <- which(item.groups == group.name)
			group.thr <- thresholds[group.idx, , drop = FALSE]

			# Preserve row names for item labels
			if (!is.null(rownames(thresholds))) {
				rownames(group.thr) <- rownames(thresholds)[group.idx]
			}

			# Use empty axis label - we'll add the group name separately with smaller font
			group.axis.items <- ""

			# Determine which logit axis to show
			# Only show on rightmost panel
			if (i == nG) {
				group.show.axis.logits <- if (!is.null(dots$show.axis.logits)) dots$show.axis.logits else "R"
			} else {
				group.show.axis.logits <- FALSE
			}

			# Build parameters for this group
			group.dots <- dots
			group.dots$axis.items <- group.axis.items
			group.dots$show.axis.logits <- group.show.axis.logits

			# Subset matrix parameters (rows = items) for this group
			for (param in matrix.params) {
				if (!is.null(group.dots[[param]])) {
					val <- group.dots[[param]]
					if (is.matrix(val) || is.data.frame(val)) {
						if (nrow(val) == nI) {
							group.dots[[param]] <- val[group.idx, , drop = FALSE]
						}
					}
				}
			}

			# Subset vector parameters (one per item) for this group
			for (param in vector.params) {
				if (!is.null(group.dots[[param]])) {
					val <- group.dots[[param]]
					if (is.vector(val) && length(val) == nI) {
						group.dots[[param]] <- val[group.idx]
					}
				}
			}

			# Handle per-panel cutpoints if cutpoints is a list
			if (!is.null(group.dots$cutpoints) && is.list(group.dots$cutpoints)) {
				cutpoints.list <- group.dots$cutpoints
				if (group.name %in% names(cutpoints.list)) {
					group.dots$cutpoints <- cutpoints.list[[group.name]]
				} else {
					# No cutpoints specified for this group
					group.dots$cutpoints <- NULL
				}
			}

			item.params <- list(thr = group.thr, yRange = yRange, oma = c(0, 0, 0, 0))
			do.call(item.side, c(item.params, group.dots))

			# Add group name below the panel
			mtext(group.name, side = 1, line = 2, font = 3, cex = item.group.cex)

			# Add shared "Items" label below the middle panel
			if (i == middle.panel) {
				mtext("Items", side = 1, line = 3.5, font = 3, cex = axis.items.cex)
			}

			# Move to next screen if not the last group
			if (i < nG && screen() != max(split.screen())) {
				screen(screen() + 1)
			}
		}

		# Clean up item sub-screens
		item.curr.screens <- split.screen()
		item.new.screens <- item.curr.screens[!(item.curr.screens %in% item.old.screens)]
		close.screen(item.new.screens)
	} else {
		# Original single-panel behavior
		item.params <- list(thr = thresholds, yRange = yRange, oma = c(0,0,0,0))
		do.call(item.side, c(item.params, dots))
	}

	# Only add title and outer margin if main.title is provided
	if (!is.null(main.title) && nchar(main.title) > 0) {
		par(oma = c(0, 0, 3, 0))
		mtext(main.title, side = 3, line = 1, outer = TRUE, font = 2)
	}
	close.screen(item.screen)
	if(!is.null(use.hist)) {
		message("Parameter 'use.hist' is deprecated. Please use 'person.side' parameter instead.")
		person.side <- ifelse(use.hist,personHist,personDens)
	}
	
	dots[c("axis.logits","show.axis.logits","close.on.close")] <- NULL
	person.params <- list(thetas = thetas,yRange = yRange,close.on.close = FALSE,oma = c(0,0,0,0),axis.logits = "",show.axis.logits = FALSE)
	do.call(person.side,c(person.params,dots))
	par(op)
	
	curr.screens <- split.screen()
	new.screens <- curr.screens[!(curr.screens %in% old.screens) ]
	close.screen(new.screens)
	
	if (return.thresholds) {
		return(thresholds)
	}



}
