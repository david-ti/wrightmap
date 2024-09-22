sim.PCM <- function(pN, iN, lN, itemLevels = NULL, delta = NULL, delta.l = NULL, theta = NULL) {
    # Function to simulate polytomous item responses with items having varying levels
    #
    # Arguments:
    # pN        - Number of people (respondents)
    # iN        - Number of items
    # lN        - Default number of levels for the polytomous items (e.g., Likert scale levels)
    # itemLevels - Optional vector specifying the number of levels for each item (length = iN)
    # delta     - Optional matrix of item thresholds (iN x max(lN)). If not provided, it is generated
    # delta.l   - Optional vector of common level thresholds (max(lN)-1). If not provided, it is generated
    # theta     - Optional vector of person abilities. If not provided, it is generated from a standard normal distribution

    # Ensure valid values for iN and lN
    if (lN < 2) stop("Error: 'lN' (number of levels) must be greater than or equal to 2.")
    if (iN < 1) stop("Error: 'iN' (number of items) must be greater than or equal to 1.")

    # If itemLevels is not provided, assume all items have the same number of levels (lN)
    if (is.null(itemLevels)) {
        itemLevels <- rep(lN, iN)  # Default all items to have lN levels
    }

    # Ensure theta (person abilities) is provided or generate it
    if (is.null(theta)) {
        theta <- rnorm(pN, mean = 0, sd = 1)  # Default normal distribution for person abilities
    }

    # Check if delta (item thresholds) is provided
    if (is.null(delta)) {
        # If delta is not provided, generate common level thresholds (delta.l)
        if (is.null(delta.l)) {
            delta.l <- sort(runif((max(itemLevels) - 1), min = -3, max = 3))  # Common level thresholds
        }

        # Generate item-specific deviations for thresholds
        delta.il <- matrix(runif(iN * (max(itemLevels) - 1), min = -.5, max = .5), 
                           ncol = (max(itemLevels) - 1))
        delta.il <- t(t(delta.il) - colMeans(delta.il))  # Center the item deviations

        # Construct the full delta matrix (item thresholds)
        delta <- matrix(NA, iN, max(itemLevels))
        delta[, 2:max(itemLevels)] <- t(delta.l + t(delta.il))  # Combine common level thresholds and item-specific deviations
        delta[, 1] <- 0  # Ensure the first column is 0 for each item
    } else {
        # If delta is provided as a vector, convert it to a single-column matrix
        if (is.vector(delta)) {
            delta <- matrix(delta, ncol = 1)
        }
        
        # Check if the number of rows in delta matches iN (number of items)
        if (nrow(delta) != iN) {
            stop(paste("Error: The number of rows in 'delta' (", nrow(delta), 
                       ") must match the number of items (", iN, ").", sep = ""))
        }

        # Ensure the first column of delta contains 0s
        if (!all(delta[, 1] == 0)) {
            # Prepend a column of 0s if the first column isn't all 0s
            delta <- cbind(0, delta)
        }
    }

    # Adjust delta based on item levels (set extra thresholds to NA for items with fewer levels)
    for (i in 1:iN) {
        if (itemLevels[i] < max(itemLevels)) {
            delta[i, (itemLevels[i] + 1):max(itemLevels)] <- NA
        }
    }

    # Generate uniform random values for response generation
    U <- matrix(runif(pN * iN, 0, 1), ncol = iN)
    resp <- matrix(NA, pN, iN)

    M <- list()
    CM <- list()

    # Build M and CM matrices for each item (logits and cumulative probabilities)
    for (i in 1:iN) {
        M[[i]] <- matrix(NA, pN, max(itemLevels))
        CM[[i]] <- matrix(NA, pN, max(itemLevels))

        for (j in 1:pN) {
            M[[i]][j, 1] <- 0
            CM[[i]][j, 1] <- 1

            for (k in 2:itemLevels[i]) {
                M[[i]][j, k] <- M[[i]][j, k - 1] + theta[j] - delta[i, k]
                CM[[i]][j, k] <- CM[[i]][j, k - 1] + exp(M[[i]][j, k])
            }
        }
    }

    # Generate responses based on the cutpoints in CM
    for (i in 1:iN) {
        for (j in 1:pN) {
            cutpoint <- U[j, i] * CM[[i]][j, sum(!is.na(delta[i, ]))]
            found <- 0

            for (k in 1:sum(!is.na(delta[i, ]))) {
                if (cutpoint < CM[[i]][j, k] & found == 0) {
                    resp[j, i] <- k
                    found <- 1
                }
            }
        }
    }

    # Prepare the output as a list
    output <- list()
    output[['pN']] <- pN  # Number of people
    output[['iN']] <- iN  # Number of items
    output[['lN']] <- lN  # Default number of levels
    output[['itemLevels']] <- itemLevels  # Specific levels for each item
    output[['M']] <- M  # Logits for each person/item/level
    output[['CM']] <- CM  # Cumulative probabilities for each person/item/level
    output[['U']] <- U  # Random uniform values used for response generation
    output[['theta']] <- theta  # Person abilities

    if (!is.null(delta.l)) {
        output[['delta.l']] <- delta.l  # Common level thresholds
        output[['delta.il']] <- delta.il  # Item-specific deviations for thresholds
    }

    output[['delta']] <- delta  # Full item thresholds matrix
    output[['resp']] <- resp  # Simulated responses

    return(output)
}