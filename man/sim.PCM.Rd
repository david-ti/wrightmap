\name{sim.PCM}
\alias{sim.PCM}
\title{Simulate Item Responses for Partial Credit and Rasch Models with Varying Levels}
\description{
Simulate polytomous item responses based on item difficulty thresholds (\code{delta}) and person abilities (\code{theta}) for the Partial Credit Model (PCM) and the Rasch Model as a special case. The function allows items to have varying numbers of levels, making it useful for generating data in contexts such as item response theory (IRT) models for polytomous items (e.g., Likert scales).
}
\usage{
sim.PCM(pN, iN, lN, itemLevels = NULL, delta = NULL, delta.l = NULL, theta = NULL)
}
\arguments{
  \item{pN}{\code{integer}. The number of people (respondents) to simulate.}
  \item{iN}{\code{integer}. The number of items in the test or survey.}
  \item{lN}{\code{integer}. The default number of levels (categories) for each polytomous item. Must be greater than or equal to 2. This is used if \code{itemLevels} is not provided.}
  \item{itemLevels}{\code{integer vector}. An optional vector specifying the number of levels for each item. If provided, the length must equal \code{iN}. If not provided, all items are assumed to have \code{lN} levels.}
  \item{delta}{\code{matrix}. An optional matrix of item thresholds with dimensions \code{iN} by the maximum number of levels in \code{itemLevels}. If not provided, the function will generate thresholds internally. The first column must contain zeros or the function will automatically prepend a column of zeros. If provided as a vector, it will be converted into a single-column matrix.}
  \item{delta.l}{\code{numeric vector}. An optional vector of common level thresholds (with length \code{lN - 1}). This vector represents the difficulty of each level. If not provided, the function will generate it.}
  \item{theta}{\code{numeric vector}. An optional vector of person abilities. If not provided, abilities will be drawn from a standard normal distribution (mean = 0, sd = 1).}
}
\details{
This function generates polytomous item responses using a logistic IRT model. It first creates or uses given person abilities (\code{theta}) and item thresholds (\code{delta}). Responses are simulated by computing the probability of a person responding in each category of an item based on their ability and the item's threshold. The function allows for items to have different numbers of response categories through the \code{itemLevels} argument.
}
\value{
A list with the following components:
  \item{pN}{The number of people (respondents).}
  \item{iN}{The number of items.}
  \item{lN}{The default number of levels (categories) for the polytomous items.}
  \item{itemLevels}{A vector specifying the number of levels for each item.}
  \item{M}{A list of \code{iN} matrices containing logits (differences between person ability and item threshold) for each person and item.}
  \item{CM}{A list of \code{iN} matrices containing cumulative probabilities for each person and item.}
  \item{U}{A \code{pN} by \code{iN} matrix of uniform random values used to generate the responses.}
  \item{theta}{A numeric vector of person abilities.}
  \item{delta.l}{(Optional) A vector of common level thresholds if it was generated internally.}
  \item{delta.il}{(Optional) A matrix of item-specific deviations for thresholds if \code{delta.l} was generated internally.}
  \item{delta}{A matrix of item thresholds.}
  \item{resp}{A \code{pN} by \code{iN} matrix of simulated responses. Each entry corresponds to the level chosen by the person for the respective item.}
}
\examples{
# Simulate 100 respondents, 5 items, and 3 levels (e.g., Likert scale with 3 options)
simulated_data <- sim.PCM(pN = 100, iN = 5, lN = 3)

# Simulate with custom item thresholds
custom_delta <- matrix(c(0, -1, -2, 0, 0.5, 1, 0, -0.5, -1), ncol = 3)
simulated_data_custom <- sim.PCM(pN = 50, iN = 3, lN = 3, delta = custom_delta)

# Simulate with custom person abilities
custom_theta <- rnorm(100, mean = 0, sd = 1.5)
simulated_data_theta <- sim.PCM(pN = 100, iN = 5, lN = 3, theta = custom_theta)

# Rasch model simulation (dichotomous items)
# 50 persons with abilities ranging from -3 to 3
rasch_theta <- seq(-3, 3, length.out = 50)
rasch_delta <- matrix(c(0, 1.5, 0, -1, 0, 0.5, 0, -0.5, 0, 1, 0, -1.5, 0, 2, 0, -2, 0, 0.8, 0, -0.8), 
                      ncol = 2, byrow = TRUE)
simulated_rasch <- sim.PCM(pN = 50, iN = 10, lN = 2, delta = rasch_delta, theta = rasch_theta)

# Simulation with items having different numbers of levels
# 4 items with 5 levels, 3 items with dichotomous responses, and 3 items with 3 levels
# Simulate with a total of 10 items and varying levels
simulated_mixed_levels <- sim.PCM(pN = 50, iN = 10, lN = 5, 
                                  itemLevels = c(5, 5, 5, 5, 2, 2, 2, 3, 3, 3))
}
\seealso{
\code{\link{runif}}, \code{\link{rnorm}}
}
\author{
David Torres Irribarra
}