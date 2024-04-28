#' ntickets
#'
#' @param N
#' @param gamma
#' @param p
#'
#' @return numeric vector
#' @export
#'
#' @examples ntickets(N=400, gamma = 0.2, p=0.95)
ntickets <- function(N, gamma, p) {
  # nd using the binomial distribution
  nd <- qbinom(1 - gamma, N, p, lower.tail = FALSE)

  # nc using the normal approximation
  meannc <- N * p
  sdnc <- sqrt(N * p * (1 - p))
  nc <- qnorm(1 - gamma, mean = meannc, sd = sdnc, lower.tail = FALSE)

  # list
  result <- list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
  print(result)

  # plot of Objective functions Vs. n for the discrete case
  nvalues <- seq(1, N + 50, by = 1)
  discretecase <- 1 - gamma - pbinom(nvalues, N, p, lower.tail = TRUE)
  plot(nvalues, discretecase, type = "l", col = "red",
       xlab = "n",
       ylab = "Objective",
       main = "Objective Vs. n to find optimal tickets sold for discrete")

  # plot of Objective function Vs. n for the continuous case
  continuouscase <- 1 - gamma - pnorm(nvalues, meannc, sdnc, lower.tail = TRUE)
  plot(nvalues, continuouscase, type = "l", col = "blue",
       xlab = "n",
       ylab = "Objective",
       main = "Objective Vs. n to find optimal tickets sold for continuous")
}
ntickets(N=400, gamma=0.2, p=0.95)
