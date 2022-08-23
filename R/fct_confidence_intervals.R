#' confidence_intervals
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
conf <- function(n, mu, sigma, alpha1, alpha2) {
  x <- rnorm(n, mu, sigma)
  lower <- mean(x) - sigma / sqrt(n) * qnorm(alpha2)
  upper <- mean(x) - sigma / sqrt(n) * qnorm(alpha1)
  c(mean = mean(x), lower = lower, upper = upper)
}
