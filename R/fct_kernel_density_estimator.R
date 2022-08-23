#' kernel_density_estimator
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_kernel_function <- function(string) {
  if (string == "Gaussian") {
    dnorm
  } else if (string == "Epanechnikov") {
    function(x) 3 / 4 * (1 - x^2) * (abs(x) <= 1)
  } else if (string == "Bisquare") {
    function(x) 15 / 16 * (1 - x^2)^2 * (abs(x) <= 1)
  } else if (string == "Uniform") {
    function(x) dunif(x, min = -1, max = 1)
  }
}


kernel_dens_estimate <- function(x, K, h, sample) {
  n_sample <- length(sample)
  map_dbl(sample, ~(K((x - .) / h) / (n_sample * h))) %>%
    sum()
}
