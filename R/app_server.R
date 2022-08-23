#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_var_of_sample_var_server("var_of_sample_var_1")
  mod_kernel_density_estimator_server("kernel_density_estimator_1")
  mod_confidence_intervals_server("confidence_intervals_1")
}
