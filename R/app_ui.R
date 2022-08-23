#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      theme = bslib::bs_theme(
        # Colors (background, foreground, primary)
        bg = 'white',
        fg = '#06436e',
        primary = colorspace::lighten('#06436e', 0.3),

        # Fonts (Use multiple in case a font cannot be displayed)
        base_font = c('Source Sans Pro',  'Lato', 'Merriweather', 'Roboto Regular', 'Cabin Regular'),
        heading_font = c('Oleo Script', 'Prata', 'Roboto', 'Playfair Display', 'Montserrat'),
        font_scale = 1
      ),
      navbarPage(
        'Statistical Concepts',
        tabPanel(
          "Variance of the sample variance",
          mod_var_of_sample_var_ui("var_of_sample_var_1")
        ),
        tabPanel(
          "Kernel density estimator",
          mod_kernel_density_estimator_ui("kernel_density_estimator_1"),
        ),
        tabPanel(
          "Confidence intervals",
          mod_confidence_intervals_ui("confidence_intervals_1")
        ),
        tabPanel(
          'About',
          fluidRow(
            # White space left and right avoids wall of text.
            column(2),
            column(8, div(includeMarkdown('inst/app/www/about.md'))),
            column(2)
          )
        )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Statistical Concepts"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
