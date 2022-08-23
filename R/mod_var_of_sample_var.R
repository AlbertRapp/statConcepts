#' var_of_sample_var UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_var_of_sample_var_ui <- function(id){
  ns <- NS(id)

  estimator_tabs <- tabsetPanel(
    id = ns("estimators"),
    type = "hidden",
    tabPanel(
      "none"
    ),
    tabPanel(
      "mean",
      htmlOutput(ns("description_mean"), container = p),
      uiOutput(ns("simu_panel_mean"))
    ),
    tabPanel(
      "variance",
      tabsetPanel(
        id = ns("variance"),
        type = "hidden",
        tabPanel(
          "sure_about_variance",
          p("So, you want to look at the variance of the sample variance.
              That is a bold choice.
              But did you already look at the sample mean?
              If so, then proceed.
              Otherwise, you may want to look at the that first.
              Do you really want to proceed with the sample variance?"),
          selectInput(
            ns("proceed_variance"),
            label = NULL,
            choices = c("You sure?", "Yes, I know what I am doing.")
          )
        ),
        tabPanel(
          "proceed_variance",
          htmlOutput(ns("description_var"), container = p),
          uiOutput(ns("simu_panel_var"))
        )
      )
    )
  )


  tagList(
    fluidRow(
      # White space left and right avoids wall of text.
      column(1),
      column(10,
             h1("Variance of an estimator"),
             p(
               "In this somewhat short and basic web app we want to explore the notion of an estimator's variance.
                This is just to help clarify things like 'the variance of the sample variance'
                which might sound somewhat convoluted and complex.
                In fact, it is not. This is what we want to learn here.
                To do so, we will look at the sample mean and the sample variance
                which are estimators for the mean or the variance of the underlying distribution
                of a sample.
                In mathematical notation, we will look at "
             ),
             withMathJax("$$\\overline{X}_n = \\frac{1}{n} \\sum_{k = 1}^n X_k \\quad \\text{or} \\quad S_n^2 = \\frac{1}{n - 1} \\sum_{k = 1}^n (X_k - \\overline{X}_n)^2$$"),
             selectInput(
               ns("estimator_choice"),
               "Before we begin, choose an Estimator.",
               choices = c("Make a choice", "Sample mean", "Sample variance")
             ),
             estimator_tabs
      ),
      column(1)
    )

  )
}

#' var_of_sample_var Server Functions
#'
#' @noRd
mod_var_of_sample_var_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    isNone <- reactive({stringr::str_detect(input$estimator_choice, "Make")})
    isMean <- reactive({stringr::str_detect(input$estimator_choice, "mean")})
    isVariance <- reactive({!(isMean() | isNone())})

    observeEvent(input$estimator_choice, {
      updateTabsetPanel(
        inputId = "estimators",
        selected = dplyr::case_when(
          isMean() ~ "mean",
          isVariance() ~ "variance",
          isNone() ~ "none"
        )
      )
    })

    # Proceed with variance?
    observeEvent(input$proceed_variance, {
      updateTabsetPanel(
        inputId = "variance",
        selected = dplyr::case_when(
          stringr::str_detect(input$proceed_variance, "Yes") ~ "proceed_variance",
          TRUE ~ "sure_about_variance"
        )
      )
    })


    # Description
    description_mean <- reactiveVal(value = generate_descriptions("mean", 0))
    description_var <- reactiveVal(value = generate_descriptions("var", 0))
    output$description_mean <- renderText({
      withMathJax()
      description_mean()
    })
    output$description_var <- renderText({description_var()})

    # Handle simu panel
    max_samples_mean <- reactiveVal(value = 1) # updated after first draw
    max_samples_var <- reactiveVal(value = 1)
    output$simu_panel_mean <- renderUI(
      isolate(create_sidebar_layout("mean", max_samples_mean(), ns))
    )
    output$simu_panel_var <- renderUI(
      isolate(create_sidebar_layout("var", max_samples_var(), ns))
    )

    # Handle graphics
    observeEvent(input$draw_sample_mean, {
      updateSliderInput(
        inputId = "n_samples_mean",
        max = 100,
        step = 1
      )
      output$plot_mean <- renderPlot({
        create_plot(
          sample_length = isolate(input$sample_length_mean),
          mu = isolate(input$true_mean_mean),
          sd = isolate(sqrt(input$true_var_mean)),
          n_samples = isolate(input$n_samples_mean),
          estimator = mean
        )
      })
    })
    observeEvent(input$draw_sample_var, {
      updateSliderInput(
        inputId = "n_samples_var",
        max = 100,
        step = 1
      )
      output$plot_var <- renderPlot({
        create_plot(
          sample_length = isolate(input$sample_length_var),
          mu = isolate(input$true_mean_var),
          sd = isolate(sqrt(input$true_var_var)),
          n_samples = isolate(input$n_samples_var),
          estimator = var
        )
      })
    })


    ## Buttons and counters and n_samples slider
    alert_time_text_change <- 250
    observeEvent(input$draw_sample_mean, {max_samples_mean(100)})
    observeEvent(input$draw_sample_var, {max_samples_var(100)})

    c_draws_mean <- reactiveVal(value = 0)
    observeEvent(input$draw_sample_mean, {
      tmp <- c_draws_mean()
      c_draws_mean(tmp + 1)
    })
    c_draws_var <- reactiveVal(value = 0)
    observeEvent(input$draw_sample_var, {
      tmp <- c_draws_var()
      c_draws_var(tmp + 1)
    })

    stage_mean <- reactiveVal(value = 0)
    observeEvent(input$prev_button_mean, {
      tmp <- stage_mean()
      stage_mean(max(tmp - 1, 0))
      description_mean(generate_descriptions("mean", stage_mean()))
      shinyjs::delay(alert_time_text_change, description_mean(generate_descriptions("mean", stage_mean(), highlight = F)))
    })
    observeEvent(input$next_button_mean, {
      if (c_draws_mean() >= 1) {
        if (!stringr::str_detect(description_mean(), "Yay")) {
          tmp <- stage_mean()
          stage_mean(tmp + 1)
          description_mean(generate_descriptions("mean", stage_mean()))
          shinyjs::delay(alert_time_text_change, description_mean(generate_descriptions("mean", stage_mean(), highlight = F)))
        }
      }

    })

    stage_var <- reactiveVal(value = 0)
    observeEvent(input$prev_button_var, {
      tmp <- stage_var()
      stage_var(max(tmp - 1, 0))
      description_var(generate_descriptions("var", stage_var()))
      shinyjs::delay(alert_time_text_change, description_var(generate_descriptions("var", stage_var(), highlight = F)))
    })
    observeEvent(input$next_button_var, {
      if (c_draws_var() >= 1) {
        if (!stringr::str_detect(description_var(), "Hooray")) {
          tmp <- stage_var()
          stage_var(tmp + 1)
          description_var(generate_descriptions("var", stage_var()))
          shinyjs::delay(alert_time_text_change, description_var(generate_descriptions("var", stage_var(), highlight = F)))
        }
      }

    })
  })
}






## To be copied in the UI
# mod_var_of_sample_var_ui("var_of_sample_var_1")

## To be copied in the server
# mod_var_of_sample_var_server("var_of_sample_var_1")
