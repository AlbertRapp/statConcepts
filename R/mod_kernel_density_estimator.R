#' kernel_density_estimator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kernel_density_estimator_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(10,
             h1("Kernel density Estimator"),
             withMathJax(
               "A common estimator for the density function \\(f \\) using a sample \\((X_1, \\ldots, X_n)\\) is given by
                $$
                    \\hat{f}_n(x) = \\frac{1}{nh} \\sum_{k = 1}^n K\\bigg( \\frac{x - X_k}{h} \\bigg),
                $$
                where \\( K \\) is a so-called kernel function and \\(h > 0\\) is the so-called bandwidth operator.
                While this formula may feel intimidating, the idea is actually pretty simple.
                First, we pick a kernel function.
                Often, this is just the density function of a distribution.
                Imagine that we pick the density function from a standard normal distribution.
                Then, second, we 'place' one standard normal density function at each \\( X_k \\) and scale that by \\( h \\).
                Finally, for each \\(x \\in \\mathbb{R} \\) we average the function values of all kernel functions.
                "
             ),
             p(HTML("
                   For a visualization of this check out the animation I built a while back <a href='https://albert-rapp.de/post/visualize-kernel-density-estimation/kernelAnimation.gif'> here </a>.
                   For an interactive demonstration of kernel density estimation let us simulate samples from an exponential distribution.
                   In this first panel, notice how the width of the kernel function changes with the bandwidth.
                   This is essentially the role of the bandwith.
                   Also, see how this changes the density estimates.
                   To avoid that the picture becomes too crowded with kernel functions, here let us stick to small sample sizes.
                  ")),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   ns("kernel_bandwidth"),
                   "Bandwidth",
                   min = 0,
                   max = 4,
                   value = 1,
                   step = 0.1,
                   ticks = F
                 ),
                 selectInput(
                   ns("kernel_fct"),
                   "Kernel function",
                   choices = c("Gaussian", "Epanechnikov", "Bisquare", "Uniform")
                 ),
                 p("To simulate a new underlying sample, adjust the following sliders and hit the button."),
                 sliderInput(
                   ns("kernel_sample_length"),
                   "Sample length",
                   min = 1,
                   max = 10,
                   value = 5,
                   step = 1,
                   ticks = F
                 ),
                 sliderInput(
                   ns("kernel_lambda"),
                   "Parameter lambda",
                   min = 0.01,
                   max = 1.5,
                   value = 1 / 2,
                   ticks = F
                 ),
                 actionButton(ns("kernel_sample_draw"), label = "Draw new sample", width = "100%")
               ),
               mainPanel(
                 plotOutput(ns("kernel_plot1"))
               )
             ),
             p(
               "Now that you (hopefully) understand the mechanism behind kernel density estimation,
                try to find a bandwidth such that the estimation resembles the true density.
                Maybe you can increase the sample size to improve the quality of the estimate."
             ),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   ns("kernel_bandwidth2"),
                   "Bandwidth",
                   min = 0,
                   max = 4,
                   value = 1,
                   step = 0.05,
                   ticks = F
                 ),
                 selectInput(
                   ns("kernel_fct2"),
                   "Kernel function",
                   choices = c("Gaussian", "Epanechnikov", "Bisquare", "Uniform")
                 ),
                 p("To simulate a new underlying sample, adjust the following sliders and hit the button."),
                 sliderInput(
                   ns("kernel_sample_length2"),
                   "Sample length",
                   min = 1,
                   max = 200,
                   value = 15,
                   step = 1,
                   ticks = F
                 ),
                 sliderInput(
                   ns("kernel_lambda2"),
                   "Parameter lambda",
                   min = 0.01,
                   max = 1.5,
                   value = 1 / 2,
                   ticks = F
                 ),
                 actionButton(ns("kernel_sample_draw2"), label = "Draw new sample", width = "100%")
               ),
               mainPanel(
                 plotOutput(ns("kernel_plot2"))
               )
             )
      ),
      column(1)
    )
  )
}

#' kernel_density_estimator Server Functions
#'
#' @noRd
mod_kernel_density_estimator_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    lambda_kernel <- reactive({input$kernel_lambda})
    n_kernel <- reactive({input$kernel_sample_length})
    kernel_fct <- reactive({
      get_kernel_function(input$kernel_fct)
    })

    bw_kernel <- reactive({input$kernel_bandwidth})
    sample1 <- reactiveVal(rexp(5, rate = 1 / 2))
    observeEvent(
      input$kernel_sample_draw,
      sample1({rexp(n_kernel(), rate = lambda_kernel())})
    )

    output$kernel_plot1 <- renderPlot({
      kernels_from_sample <- tibble(
        Z = sample1()
      ) %>%
        mutate(
          group = seq_along(Z),
          x = list(seq(-min(Z) - 2,  max(Z) + 2, 0.01))
        ) %>%
        tidyr::unnest(x) %>%
        mutate(kernel = purrr::map2_dbl(
          Z, x, function(x, y) {
            fct <- kernel_fct()
            fct((y - x) / bw_kernel()) / bw_kernel()
          }
        )
        )

      estimates <- kernels_from_sample %>%
        group_by(x) %>%
        summarise(est = mean(kernel))


      kernels_from_sample %>%
        ggplot(aes(x = x, y = kernel)) +
        geom_line(aes(group = group, col = "Kernel function"), size = 1) +
        geom_segment(
          aes(x = Z, xend = Z, y = 0, yend = kernel,
              col = "Sample"),
          linetype = 2,
          size = 0.25
        ) +
        geom_line(
          data = estimates,
          aes(x = x, y = est, col = "Estimation"),
          size = 1
        ) +
        geom_point(
          aes(x = Z, y = 0,
              col = "Sample"),
          size = 3
        ) +
        scale_y_continuous(minor_breaks = NULL) +
        scale_x_continuous(minor_breaks = NULL) +
        scale_color_manual(values = c(oki_vermillion, "grey80", oki_blue)) +
        theme_light() +
        theme(text = element_text(size = 14)) +
        labs(x = "x", y = element_blank(), color = element_blank())
    })


  lambda_kernel2 <- reactiveVal(1 / 2)
  n_kernel2 <- reactive({input$kernel_sample_length2})
  kernel_fct2 <- reactive({
    get_kernel_function(input$kernel_fct2)
  })
  bw_kernel2 <- reactive({input$kernel_bandwidth2})
  sample2 <- reactiveVal(rexp(15, rate = 1 / 2))
  observeEvent(
    input$kernel_sample_draw2,{
      lambda_kernel2(input$kernel_lambda2)
      sample2({rexp(n_kernel2(), rate = lambda_kernel2())})
    }
  )

  output$kernel_plot2 <- renderPlot({
    kernel_sample <- tibble(
      Z = sample2()
    )
    min_range <- min(kernel_sample$Z)
    max_range <- max(kernel_sample$Z)

    dens_estimate <- tibble(
      x = seq(min_range, max_range, 0.01),
    ) %>%
      mutate(
        est = map_dbl(
          x, ~kernel_dens_estimate(., kernel_fct2(), bw_kernel2(), kernel_sample$Z)
        )
      )

    kernel_sample <- kernel_sample %>%
      mutate(est = map_dbl(
        Z, ~kernel_dens_estimate(., kernel_fct2(), bw_kernel2(), kernel_sample$Z)
      ))

    dens_estimate %>%
      ggplot(aes(x)) +
      stat_function(
        fun = ~dexp(., rate = lambda_kernel2()),
        aes(col = "True density"),
        size = 1
      )  +
      geom_line(mapping = aes(y = est, col = "Estimation"), size = 1) +
      geom_point(
        data = kernel_sample,
        aes(y = 0, x = Z, col = "Sample"),
        size = 3
      ) +
      geom_segment(
        data = kernel_sample,
        aes(y = 0, yend = est, x = Z, xend = Z, col = "Sample"),
        linetype = 2,
        size = 0.25
      ) +
      theme_light() +
      theme(
        text = element_text(size = 14)
      ) +
      scale_color_manual(values = c(oki_vermillion, oki_blue, "grey80"))
  })

  })
}

## To be copied in the UI
# mod_kernel_density_estimator_ui("kernel_density_estimator_1")

## To be copied in the server
# mod_kernel_density_estimator_server("kernel_density_estimator_1")
