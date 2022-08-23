#' confidence_intervals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_confidence_intervals_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(10,
             h1('Confidence Intervals'),
             withMathJax('In statistical inference, one frequently tries to estimate an unknown parameter
              from a given distribution based on a data sample. For instance, if we assume that
              a sample consists of observations from a normal distribution with unknown mean \\(\\mu\\) and
              known standard deviation \\(\\sigma\\), then we might be interested to estimate \\(\\mu\\).
              But estimating just a single value is often not enough.
              This is where so-called confidence intervals come into play.
              These are constructed by using so-called quantiles from a known distribution.
              To find out what that means, let us take a look at the \\( \\alpha \\)-quantiles of
              the standard normal distribution.
              The following plot shows its density and its entire quantile function.
              Play around with the slider to evaluate the quantile function at a value \\(\\alpha\\)
              to get a feeling for the interplay here.'),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 shinyWidgets::noUiSliderInput(
                   ns('alpha'),
                   label = withMathJax('\\(\\alpha\\) '),
                   min = 0, max = 1, step = 0.01,
                   value = 0.9,
                   orientation = "vertical",
                   direction = 'rtl',
                   width = "100px", height = "300px",
                   color = '#df691a'
                 ),
                 width = 2
               ),
               mainPanel(
                 plotOutput(ns('single_alpha')),
                 width = 10
               )
             ),
             hr(),
             withMathJax('The orange area under the blue curve has an area of exactly \\(\\alpha\\).
            This means that the \\( \\alpha \\)-quantile \\( q_a \\) of a continous distribution
            (like the normal distribution) is defined such that \\( \\mathbb{P}(Z \\leq q_a) = \\alpha \\),
            where \\(Z\\) is just a random variable that follows that distribution.
            Similarly, we can use the exact same approach using quantiles to find a shaded area of any
            given size (simply use two quantiles). '),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 shinyWidgets::noUiSliderInput(
                   ns('alphas'),
                   label = withMathJax('\\(\\alpha\\) '),
                   min = 0, max = 1, step = 0.01,
                   value = c(0.1, 0.9), margin = 0.01,
                   orientation = "vertical",
                   direction = 'rtl',
                   width = "100px", height = "300px",
                   color = '#df691a'
                 ),
                 width = 2
               ),
               mainPanel(
                 plotOutput(ns('two_alphas')),
                 width = 10
               )
             ),
             hr(),
             withMathJax(
               '
            In our setting (normally distributed samples with unknown mean \\(\\mu\\) and known standard
            deviation \\(\\sigma\\)) we know that our statistic
            $$
            T = \\sqrt{n} \\frac{\\overline{X_n} - \\mu}{\\sigma}
            $$
            is standard normally distributed (even if we do not know \\(\\mu\\)).
            Thus, we can use the previous technique to find constants \\(c_1\\) and \\(c_2\\) such that
            $$\\mathbb{P}(c_1 \\leq T \\leq c_2) = \\gamma \\in (0, 1).$$
            Where \\( \\gamma \\) is an arbitrary value in \\( (0, 1) \\).
            The point here is that we can use the standard normal quantile function to find
            constants such that the probability (i.e. the previous orange area) is as large as we want.
            Rewriting the equation we get
            $$
            \\mathbb{P}\\bigg(\\overline{X_n} - \\frac{\\sigma}{n}c_1 \\leq \\mu \\leq \\overline{X_n} - \\frac{\\sigma}{n}c_2\\bigg) = \\gamma.
            $$
            This will give us a random interval and the interpretation here is that
            \\(\\gamma\\) of these randomly generated intervals will contain the
            "true" value \\(\\mu\\).
            To see that, pick two new thresholds for \\( \\alpha \\) to pick two quantiles and simulate \\( N \\)  samples
            of length \\( n \\) from a normal distribution with mean \\(\\mu\\)  and standard
            deviation \\(\\sigma\\) and the resulting plot will tell you how
            many intervals contain \\(\\mu\\).
            '),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   ns('alpha_CIs'),
                   withMathJax('\\(\\alpha\\)'),
                   min = 0,
                   max = 1,
                   value = c(0.1, 0.9),
                   step = 0.01,
                   width = '100%'
                 ),
                 numericInput(
                   ns('N_CIs'),
                   withMathJax('Number of samples \\(N\\)'),
                   min = 1,
                   max = 10000,
                   value = 25,
                   step = 1
                 ),
                 numericInput(
                   ns('n_CIs'),
                   withMathJax('Sample length \\(n\\)'),
                   min = 10,
                   max = 1000,
                   value = 15,
                   step = 1
                 ),
                 sliderInput(
                   ns('mu_CIs'),
                   withMathJax('True mean \\(\\mu\\)'),
                   min = -3,
                   max = 3,
                   value = 0,
                   step = 0.1
                 ),
                 sliderInput(
                   ns('sigma_CIs'),
                   withMathJax('True standard deviation \\(\\sigma\\)'),
                   min = 0.1,
                   max = 5,
                   value = 1,
                   step = 0.1
                 ),
                 checkboxInput(
                   ns('show_means'),
                   'Display sample means?',
                   value = F
                 )
               ),
               mainPanel(
                 plotOutput(ns('plot_CIs'), height = '575px' )
               )
             ),
             hr(),
             p('Spoiler alert: There is a lot to learn from this simple plot and
            I will spill it out for you.
              So if you want to play around with the plot, you may want to do
              that first, before you read on.'),
             checkboxInput(
               ns('readon_CI'),
               'If you want to continue reading, check this box.'
             ),
             uiOutput(ns('readon_CI_UI'))
      ),
      column(1)
    )
  )
}

#' confidence_intervals Server Functions
#'
#' @noRd
#' @import patchwork
mod_confidence_intervals_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    normal_tib <- reactive({
      tibble(
        x = seq(-5, 5, 0.01),
        y = dnorm(x)
      )
    })

    quantile_tib <- reactive({
      tibble(
        x = seq(0, 1, 0.01),
        y = qnorm(x)
      )
    })

    output$single_alpha <- renderPlot({
      dens_plot <- normal_tib() %>%
        ggplot(aes(x = x)) +
        geom_ribbon(
          data = normal_tib() %>% filter(x <= qnorm(input$alpha)),
          aes(ymin = 0, ymax = y),
          col = NA,
          fill = oki_vermillion
        ) +
        geom_line(aes(y = y), size = 1.5, col = oki_blue) +
        ggtitle(label = paste('The size of the orange area is equal to', input$alpha))

      quant_plot <- quantile_tib() %>%
        ggplot(aes(x, y)) +
        geom_line(size = 1.5, col = oki_blue) +
        geom_segment(
          data = quantile_tib() %>% filter(x == input$alpha),
          aes(xend = x, yend = -3),
          linetype = 2,
          size = 1,
          col = oki_vermillion
        ) +
        geom_segment(
          data = quantile_tib() %>% filter(x == input$alpha),
          aes(xend = 0, yend = y),
          linetype = 2,
          size = 1,
          col = oki_vermillion
        ) +
        geom_point(
          data = quantile_tib() %>% filter(x == input$alpha),
          size = 3,
          col = oki_vermillion
        ) +
        geom_hline(yintercept = 0, col = 'black') +
        coord_cartesian(expand = F) +
        ggtitle('The point describes the right cutoff point')

      dens_plot + quant_plot & theme_light() & theme(
        text = element_text(face = 'bold'),
        plot.title.position = 'plot'
      )
    })



    alpha1 <- reactive(input$alphas[1])
    alpha2 <- reactive(input$alphas[2])

    output$two_alphas <- renderPlot({
      dens_plot2 <- normal_tib() %>%
        ggplot(aes(x = x)) +
        geom_ribbon(
          data = normal_tib() %>% filter(between(x, qnorm(alpha1()), qnorm(alpha2()))),
          aes(ymin = 0, ymax = y),
          col = NA,
          fill = oki_vermillion
        ) +
        geom_line(aes(y = y), size = 1.5, col = oki_blue) +
        ggtitle(label = paste('The size of the orange area is equal to', alpha2() - alpha1()))


      quantile_tib <- tibble(
        x = seq(0, 1, 0.01),
        y = qnorm(x)
      )

      quant_plot2 <- quantile_tib() %>%
        ggplot(aes(x, y)) +
        geom_line(size = 1.5, col = oki_blue) +
        geom_segment(
          data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()),
          aes(xend = x, yend = -3),
          linetype = 2,
          size = 1,
          col = oki_vermillion
        ) +
        geom_segment(
          data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()),
          aes(xend = 0, yend = y),
          linetype = 2,
          size = 1,
          col = oki_vermillion
        ) +
        geom_point(
          data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()),
          size = 3,
          col = oki_vermillion
        ) +
        geom_hline(yintercept = 0, col = 'black') +
        coord_cartesian(expand = F) +
        ggtitle('The points describes the left and right cutoff points')

      dens_plot2 + quant_plot2 & theme_light() & theme(
        text = element_text(face = 'bold'),
        plot.title.position = 'plot'
      )
    })



    alpha1_CIs <- reactive({input$alpha_CIs[1]})
    alpha2_CIs <- reactive({input$alpha_CIs[2]})
    N <-  reactive({input$N_CIs})
    n <-  reactive({input$n_CIs})
    sigma <- reactive({input$sigma_CIs})
    mu <- reactive({input$mu_CIs})
    display_mean <- reactive({input$show_means})

    CIs <- reactive({
      tibble(
        i = 1:N(),
        CI = map(i, ~conf(n(), mu(), sigma(), alpha1_CIs(), alpha2_CIs()))
      ) %>%
        tidyr::unnest_wider(CI) %>%
        mutate(mu_contained = map2_lgl(lower, upper, ~between(mu(), .x, .y)))
    })

    mu_contained <- reactive({mean(CIs()$mu_contained)})

    output$plot_CIs <- renderPlot({
      p <- CIs() %>%
        ggplot(aes(x = lower, xend = upper, y = i, yend = i)) +
        geom_segment() +
        geom_vline(xintercept = mu(), col = oki_vermillion, size = 2) +
        coord_cartesian(xlim = c(mu() - 1, mu() + 1)) +
        ggtitle(paste0('Intervals containing true mean: ', scales::percent(mu_contained(), 0.1),
                       ' (Difference of alphas: ', scales::percent(alpha2_CIs() - alpha1_CIs(), 0.1), ')')) +
        theme_light() +
        theme(
          text = element_text(face = 'bold'),
          plot.title.position = 'plot'
        ) +
        scale_y_continuous(labels = round) +
        labs(x =  element_blank(), y = element_blank())

      if (display_mean()) {
        p <- p +
          geom_point(aes(x = mean), size = 3, col = oki_blue)
      }
      p
    })

    output$readon_CI_UI <- renderUI({
      if (input$readon_CI) {
        withMathJax(includeMarkdown('inst/app/www/CIs.md'))
      }
    })

  })
}

## To be copied in the UI
# mod_confidence_intervals_ui("confidence_intervals_1")

## To be copied in the server
# mod_confidence_intervals_server("confidence_intervals_1")
