shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    includeCSS("../common/www/custom.css"),
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    fluidRow(
      column(
        width = 6,
        box(width = NULL, #status = "danger",
            solidHeader = TRUE,
            title = "Experiment simulation",
            p(
              "Run a simulation of a looking-time experiment, choosing an effect
               size and a number of participants per group. See the results of
               statistical comparisons for within-subjects effects (t-test) and
               for comparison with a negative control group (ANOVA interaction)."
            ),
            sliderInput("N", "Number of infants per group (N)",
                        min = 4, max = 120, value = 16, step = 2),
            sliderInput("d_pwr", "Effect size (Cohen's d)",
                        min = 0, max = 2, step = .1,
                        value = .5),
            fluidRow(
              column(
                width = 6,
                radioButtons("control", "Conditions",
                             choices = list("Experimental only" = FALSE,
                                            "Experimental & control" = TRUE))),
              column(
                width = 6,
                radioButtons("interval", "Type of error bars",
                             choices = list("Standard error of the mean" = "sem",
                                            "95% confidence interval" = "ci"),
                             selected = "ci"))
            ),
            selectInput("pwr_bar", "Plot type",
                        choices = list("Bar graph" = TRUE,
                                       "Scatter plot" = FALSE),
                        selected = TRUE),
            actionButton("go", label = "Sample Again"),
            hr(),
            strong("Simulated data"),
            conditionalPanel(condition = "input.pwr_bar == 'FALSE'",
                             plotOutput("pwr_scatter")),
            conditionalPanel(condition = "input.pwr_bar == 'TRUE'",
                             plotOutput("pwr_bar")),
            br(),
            strong("Simulated statistical tests"),
            textOutput("stat")
        )
      )
    )
  )
))
