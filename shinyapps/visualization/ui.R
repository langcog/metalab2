library(metafor)

ma_choices <- c("Random effects" = "REML",
                "Multi-level random effects with study grouping" = "REML_mv",
                "Fixed effects" = "FE",
                "Empirical bayes" = "EB")

scatter_choices <- c("Locally-linear regression (loess)" = "loess",
                     "Weighted linear model (lm)" = "lm")

es_choices <- c("Cohen's d" = "d",
                "Hedges' g" = "g",
                "Pearson r" = "r",
                "Fisher's z" = "z",
                "Log odds ratio" = "log_odds")

shinyUI(
  dashboardPage(
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
              downloadButton("download_data", "Download data",
                             class = "btn-xs pull-right"),
              br(),
              selectInput("dataset_name", label = "Dataset",
                          choices = datasets$name),
              selectInput("ma_method", label = "Meta-analytic model",
                          choices = ma_choices, selected = "REML"),
              fluidRow(
                column(
                  width = 4,
                  selectInput("es_type", label = "Effect size type",
                              choices = es_choices, selected = "d")
                ),
                column(
                  width = 8,
                  uiOutput("moderator_input")
                )
              )
          ),
          conditionalPanel(
            condition = "output.longitudinal == 'FALSE'",
            box(width = NULL, #status = "danger",
                fluidRow(
                  column(
                    width = 10,
                    p(strong("Scatter plot"), "of effect sizes over age")),
                  column(
                    width = 2,
                    downloadButton("download_scatter", "Save",
                                   class = "btn-xs pull-right"))),
                column(
                  width = 7,
                  # uiOutput("select_scatter_curve")),
                  selectInput("scatter_curve", label = "Curve type",
                              choices = scatter_choices, selected = "loess")),
                plotlyOutput("scatter"), height = 530)
          ),
          box(width = NULL, #status = "danger",
              fluidRow(
                column(width = 10,
                       p(strong("Funnel plot"), "of bias in effect sizes")),
                column(width = 2,
                       downloadButton("download_funnel", "Save",
                                      class = "btn-xs pull-right"))),
              plotOutput("funnel"),
              div(class = "text-center", textOutput("funnel_test"))),
          box(width = NULL, #status = "danger",
              fluidRow(
                column(width = 10,
                       p(strong("Violin plot"), "of effect size density")),
                column(width = 2,
                       downloadButton("download_violin", "Save",
                                      class = "btn-xs pull-right"))),
              plotlyOutput("violin", height = "auto")
          )),
        column(
          width = 6,
          fluidRow(
            uiOutput("viz_boxes")),
          fluidRow(
            box(width = NULL, #status = "danger",
                fluidRow(
                  column(
                    width = 10,
                    p(strong("Forest plot"),
                      "of effect sizes and meta-analysis model estimates")),
                  column(
                    width = 2,
                    downloadButton("download_forest", "Save",
                                   class = "btn-xs pull-right"))),
                column(
                  width = 4,
                  selectInput("forest_sort", label = "Sort order",
                              choices = c("weight (1/variance)" = "variances",
                                          "effect size" = "effects",
                                          "model estimate" = "estimate",
                                          "alphabetical" = "study_ID",
                                          "chronological" = "year"))),
                plotOutput("forest", height = "auto")),
            box(width = NULL, #status = "danger",
                fluidRow(
                  column(width = 12,
                         p(strong("Meta-analytic model summary")),
                         tabsetPanel(
                           tabPanel("Plot",
                                    plotOutput("forest_summary", height = "auto")),
                           tabPanel("Model",
                                    p(verbatimTextOutput("forest_summary_text")))
                         )
                  )
                )
            )
          )
        )
      )
    )
  )
)
