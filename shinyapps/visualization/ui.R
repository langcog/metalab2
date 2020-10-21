library(metafor)

ma_choices <- c("Multi-level random effects with study grouping" = "REML_mv")
                # "Random effects" = "REML",
                # "Fixed effects" = "FE",
                # "Empirical bayes" = "EB")

scatter_choices <- c("Locally-linear regression (loess)" = "loess",
                     "Weighted linear model (lm)" = "lm")

es_choices <- c("Hedges' g" = "g",
                "Cohen's d" = "d",
                "Pearson r" = "r",
                "Log odds ratio" = "log_odds")

# ma_explanations <- c("")

shinyUI(
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(includeHTML("./google-analytics.html")),
      tags$head(tags$style("
                  #data_description{
                  display:inline
                  }")),
      tags$head(tags$style("
                  #data_citation{
                  display:inline
                  }")),
      tags$head(tags$style("
                  #ma_model_blurb{
                  display:inline
                  }")),
      includeCSS("../common/www/custom.css"),
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      fluidRow(
        column(
          width = 6,
          box(width = NULL, #status = "danger",
              solidHeader = TRUE,
              title = strong("Meta-Analytic Visualisations"),
              #br(),
              uiOutput("domain_selector"),
              bsPopover("domain_selector", title = NULL,
                        content = HTML("<small>Select a disorder</small>"),
                        placement = "right"),
              downloadButton("download_data", "Download data",
                             class = "btn-xs pull-right"),
              uiOutput("dataset_name"),
              bsPopover("dataset_name", title = NULL,
                        content = HTML("<small>Select a dataset/meta-analysis</small>"),
                        placement = "right"),
              # br(),
              # selectInput("ma_method", label = "Meta-analytic model",
              #             choices = ma_choices, selected = "REML_mv"),
              # uiOutput("ma_help_text"),
              #br(),
              # bsPopover("ma_method", title = NULL,
              #           content = HTML("<small>Statistical model underlying the aggregated, weighted effect size estimates</small>"),
              #           placement = "right"),
              fluidRow(
                column(
                  width = 4,
                  uiOutput("feature_selector"),
                  bsPopover("feature_selector", title = NULL,
                            content = HTML("<small>Select a feature</small>"),
                            placement = "right"),
                  uiOutput("feature_help_text"),
                  br()
                ),
                column(
                  width = 4,
                  selectInput("es_type", label = "Effect size type", #CHANGE: selectInput!!!!
                              choices = es_choices, selected = "d"),
                  uiOutput("es_help_text"),
                  br(),
                  bsPopover("es_type", title = NULL,
                            content = HTML("<small>Measure for strength of phenomenon</small>"),
                            placement = "right")
                ),
                column(
                  width = 4,
                  uiOutput("moderator_input"),
                  bsPopover("moderator_input", title = NULL,
                            content = HTML("<small>Explore the impact of continuous and categorical moderator variables</small>"),
                            placement = "right")
                ),
              ),

              strong("Dataset Description:", style="display:inline"), textOutput("data_description"),
              br(),
              br(),
              strong("Dataset Citation:", style="display:inline"), textOutput("data_citation"),
              br(),
              br(),
              uiOutput("link_to_dataset"),
              br(),
              strong("Statistical Model:", style="display:inline"), uiOutput("ma_model_blurb")
          ),
          box(width = NULL, #status = "danger",
                fluidRow(
                  column(
                    width = 10,
                    p(strong("Box plot"), "of effect sizes"))
                  ),
                fluidRow(column(
                  width = 7,
                  ## uiOutput("select_scatter_curve"),
                  selectInput("scatter_curve", label = "Curve type",
                              choices = scatter_choices, selected = "loess")),
                  bsPopover("scatter_curve", title = NULL,
                            content = HTML("<small>Select a type of curve</small>"),
                            placement = "right")),
                plotlyOutput("scatter"),
                br(),
                helpText("Select a type of regression line. Dot sizes are inversely related
                         to the standard error of effect size."),
                height = 600),
          uiOutput("experiment_limit_left")
          ),
        column(
          width = 6,
          fluidRow(
            uiOutput("viz_boxes")),
          uiOutput("experiment_limit_right")
            # tags$script(HTML(
            #   "function myFunction() {
            #      var x = document.getElementById('forest_summary_box');
            #     if (x.style.display === 'none') {
            #         x.style.display = 'block';
            #     } else {
            #         x.style.display = 'none';
            #     }
            #   }"
            # ))
          )
        )
      )
    )
  )
