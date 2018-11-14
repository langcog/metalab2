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
          title = "Experiment planning",
          p("Select a meta-analysis and a set of moderators to see statistical
             power estimates using the estimated effect size (random effects) for that
             phenomenon."),
          uiOutput("domain_selector"),
          uiOutput("dataset_name"),
          # selectInput("dataset_name_pwr", "Meta-analysis",
          #             choices = datasets$name),
          fluidRow(
            column(
              width = 5,
              uiOutput("pwr_moderator_input"),
              uiOutput("pwr_moderator_choices")
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "output.subset_options",
                uiOutput("subset_selector"),
                bsPopover("subset_selector", title = NULL,
                          content = HTML("<small>Restrict the data by the following criteria</small>"),
                          placement = "right")
              )
            )
          )
        ),
      fluidRow(
        valueBoxOutput("power_d", width = 6),
        valueBoxOutput("power_n", width = 6)),
      box(width = NULL, #status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Power plot"), "of N necessary to achieve p < .05")),
            column(width = 2,
                   downloadButton("download_power", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("power"),
          p("Statistical power to detect a difference between
            conditions at p < .05. Dashed line shows 80% power, dotted line
            shows necessary sample size to achieve that level of power.")))))))
