shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(includeHTML("./google-analytics.html")),
  includeCSS("../common/www/custom.css"),
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  fluidRow(
    column(
      width = 6,
      box(width = NULL, #status = "danger",
          solidHeader = TRUE,
          title = strong("Experiment planning"),
          p("Select a meta-analysis and a set of moderators to see statistical
             power estimates using the estimated effect size (random effects) for that
             phenomenon."),
          uiOutput("domain_selector"),
          bsPopover("domain_selector", title = NULL,
                    content = HTML("<small>Select a disorder<small>"),
                    placement = "right"),
          uiOutput("dataset_name"),
          bsPopover("dataset_name", title = NULL,
                    content = HTML("<small>Select a dataset/meta-analysis<small>"),
                    placement = "right"),
          fluidRow(
            column(
              width = 4,
              uiOutput("feature_selector"),
              bsPopover("feature_selector", title = NULL,
                        content = HTML("<small>Select a feature</small>"),
                        placement = "right"),
              uiOutput("feature_help_text")),
            column(
              width = 4,
              uiOutput("pwr_moderator_input"),
              bsPopover("pwr_moderator_input", title = NULL,
                        content = HTML("<small>Explore the impact of continuous and categorical moderator variables</small>"),
                        placement = "right")
            ),
            column(
              width = 4,
              uiOutput("pwr_moderator_choices"),
              bsPopover("pwr_moderator_choices", title = NULL,
                        content = HTML("<small>Choose a value of the chosen moderator</small>"),
                        placement = "right")
            )
          )
        ),
      uiOutput("conditional_results")
      )))))
