shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        includeCSS("../common/www/custom.css"),

        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        selectInput("dataset", "Select a dataset to validate", datasets$name,
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        h3("Validation Results"),
        textOutput("result")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Information",
                   h3(paste("Information entered in datasets.yaml file")),
                   verbatimTextOutput("dataset_spec")
                   ),
          tabPanel("Fields",
                   dataTableOutput("fields_spec")
                   )
        )
      )
    )
  )
)
