shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        includeCSS("../common/www/custom.css"),

        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        h2("MetaLab Dataset Validation Tool"),
        selectInput("dataset", "Select a dataset to validate", datasets$name,
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        h3("Validation Results"),
        h4("Dataset Key Present"),
        textOutput("check_key"),
        h4("Dataset Key Found in metadata.yaml"),
        textOutput("check_data_exists"),
        h4("Dataset Loaded from Google Sheets"),
        textOutput("check_fetch"),
        h4("Field Validation Results"),
        dataTableOutput("field_validation")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Dataset Information",
                   h3(paste("Information entered in datasets.yaml file")),
                   verbatimTextOutput("dataset_spec")
                   ),
          tabPanel("Fields Information",
                   dataTableOutput("fields_spec")
                   )
        )
      )
    )
  )
)
