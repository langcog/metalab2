ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        includeCSS("../common/www/custom.css"),

        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        h2("MetaLab Dataset Validation Tool"),
        tabsetPanel(
          tabPanel("MetaLab Data",
                   selectInput("dataset", "Select a dataset to validate", dataset_info$name,
                               selected = NULL, multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL),
                   h3("Validation Results"),
                   h4("Dataset Key Present"),
                   textOutput("check_key_m"),
                   h4("Dataset Key Found in metadata.yaml"),
                   textOutput("check_data_exists_m"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_m"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_m")),
          tabPanel("External URL",
                   textInput("google_sheet_url", "Google Sheets URL", ""),
                   actionButton("validate_sheet_button", "Validate Sheet"),
                   h3("Validation Results"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_g"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_g")),
          tabPanel("Upload CSV",
                   fileInput("csv_file", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   h3("Validation Results"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_csv"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_csv")))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Dataset Information",
                   h3("Information in",
                      a("datasets.yaml",
                        href = "https://github.com/langcog/metalab2/blob/master/metadata/datasets.yaml"),
                      "file"),
                   dataTableOutput("dataset_spec")
                   ),
          tabPanel("Fields Information",
                   dataTableOutput("fields_spec")
                   )
        )
      )
    )
  )
)
