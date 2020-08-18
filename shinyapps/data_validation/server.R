suppressMessages(suppressWarnings({
  library(dplyr)
  library(purrr)
  library(here)
  library(DT)
  library(metalab)
}))

server <- function(input, output, session) {
  output$dataset_spec <- renderDT(
    data.frame(column = names(dataset_info),
               value = t(dataset_info %>% filter(name == input$dataset))),
    rownames = "", options = list(dom = 't', pageLength = 50)
  )

  output$fields_spec <- renderDT(make_fields(fields))
  output$selected_name <- renderText(input$dataset)

  output$check_key_m <- renderText(check_key(dataset_info %>% filter(name == input$dataset)))
  output$check_data_exists_m <- renderText(check_data_exists(dataset_info %>% filter(name == input$dataset)%>% pull(short_name)))
  output$check_fetch_m <- renderText(fetch_dataset(dataset_info %>% filter(name == input$dataset)))
  
  output$field_validation_m <- renderDT({
    valid_fields <- validate_dataset(dataset_info %>% filter(name == input$dataset),
                                     get_data(dataset_info %>% filter(name == input$dataset)),
                                     fields)

    ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                         Valid = unlist(valid_fields)) %>%
      mutate(Valid = ifelse(Valid, "\u2705", "No")) %>% 
      arrange(Valid)

    return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
  })

  observeEvent(input$validate_sheet_button,
  {
    df <- get_data(data.frame(key = get_google_sheet_id(input$google_sheet_url),
                              name = "input url"))
    output$field_validation_g <-
      renderDT({
        valid_fields <- validate_dataset(data.frame(name = 'from url'), df, fields)

        ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                             Valid = unlist(valid_fields)) %>% arrange(Valid)

        return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
      })
    ## shinyjs::toggle("dataset_spec")    
    output$dataset_spec_m <- renderDT(data.frame(column = "Not available for datasets not included in Metalab",
                                                 value = ""))
  })

  output$field_validation_csv <- renderDT({
    req(input$csv_file)

    df <- read.csv(input$csv_file$datapath,
                   header = TRUE)
    
    valid_fields <- validate_dataset(data.frame(name = 'from csv'), df, fields)

    ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                         Valid = unlist(valid_fields)) %>% arrange(Valid)

    return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
  })
}
