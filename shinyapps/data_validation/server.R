suppressMessages(suppressWarnings({
  library(dplyr)
  library(purrr)
  library(here)
  library(DT)
}))

# Validate dataset's values for all fields
validate_dataset <- function(dataset_meta, dataset_contents) {
  valid_fields <- map(fields, function(field) {
                        validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
  valid_dataset <- all(unlist(valid_fields) == "OK")
  return(valid_dataset)
}

fetch_dataset <- function(dataset_meta) {

  if (dataset_meta$key == "") {
    return(sprintf("Can't load dataset '%s', key missing.\n", dataset_meta$name))
  }

  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  tryCatch({
    dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    sprintf("Can't load dataset '%s' with key '%s'. Exception: %s.\n", dataset_meta$name,
                dataset_meta$key, e)
  })

}

# Manipulate a dataset's contents to prepare it for saving
tidy_dataset <- function(dataset_meta, dataset_contents) {

  # Coerce each field's values to the field's type, discard any columns not in
  # field spec, add NA columns for missing (optional) fields
  dataset_data <- data_frame(row = 1:nrow(dataset_contents))
  for (field in fields) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "string") {
        field_fun <- as.character
      } else if (field$type == "numeric") {
        field_fun <- as.numeric
      } else {
        field_fun <- function(x) x
      }
      dataset_data[,field$field] <- field_fun(dataset_contents[[field$field]])
    } else {
      dataset_data[,field$field] <- NA
    }
  }

  # Impute values for missing correlations
  set.seed(111)
  # First we replace corr values outside the range (.01,.99) with NA
  dataset_data = dataset_data %>%
    mutate(corr = abs(corr)) %>%
    mutate(corr = ifelse(corr > .99 | corr < .01, NA, corr))
  # Then impute NA values
  if (all(is.na(dataset_data$corr))) {
    dataset_data$corr_imputed <- NA
  } else {
    dataset_data$corr_imputed <- dataset_data$corr %>%
      Hmisc::impute(fun = "random") %>%
      as.numeric()
  }

  # Compute effect sizes and variances
  dataset_data_calc <- dataset_data %>%
    mutate(dataset = dataset_meta[["name"]],
           short_name = dataset_meta[["short_name"]],
           domain = dataset_meta[["domain"]]) %>%
    split(.$row) %>%
    map_df(~bind_cols(
      .x, compute_es(
        .x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1, .x$SD_2,
        .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d, .x$d_var, .x$corr,
        .x$corr_imputed, .x$r, .x$r_var, .x$study_ID, .x$expt_num,
        .x$special_cases_measures, .x$contrast_sampa, .x$short_name
      ))) %>%
    select(-row)

  # Add any other derived values
  method_options <- keep(fields, ~.x$field == "method")[[1]]$options
  method_names <- unlist(map(method_options, ~.x[[names(.x)]]$fullname))
  names(method_names) <- unlist(map(method_options, names))

  dataset_data_calc %>%
    # mutate(dataset = dataset_meta[["name"]],
    #        short_name = dataset_meta[["short_name"]],
    #        method = unlist(method_names[method])) %>%
    mutate(method = unlist(method_names[method])) %>%
    rowwise() %>%
    mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                    na.rm = TRUE),
           n = mean(c(n_1, n_2), na.rm = TRUE),
           same_infant_calc = paste(study_ID,same_infant)) %>%
    add_rownames("unique_row") %>%
    ungroup()

}




# Fetch a dataset from google sheets, run it through field validation,
# perform any necessary manipulations of its contents, save it to a file
load_dataset <- function(dataset_short_name) {

  if (dataset_short_name == '') {
    return("")
  }

  dataset_meta <- datasets %>% filter(short_name == dataset_short_name)
  if (!nrow(dataset_meta)) {
    return(sprintf("Dataset '%s' isn't in datasets metadata.\n", dataset_short_name))
  }

  dataset_contents <- fetch_dataset(dataset_meta)
  if (is.null(dataset_contents)) {
    return()
  }

  valid_dataset <- validate_dataset(dataset_meta, dataset_contents)
  if (!valid_dataset) {
    return(sprintf(
      "Dataset '%s' had one or more validation issues, please correct.\n",
      dataset_meta$name))
  } else {
    "All required columns present"
  }

}

validate_dataset_field <- function(dataset_name, dataset_contents, field) {
  if (field$required) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "options") {
        if (class(field$options) == "list") {
          options <- names(unlist(field$options, recursive = FALSE))
        } else {
          options <- field$options
        }
        invalid_values <- unique(dataset_contents[[field$field]]) %>%
          setdiff(options)
        if (!is.null(field$nullable) && field$nullable) {
          invalid_values <- na.omit(invalid_values)
        }
        if (length(invalid_values)) {
          ret <- ""
          for (value in invalid_values) {
            ret <- paste(ret, 
            sprintf("Dataset '%s' has invalid value '%s' for field '%s'.\n",
                        dataset_name, value, field$field))
          }
          return(ret)
        }
      } else if (field$type == "numeric") {
        field_contents <- dataset_contents[[field$field]]
        if (!(is.numeric(field_contents) || all(is.na(field_contents)))) {
          ret <- sprintf("Dataset '%s' has wrong type for numeric field '%s'.\n",
                      dataset_name, field$field)
          return(ret)
        }
      }
    } else {
      ret <- sprintf("Dataset '%s' is missing required field: '%s'.\n",
                  dataset_name, field$field)
      return(ret)
    }
  }
  return("OK")
}
get_property <- function(property, property_fun = function(x) x) {
    map_chr(fields, function(entry) {
      if (property %in% names(entry) && !is.null(entry[[property]]))
        property_fun(entry[[property]])
      else ""
    })
}

process_options <- function(options) {
  if (class(options) == "list") {
    opts <- names(unlist(options, recursive = FALSE))
  } else {
    opts <- options
  }
  paste(map_chr(opts, ~sprintf("<code>%s</code>", .x)), collapse = ", ")
}


make_fields <- function(fields) {
  
fields_data <- dplyr::data_frame(field = get_property("field"),
                          description = get_property("description"),
                          type = get_property("type"),
                          format = get_property("format"),
                          options = get_property("options", process_options),
                          required = get_property("required")) %>%
  tidyr::unite(`format/options`, format, options, sep = "") 
}

shinyServer(function(input, output, session) {
  output$result <- renderText(load_dataset(datasets %>% filter(name == input$dataset) %>% pull(short_name)))
  output$dataset_spec <- renderPrint(as.list(datasets %>% filter(name == input$dataset)))
  output$fields_spec <- renderDT(make_fields(fields))
  output$selected_name <- renderText(input$dataset)
})
