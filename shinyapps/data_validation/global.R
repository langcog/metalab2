library(here)
source(here("shinyapps", "common", "global.R"))


get_google_sheet_id <- function(url) {
  str_match(url, "/spreadsheets/d/([a-zA-Z0-9-_]+)")[2]
}


check_key <- function(dataset_meta) {
  if (dataset_meta$key == "") {
    return("\U274C Cannot load dataset, key missing.\n")
  } else {
    return("\U2705 OK!")
  }
}

fetch_dataset <- function(dataset_meta) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  result <- tryCatch({
    dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    return ("error")
  })

  if ("string" %in% class(result)) {
    return("\U274C Cannot load dataset from Google Sheets")
  } else {
    return("\U2705 OK!")
  }
}

get_data <- function(dataset_meta) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  tryCatch({
    ret <- dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    sprintf("\U274C Cannot load dataset '%s' with key '%s'. Exception: %s.\n", dataset_meta$name,
            dataset_meta$key, e)
  })

  ret
}

check_data_exists <- function(dataset_short_name) {
  dataset_meta <- dataset_info %>% filter(dataset_info$short_name == dataset_short_name)
  if (!nrow(dataset_meta)) {
    return(sprintf("\U274C Dataset is not listed in dataset_info metadata")) 
  } else {
    return(sprintf("\U2705 OK!", dataset_meta$name))
  }
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
