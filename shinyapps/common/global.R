library(shiny)
library(shinyBS)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(langcog)
library(feather)
library(plotly)

project_directory <- "/srv/shiny-server/common/" # path using new deploy script that copies data to the shiny server global directory
#project_directory <- "/home/metalab/metalab2/" # path using old deploy script
#project_directory <- "../../" # path for development purposes

logOnError <- function(expression) {
  tryCatch(expression, error = function(e) { message(e) })
}

logOnError({
  fields <- yaml::yaml.load_file(paste0(project_directory, "metadata/spec.yaml"))
})

logOnError({
  fields_derived <- yaml::yaml.load_file(paste0(project_directory, "metadata/spec_derived.yaml")) %>%
    transpose() %>%
    simplify_all() %>%
    dplyr::as_data_frame()
})



# creating datasets object structure:
logOnError({
  datasets_file <- yaml::yaml.load_file(paste0(project_directory, "metadata/datasets.yaml"))
})

logOnError({
  datasets <- datasets_file %>%
    map(function(row){
      row$moderators <- NULL
      as.data.frame(row, stringsAsFactors = FALSE)
    })
  datasets <- do.call(rbind, datasets)
  moderators <- map(datasets_file, "moderators")
  datasets$moderators <- moderators
})


logOnError({
  cached_data <- list.files(paste0(project_directory, "data/"), pattern = "\\.csv$") %>% {
    substr(., 1, nchar(.) - 4)
  }
})

load_dataset <- function(filename) {
  read.csv(
    file.path(paste0(project_directory, "data"), paste0(filename, ".csv")),
    stringsAsFactors = FALSE) %>%
    mutate(
      filename = filename,
      year = ifelse(
        test = grepl("submitted", study_ID),
        yes = Inf,
        no = stringr::str_extract(study_ID, "([:digit:]{4})"))
    ) %>%
    mutate(
      study_ID = as.character(study_ID),
      same_infant = as.character(same_infant))
}

avg_month <- 365.2425 / 12.0

logOnError({
  all_data <- cached_data %>%
    map_df(load_dataset) %>%
    mutate(all_mod = "",
           mean_age_months = mean_age / avg_month)

  all_data <- all_data %>%
    filter(!is.na(d_calc))
})

logOnError({
  studies <- all_data %>%
    group_by(dataset) %>%
    summarise(
      num_experiments = n(),
      num_papers = length(unique(study_ID)))
})

logOnError({
  subjects <- all_data %>%
    rowwise() %>%
    mutate(n_total = sum(c(n_1, n_2), na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(dataset, study_ID, same_infant, .keep_all = TRUE) %>%
    group_by(dataset) %>%
    summarise(num_subjects = sum(n_total))
})

logOnError({
  datasets <- datasets %>%
    rename(dataset = name) %>%
    left_join(studies, by = "dataset") %>%
    left_join(subjects, by = "dataset") %>%
    rename(name = dataset) %>%
    filter(filename %in% cached_data)
})



