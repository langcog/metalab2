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
library(here)
library(DT)
library(stringr)
library(metalabr)

get_metalab_data_shiny <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "csv")
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  ret_df <- do.call(rbind, data)
  ret_df$all_mod <- ""
  ret_df
}

fields <- get_metalab_field_info()

fields_derived <- get_metalab_derived_field_info()

metalab_data <- get_metalab_data_shiny(here("shinyapps", "site_data", "csv"))
dataset_yaml <- get_metalab_dataset_info(here("metadata", "datasets-tests.yaml")) 
                                           #should be local too? how
                                           #is it used?
dataset_info <- add_metalab_summary_info(dataset_yaml, metalab_data)
