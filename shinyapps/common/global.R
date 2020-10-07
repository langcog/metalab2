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

get_metavoice_data_shiny <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "csv")
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  ret_df <- do.call(rbind, data)
  ret_df$all_mod <- ""
  ret_df
}

fields <- get_metavoice_field_info()

fields_derived <- get_metavoice_derived_field_info()

metavoice_data <- get_metavoice_data_shiny(here("shinyapps", "site_data"))

dataset_yaml <- get_metavoice_dataset_info() #this goes to github, maybe
                                           #should be local too? how
                                           #is it used?
dataset_info <- add_metavoice_summary_info(dataset_yaml, metavoice_data)
