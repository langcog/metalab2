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
library(metalabr)

if (!exists("dataset_yaml")) {
  fields <- get_metalab_field_info()
  dataset_yaml <- get_metalab_dataset_info()
  metalab_data <- get_metalab_data(dataset_yaml)
  dataset_info <- add_metalab_summary_info(dataset_yaml, metalab_data)
}
