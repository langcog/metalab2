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

get_metalab_data_local <- function(directory) {
  files <- list.files(directory, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  do.call(rbind, data)
}

fields <- get_metalab_field_info()
metalab_data <- get_metalab_data_local(here("shinyapps", "site_data"))


