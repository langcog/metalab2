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

load_cached_data(here("shinyapps", "site_data", "Rdata", "metalab.Rdata"))

fields <-
  get_metalab_field_info("https://raw.githubusercontent.com/langcog/metalab/main/metadata/spec.yaml")

fields_derived <-
  get_metalab_derived_field_info("https://raw.githubusercontent.com/langcog/metalab/main/metadata/spec_derived.yaml")
