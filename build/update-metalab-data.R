library(dplyr)
library(purrr)
library(here)
library(metalabr)
library(readr) # <- placed so that renv captures as dependency

## In this script, we choose to use the local versions of datasets.yaml
## and spec.yaml, since it may have been modified with, for example,
## an additional dataset or updated version
dataset_yaml <- get_metalab_metadata(here("metadata", "datasets.yaml"))
specs <- metalabr:::get_metalab_specs(here("metadata", "spec.yaml"))
metalab_data <- get_metalab_data(dataset_yaml, specs = specs)
dataset_info <- metalabr:::add_metalab_summary(dataset_yaml, metalab_data)

persist_metalab_data <- function(x, dataset_info) {
  filename <- dataset_info[dataset_info$short_name == unique(x$short_name),
                           "filename"]
  
  write.csv(x,
            file = here("shinyapps", "site_data", "csv", paste0(filename, ".csv")),
            row.names = FALSE, quote = TRUE)

}

## save all metalab data locally in CSV format: this will get checked
## into the git repository
lapply(
  split(metalab_data, metalab_data$short_name),
  function(x) {
    persist_metalab_data(x, dataset_info)
  })

## save all metalab data locally in Rdata format: this will get
## checked into the git repository
save(metalab_data, dataset_info, 
     file = here("shinyapps", "site_data", "Rdata", paste0("metalab", ".Rdata")),
     version = 3)
