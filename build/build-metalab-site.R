library(dplyr)
library(purrr)
library(here)
library(metalabr)

domains <- get_metalab_domain_info()
reports <- get_metalab_report_info()
dataset_yaml <- get_metalab_dataset_info()
metalab_data <- get_metalab_data(dataset_yaml)
dataset_info <- add_metalab_summary_info(dataset_yaml, metalab_data)

persist_metalab_data <- function(x, dataset_info) {
  filename <- dataset_info[dataset_info$short_name == unique(x$short_name),
                           "filename"]
  
  write.csv(x,
            file = here("shinyapps", "site_data", "csv", paste0(filename, ".csv")),
            row.names = FALSE, quote = TRUE)

}

## save all metalab data locally in CSV format
lapply(
  split(metalab_data, metalab_data$short_name),
  function(x) {
    persist_metalab_data(x, dataset_info)
  })

## save all metalab data locally in Rdata format
save(metalab_data, dataset_info, domains, reports,
     file = here("shinyapps", "site_data", "Rdata", paste0("metalab", ".Rdata")))


## build dataset Rmd files from Rmd template, filling in each value of dataset.

## read template
dataset_template <- readLines(here("build", "dataset-template.Rmd"))

lapply(dataset_info$short_name, function(s_name) {
  to_write <- sapply(dataset_template, function(template_line) {
    current_dataset <- dataset_info %>% filter(short_name == s_name)
    template_line <- gsub("<<SHORT_NAME>>", current_dataset$short_name, template_line)
    template_line <- gsub("<<LONG_NAME>>", current_dataset$name, template_line)
    gsub("<<DOMAIN_NAME>>", current_dataset$domain, template_line)
  })
  cat(to_write,
      file = here("content", "dataset", paste0(s_name, ".Rmd")),
      sep = "\n")
})
