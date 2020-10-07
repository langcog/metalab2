library(dplyr)
library(purrr)
library(here)
library(metalabr)

load_cached_data(here("shinyapps", "site_data", "Rdata", "metalab.Rdata"))

## build dataset Rmd files from Rmd template, filling in each value of dataset.
dataset_template <- readLines(here("build", "dataset-template.Rmd"))
lapply(dataset_info$short_name, function(s_name) {
  current_dataset <- dataset_info %>% filter(short_name == s_name)
  to_write <- sapply(dataset_template, function(template_line) {
    template_line <- gsub("<<SHORT_NAME>>", current_dataset$short_name, template_line)
    template_line <- gsub("<<LONG_NAME>>", current_dataset$name, template_line)
    template_line <- gsub("<<SHORT_DESC>>", current_dataset$short_desc, template_line)
    template_line <- gsub("<<NUMERIC_SUMMARY>>",
                          paste(floor(current_dataset$num_papers), "papers |",
                                floor(current_dataset$num_experiments), "experiments |",
                                floor(current_dataset$num_subjects), "subjects") ,
                          template_line)
    gsub("<<DOMAIN_NAME>>", current_dataset$domain, template_line)
  })
  dir.create(here("content", "dataset", s_name))
  cat(to_write,
      file = here("content", "dataset", s_name, "index.Rmd"),
      sep = "\n")
  cat("copying file:", here("static", current_dataset$src))
  file.copy(from = here("static", current_dataset$src),
            to = here("content", "dataset", s_name, "featured.png"))
})
