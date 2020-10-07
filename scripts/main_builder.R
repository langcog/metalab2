library(dplyr)
library(purrr)
library(here)
library(metalabr)

source("mv_functions.R")
knitr::opts_chunk$set(cache = FALSE)

domains <- get_metavoice_domain_info()
reports <- get_metavoice_report_info()
dataset_yaml <- get_metavoice_dataset_info()
metavoice_data <- get_metavoice_data(dataset_yaml)

dataset_info <- add_metavoice_summary_info(dataset_yaml, metavoice_data)

persist_metavoice_data <- function(x, dataset_info) {
  filename <- dataset_info[dataset_info$short_name == unique(x$short_name),
                           "filename"]

  write.csv(x,
            file = here("shinyapps", "site_data", paste0(filename, ".csv")),
            row.names = FALSE, quote = TRUE)
}

lapply(
  split(metavoice_data, metavoice_data$short_name),
  function(x) {
    persist_metavoice_data(x, dataset_info)
  })

##### GOT UNTIL HERE ##### CHANGE THE REST

## 'rendered' directory setup
render_dir <- here("rendered")
if (!dir.exists(render_dir)) {
  dir.create(render_dir)
}
file.copy(here("pages/images"), here("rendered"), recursive = TRUE)

## render reports
reports %>% purrr::map(
  ~ rmarkdown::render(here("pages", "report.Rmd"),
                      output_file = paste0(.$id, "-outer.html"),
                      output_dir = here("rendered", "reports"),
                      params = list(report = .),
                      output_format = "html_document"
                      )
)

# render domain pages:
domain_ids <- purrr::map(domains, "id") %>% unlist()

seq_along(domains) %>% purrr::map(
  ~ rmarkdown::render(here("pages", "domain-template.Rmd"),
                      output_file = paste0(domain_ids[[.]], ".html"),
                      output_dir = here("rendered", "domain"),
                      params = list(
                        dataset = filter(ungroup(dataset_info), domain == domain_ids[[.]]),
                        domain_name = domains[[.]]$title),
                      output_format = "html_document")
)


render_dataset <- function(dataset_info) {
  if (dataset_info$short_name %in% metalab_data$short_name) {
    rmarkdown::render(here("pages", "dataset-template.Rmd"),
                      output_file = paste0(dataset_info$short_name, ".html"),
                      output_dir = here("rendered", "dataset"),
                      params = list(
                        dataset_info = dataset_info,
                        dataset_raw = metalab_data %>%
                          filter(short_name == dataset_info$short_name)),
                      output_format = "html_document")

  }
}


# render dataset pages:
## tryign to use pmap_dfr for its side-effects , call a function one
## row per input data.frame. end up just returning an empty
## data.frame, how to refactor that?

dataset_info %>% purrr::pmap(function(...) {
  render_dataset(list(...))
})

## functions to help build and serve the site for local development
metalab_build <- function(input, output) {
  rmarkdown::render(input,
                    output_file = output,
                    output_dir = dirname(output),
                    output_format = "html_document")
}

metalab_serve_local <- function (dir, script = metalab_build,
                                 method = "rmdv2", in_session = TRUE) {

  servr:::dynamic_site(dir, daemon = TRUE, build = function(message) {
    dirs <- grep("^[.].", list.dirs(), value = TRUE, invert = TRUE)

    input_dirs <- c(dirs, "./reports")
    output_dirs <- c("../rendered", "../rendered/reports")

    servr:::knit_maybe(input_dirs, output_dirs, script, method, in_session)
  }, site.dir = "../rendered")
}

metalab_serve_local(here::here("pages"))
