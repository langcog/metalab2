suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(langcog)
})

knitr::opts_chunk$set(cache = FALSE)

logOnError <- function(expression) {
  tryCatch(expression, error = function(e) { message(e) })
}

message(paste0("==> Start main_builder.R execution. ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")))

setwd("..")

unlink("data/", recursive = TRUE)
dir.create("data")

logOnError({
  domains <- yaml::yaml.load_file("metadata/domains.yaml")
  })

logOnError({
  dataset <- yaml::yaml.load_file("metadata/datasets.yaml")
  func <- function(x) paste0(substr(x$domain, 1, 1), substr(x$name, 1, 1))
  dataset <- dataset[order(sapply(dataset, func))] # sort
  })

logOnError({
  reportList <- yaml::yaml.load_file("metadata/reports.yaml")
  })

logOnError({
  shinyapps <-  yaml::yaml.load_file("metadata/shinyapps.yaml")
  })



### datasets verification and preparing data metrics (code mostly come from global.R script)

logOnError({
  fields <- yaml::yaml.load_file("metadata/spec.yaml")
  })

logOnError({
  fields_derived <- yaml::yaml.load_file("metadata/spec_derived.yaml") %>%
    transpose() %>%
    simplify_all() %>%
    dplyr::as_data_frame()
  })


# creating datasets object structure:
logOnError({
  datasets_file <- yaml::yaml.load_file("metadata/datasets.yaml")
  func <- function(x) paste0(substr(x$domain, 1, 1), substr(x$name, 1, 1))
  datasets_file <- datasets_file[order(sapply(datasets_file, func))] # sort
  })

logOnError({
  datasets <- datasets_file %>%
    map(function(row){
        row$moderators <- NULL
        as.data.frame(row, stringsAsFactors = FALSE)
        })
  })

logOnError({
  datasets <- do.call(rbind, datasets)
  moderators <- map(datasets_file, "moderators")
  datasets$moderators <- moderators
  })

source("scripts/cache_datasets.R")

cached_data <- list.files("data/", pattern = "\\.csv$") %>% {
  substr(., 1, nchar(.) - 4)
  }

load_dataset <- function(filename) {
  read.csv(
    file.path("data", paste0(filename, ".csv")),
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
    mutate(
      all_mod = "",
      mean_age_months = mean_age / avg_month) %>%
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


###
# Rendering HTML page

# creating temporary page directory:
dir.create("rendered")

# initiate final realtive state to images
file.copy("images/", "rendered/", recursive = TRUE)

# extracting domain ids
domainIDs <- map(domains, "id") %>% unlist()

# rendering landing page:
logOnError({
  rmarkdown::render(
    "pages/index.Rmd",
    output_file = "index.html",
    output_dir = "rendered/",
    output_format = "html_document")
  })

# rendering domain pages:
seq_along(domains) %>% map(
  ~ logOnError({
    rmarkdown::render("pages/domain-template.Rmd",
     output_file = paste0(domainIDs[[.]], ".html"),
     output_dir = "rendered/domain/",
     params = list(
       dataset = filter(ungroup(datasets), domain == domainIDs[[.]]),
       domainName = domains[[.]]$title),
     output_format = "html_document")
    })
)

# rendering datasets pages:
dataset %>% map_if(
  ~ .$filename %in% cached_data,
  ~ logOnError({
    rmarkdown::render("pages/dataset-template.Rmd",
      output_file = paste0(.$short_name, ".html"),
      output_dir = "rendered/dataset/",
      params = list(
        datasetID = .$short_name),
      output_format = "html_document")
    })
  )

# rendering analyses page:
logOnError({
  rmarkdown::render(
    "pages/analyses.Rmd",
    output_file = "analyses.html",
    output_dir = "rendered/",
    output_format = "html_document")
  })


# rendering app page:
logOnError({
  rmarkdown::render(
    "pages/app.Rmd",
    output_file = "app.html",
    output_dir = "rendered/",
    output_format = "html_document")
  })


# rendering standalone report pages
reportList %>% map(
  ~ logOnError({
    rmarkdown::render(paste0("reports/", .$id, ".Rmd"),
      output_file = paste0(.$id, "_inner.html"),
      output_dir = "rendered/reports/",
      params = list(report = .),
      output_format = "html_document"
      )
    })
)

# rendering final reports pages
reportList %>% map(
  ~ logOnError({
    rmarkdown::render("pages/report.Rmd",
      output_file = paste0(.$id, ".html"),
      output_dir = "rendered/reports/",
      params = list(report = .),
      output_format = "html_document"
      )
    })
)

# rendering documentation page
logOnError({
  rmarkdown::render(
    "pages/documentation.Rmd",
    output_file = "documentation.html",
    output_dir = "rendered/",
    output_format = "html_document")
  })

# rendering tutorials page
logOnError({
rmarkdown::render(
  "pages/tutorials.Rmd",
  output_file = "tutorials.html",
  output_dir = "rendered/",
  output_format = "html_document")
  })

# rendering publications page
logOnError({
rmarkdown::render(
  "pages/publications.Rmd",
  output_file = "publications.html",
  output_dir = "rendered/",
  output_format = "html_document")
  })

# rendering about page
logOnError({
rmarkdown::render(
  "pages/about.Rmd",
  output_file = "about.html",
  output_dir = "rendered/",
  output_format = "html_document")
  })

message("==> Execution of main_builder done!")

