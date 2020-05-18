suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(langcog)
  library(here)
})

knitr::opts_chunk$set(cache = FALSE)

logOnError <- function(expression) {
  tryCatch(expression, error = function(e) { message(e) })
}

message(paste0("==> Start main_builder.R execution. ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")))

unlink(here("data"), recursive = TRUE)
dir.create(here("data"))

logOnError({
  domains <- yaml::yaml.load_file(here("metadata", "domains.yaml"))
})

logOnError({
  reportList <- yaml::yaml.load_file(here("metadata", "reports.yaml"))
})

logOnError({
  fields <- yaml::yaml.load_file(here("metadata", "spec.yaml"))
})

logOnError({
  fields_derived <- yaml::yaml.load_file(here("metadata", "spec_derived.yaml")) %>%
    transpose() %>%
    simplify_all() %>%
    dplyr::as_data_frame()
})

# creating datasets object structure (list of lists with metadata)
logOnError({
  datasets_file <- yaml::yaml.load_file(here("metadata", "datasets.yaml"))

  func <- function(x) {
    paste0(substr(x$domain, 1, 1), substr(x$name, 1, 1))
  }
  
  datasets_file <- datasets_file[order(sapply(datasets_file, func))] # sort
})

# some kind of conversion here?
logOnError({
  datasets <- datasets_file %>%
    map(function(row){
      row$moderators <- NULL
      row$subset <- NULL
      as.data.frame(row, stringsAsFactors = FALSE)
    })
})

# combine into proper data.frame, put back moderators and subset
logOnError({
  datasets <- do.call(rbind, datasets)
  moderators <- map(datasets_file, "moderators")
  subset <- map(datasets_file, "subset")
  datasets$moderators <- moderators
  datasets$subset <- subset
})

source(here("scripts", "cache_datasets.R"))

cached_data <- list.files(here("data"), pattern = "\\.csv$") %>% {
  substr(., 1, nchar(.) - 4)
}

load_dataset <- function(filename) {
  read.csv(
    file.path(here("data", paste0(filename, ".csv"))),
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

domainIDs <- map(domains, "id") %>% unlist()

## rendering setup
# creating temporary page directory:
dir.create(here("rendered"))
file.copy(here("pages/images"), here("rendered"), recursive = TRUE)

## rendering reports
reportList %>% map(
  ~ logOnError({
    rmarkdown::render(here("pages", "report.Rmd"),
                      output_file = paste0(.$id, "-outer.html"),
                      output_dir = here("rendered", "reports"),
                      params = list(report = .),
                      output_format = "html_document"
                      )
  })
)

# rendering domain pages:
seq_along(domains) %>% map(
  ~ logOnError({
    rmarkdown::render(here("pages", "domain-template.Rmd"),
                      output_file = paste0(domainIDs[[.]], ".html"),
                      output_dir = here("rendered", "domain"),
                      params = list(
                        dataset = filter(ungroup(datasets), domain == domainIDs[[.]]),
                        domainName = domains[[.]]$title),
                      output_format = "html_document")
  })
)

logOnError({
  dataset <- yaml::yaml.load_file(here("metadata", "datasets.yaml"))
  func <- function(x) paste0(substr(x$domain, 1, 1), substr(x$name, 1, 1))
  dataset <- dataset[order(sapply(dataset, func))] # sort
})

# rendering datasets pages:
dataset %>% map_if(
  ~ .$filename %in% cached_data,
  ~ logOnError({
    rmarkdown::render(here("pages", "dataset-template.Rmd"),
                      output_file = paste0(.$short_name, ".html"),
                      output_dir = here("rendered", "dataset"),
                      params = list(
                        datasetID = .$short_name),
                      output_format = "html_document")
  })
)

## serve the site and watch for changes to .Rmd files

## is this still needed with the changes to output_dirs in the
## metalab_serve function?
metalab_build <- function(input, output) {
  rmarkdown::render(input,
                    output_file = output,
                    output_dir = dirname(output),
                    output_format = "html_document")
}

metalab_serve <- function (dir, script = metalab_build,
                           method = "rmdv2", in_session = TRUE) {
  servr:::dynamic_site(dir, daemon = TRUE, build = function(message) {
    dirs <- grep("^[.].", list.dirs(), value = TRUE, invert = TRUE)
    input_dirs <- c(dirs, "./reports", "./tutorials", "./documentation")
    output_dirs <- c("../rendered", "../rendered/reports", "../rendered/tutorials", "../rendered/documentation")
    servr:::knit_maybe(input_dirs, output_dirs, script, method, in_session)
  }, site.dir = "../rendered")
}

metalab_serve(here::here("pages"))

message("==> Execution of main_builder done!")
