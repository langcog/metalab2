#ml_url
mv_metadata_url <- "https://raw.githubusercontent.com/LNJ-ND/MetaVoice_Website/master/metadata/"

#get_metalab_data
get_metavoice_data <- function(dataset_info, short_names, domains) {
  if (!missing(short_names) && !missing(domains)) {
    stop("Only provide one of short_names or domains")
  }

  if (!missing(short_names)) {
    dataset_info <- dataset_info %>% filter(short_name %in% short_names)
  }

  if (!missing(domains)) {
    dataset_info <- dataset_info %>% filter(domain %in% domains)
  }

  dataset_info %>%
    purrr::pmap_dfr(function(...) {
      mv_load_and_validate_dataset(list(...))
    })
}

#load_and_validate_dataset
mv_load_and_validate_dataset <- function(dataset_info) {
  cat("Getting raw MetaVoice data from Google Sheets for dataset:", dataset_info$name, "\n")
  dataset_contents <- mv_fetch_dataset(dataset_info$key)
  field_info <- get_metavoice_field_info()

  if (is.null(dataset_contents)) {
    return()
  }

  is_valid_dataset <- mv_is_valid_dataset(dataset_info,
                                       dataset_contents,
                                       field_info)

  if (!is_valid_dataset) {
    return()
  }

  #avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  mv_tidy_dataset(dataset_info, dataset_contents, field_info) %>%
    mutate(all_mod = "") %>%
           #mean_age_months = mean_age / avg_month) %>%
     filter(!is.na(d_calc)) %>%
     mutate(
       #year = ifelse(
         # test = grepl("submitted", study_ID),
         # yes = Inf,
         # no = stringr::str_extract(study_ID, "([:digit:]{4})")),
       study_ID = as.character(study_ID),
       same_sample = as.character(same_sample),
       expt_unique = as.character(expt_unique), #factor?
       expt_condition = as.character(expt_condition))
}

#fetch_dataset
mv_fetch_dataset <- function(key) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    key, key)

  tryCatch({
    suppressMessages({
      dataset_url %>%
        httr::GET() %>%
        httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
    })
  },
  error = function(e) {
    cat(sprintf("Can't load dataset with key '%s'. Exception: %s.\n", key, e))
  })
}

#get_metalab_field_info
get_metavoice_field_info <- function(field_file = paste0(mv_metadata_url, "spec.yaml")) {
  yaml::yaml.load_file(field_file)
}

#is_valid_dataset
mv_is_valid_dataset <- function(dataset_meta, dataset_contents, field_info) {
  valid_fields <- mv_validate_dataset(dataset_meta, dataset_contents, field_info)
  all(unlist(valid_fields))
}

#validate_dataset
mv_validate_dataset <- function(dataset_meta, dataset_contents, field_info) {
  purrr::map(field_info, function(field) {
    mv_validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
}

#validate_dataset_field
mv_validate_dataset_field <- function(dataset_name, dataset_contents, field) {
  if (field$required) {
    if (!mv_is_valid_required_field(dataset_name, dataset_contents, field)) {
      return(FALSE)
    }

    if (field$field == "short_cite"){
      if(!mv_is_valid_length(dataset_name, dataset_contents, field, 60)){
        return(FALSE)
      }
    }
    if (field$type == "options") {
      if (!mv_is_valid_options_field(dataset_name, dataset_contents, field)) {
        return(FALSE)
      }
    } else if (field$type == "numeric") {
      if (!mv_is_valid_numeric_field(dataset_name, dataset_contents, field)) {
        return(FALSE)
      }
      # if (field$field == "r" || field$field == "corr"){
      #   if(!mv_is_valid_r_corr(dataset_name, dataset_contents, field)){
      #     return(FALSE)
      #   }
      # }

    }
  }
  return(TRUE)
}

#is_valid_required_field
mv_is_valid_required_field <- function(dataset_name, dataset_contents, field) {
  if (!field$field %in% names(dataset_contents)) {
    cat(sprintf("Dataset '%s' is missing required field: '%s'.\n",
                dataset_name, field$field))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#is_valid_length
mv_is_valid_length <- function(dataset_name, dataset_contents, field, length_limit){
  field_contents <- dataset_contents[[field$field]]
  if ((any(nchar(field_contents) > length_limit))){
    cat(sprintf("Dataset %s has length greater than limit (%s characters) in '%s' \n",
                dataset_name, length_limit, field$field))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#is_valid_options_field
mv_is_valid_options_field <- function(dataset_name, dataset_contents, field) {
  if (class(field$options) == "list") {
    options <- names(unlist(field$options, recursive = FALSE))
  } else {
    options <- field$options
  }

  invalid_values <- unique(dataset_contents[[field$field]]) %>%
    setdiff(options)

  if (!is.null(field$nullable) && field$nullable) {
    invalid_values <- na.omit(invalid_values)
  }

  if (length(invalid_values)) {
    for (value in invalid_values) {
      cat(sprintf("Dataset '%s' has invalid value '%s' for field '%s'.\n",
                  dataset_name, value, field$field))
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#is_valid_numeric_field
mv_is_valid_numeric_field <- function(dataset_name, dataset_contents, field) {
  field_contents <- dataset_contents[[field$field]]
  if (!(is.numeric(field_contents) || all(is.na(field_contents)))) {
    cat(sprintf("Dataset '%s' has wrong type for numeric field '%s'.\n",
                dataset_name, field$field))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#is_valid_r_corr
# is_valid_r_corr <- function(dataset_name, dataset_contents, field){
#   field_contents <- dataset_contents[[field$field]]
#   if ((any(field_contents > 1, na.rm = TRUE)) || any(field_contents < -1, na.rm = TRUE)){
#     rows <- which(((field_contents > 1 | field_contents < -1))) + 1
#     cat(sprintf("Dataset '%s' has '%s' out of range [-1,1] on row %s \n",
#                 dataset_name, field$field, rows))
#     return(FALSE)
#   } else {
#     return(TRUE)
#   }
# }

#tidy_dataset
mv_tidy_dataset <- function(dataset_meta, dataset_contents, field_info) {

  # Coerce each field's values to the field's type, discard any columns not in
  # field spec, add NA columns for missing (optional) fields
  dataset_data <- data_frame(row = 1:nrow(dataset_contents))
  for (field in field_info) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "string") {
        field_fun <- as.character
      } else if (field$type == "numeric") {
        field_fun <- as.numeric
      } else {
        field_fun <- function(x) x
      }
      dataset_data[,field$field] <- field_fun(dataset_contents[[field$field]])
    } else {
      dataset_data[,field$field] <- NA
    }
  }

  # Impute values for missing correlations
  set.seed(111)
  # First we replace corr values outside the range (.01,.99) with NA
  # dataset_data = dataset_data %>%
  #   mutate(corr = abs(corr)) %>%
  #   mutate(corr = ifelse(corr > .99 | corr < .01, NA, corr))
  # # Then impute NA values
  # if (all(is.na(dataset_data$corr))) {
  #   dataset_data$corr_imputed <- NA
  # } else {
  #   dataset_data$corr_imputed <- dataset_data$corr %>%
  #     Hmisc::impute(fun = "random") %>%
  #     as.numeric()
  # }

  # Compute effect sizes and variances
  dataset_data_calc <- dataset_data %>%
    mutate(dataset = dataset_meta[["name"]],
           short_name = dataset_meta[["short_name"]],
           domain = dataset_meta[["domain"]]) %>%
    split(.$row) %>%
    purrr::map_df(~bind_cols(
      .x, mv_compute_es( #make comment out other stuff we don't have?
        .x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1, .x$SD_2,
        .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d, .x$d_var, .x$corr,
        .x$corr_imputed, .x$r, .x$r_var, .x$study_ID, .x$expt_num,
        .x$special_cases_measures, .x$contrast_sampa, .x$short_name
      ))) %>%
    select(-row)

  # Add any other derived values
  task_type_options <- purrr::keep(field_info, ~.x$field == "task_type")[[1]]$options
  task_type_names <- unlist(purrr::map(task_type_options, ~.x[[names(.x)]]$fullname)) #ullname?
  names(task_type_names) <- unlist(purrr::map(task_type_options, names))

  dataset_data_calc %>%
    # mutate(dataset = dataset_meta[["name"]],
    #        short_name = dataset_meta[["short_name"]],
    #        method = unlist(method_names[method])) %>%
    mutate(task_type = unlist(task_type_names[task_type])) %>%
    rowwise() %>%
    mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                    na.rm = TRUE),
           n = mean(c(n_1, n_2), na.rm = TRUE),
           same_sample_calc = paste(study_ID,same_sample)) %>%
    add_rownames("unique_row") %>%
    ungroup()
}

#compute_es
mv_compute_es <- function(participant_design, x_1 = NA, x_2 = NA, x_dif = NA,
                       SD_1 = NA, SD_2 = NA, SD_dif = NA, n_1 = NA, n_2 = NA,
                       t = NA, f = NA, d = NA, d_var = NA, corr = NA,
                       corr_imputed = NA, r = NA, r_var = NA, study_ID = NA, expt_num = NA,
                       special_cases_measures = NA, contrast_sampa = NA, short_name = NA) {

  assertthat::assert_that(participant_design %in% c("between", "within_two", "within_one"))

  #we introduce variables calles d_calc and d_var_calc to distiguish them from the fields d and d_var, which are fields where effect sizes were already available from the source of the data
  d_calc <- NA
  d_var_calc <- NA
  es_method <- "missing"

  #start of decision tree where effect sizes are calculated differently based on participant design
  #depending on which data is available, effect sizes are calculated differently
  if (participant_design == "between") {
    es_method  <- "between"
    #effect size calculation
    if (mv_complete(x_1, x_2, SD_1, SD_2)) {
      pooled_SD <- sqrt(((n_1 - 1) * SD_1 ^ 2 + (n_2 - 1) * SD_2 ^ 2) / (n_1 + n_2 - 2)) # Lipsey & Wilson, 3.14
      d_calc <- (x_1 - x_2) / pooled_SD # Lipsey & Wilson (2001)
    } else if (mv_complete(t)) {
      d_calc <- t * sqrt((n_1 + n_2) / (n_1 * n_2)) # Lipsey & Wilson, (2001)
    } else if (mv_complete(f)) {
      d_calc <- sqrt(f * (n_1 + n_2) / (n_1 * n_2)) # Lipsey & Wilson, (2001)
    }
    if (mv_complete(n_1, n_2, d_calc)) {
      #now that effect size are calculated, effect size variance is calculated
      d_var_calc <- ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / (2 * (n_1 + n_2)))
    } else if (mv_complete(r, r_var)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else if (mv_complete(d, d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
    }

  } else if (participant_design == "within_two") {
    if (is.na(corr) & mv_complete(x_1, x_2, SD_1, SD_2, t)) {
      # Use raw means, SD, and t-values to calculate correlations
      corr = (SD_1^2 + SD_2^2 - (n_1 * (x_1 - x_2)^2 / t^2)) / (2 * SD_1 * SD_2)
    }
    if (is.na(corr) | corr > .99 | corr < .01){
      #if correlation between two measures is not reported, use an imputed correlation value
      #we also account for the observation that some re-calculated values are impossible and replace those
      corr <- corr_imputed
    }
    # Old version of imputing correlations, replaced by Christina & Sho 2018-06-01
    #if (is.na(corr)) {
    #  #if correlation between two measures is not reported, use an imputed correlation value
    #  corr <- corr_imputed
    #}
    #effect size calculation
    if (mv_complete(x_1, x_2, SD_1, SD_2)) {
      pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- (x_1 - x_2) / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (mv_complete(x_1, x_2, SD_dif)) {
      within_SD <- SD_dif / sqrt(2 * (1 - corr)) # Lipsey & Wilson (2001); Morris & DeShon (2002)
      d_calc <- (x_1 - x_2) / within_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (mv_complete(x_dif, SD_1, SD_2)) {
      pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- x_dif / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "subj_diff_two"
    } else if (mv_complete(x_dif, SD_dif)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- (x_dif / SD_dif) * wc #Morris & DeShon (2002)
      es_method  <- "subj_diff_two"
    } else if (mv_complete(t)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- (t / sqrt(n_1)) * wc #Dunlap et al., 1996, p.171
      es_method  <- "t_two"
    } else if (mv_complete(f)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- sqrt(f / n_1) * wc
      es_method  <- "f_two"
    }
    if (mv_complete(n_1, d_calc)) {
      #now that effect size are calculated, effect size variance is calculated
      #d_var_calc <- ((1 / n_1) + (d_calc ^ 2 / (2 * n_1))) * 2 * (1 - corr) #we used this until 4/7/17
      d_var_calc <- (2 * (1 - corr)/ n_1) + (d_calc ^ 2 / (2 * n_1)) # Lipsey & Wilson (2001)
    } else  if (mv_complete(r)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
      es_method  <- "r_two"
    } else if (mv_complete(d, d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
      es_method  <- "d_two"
    }

  } else if (participant_design == "within_one") {
    if (mv_complete(x_1, x_2, SD_1)) {
      d_calc <- (x_1 - x_2) / SD_1
      es_method  <- "group_means_one"
    } else if (mv_complete(t)) {
      d_calc <- t / sqrt(n_1)
      es_method  <- "t_one"
    } else if (mv_complete(f)) {
      d_calc <- sqrt(f / n_1)
      es_method  <- "f_one"
    }
    if (mv_complete(n_1, d_calc)) {
      #d_var_calc <- (2/n_1) + (d_calc ^ 2 / (4 * n_1)) #we used this until 4/7/2017
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) #this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)

    } else if (mv_complete(r, n_1)){
      # this deals with pointing and vocabulary
      # Get variance of transformed r (z; fisher's tranformation)
      SE_z = 1 / sqrt(n_1 - 3) # from Howell (2010; "Statistical methods for Psychology", pg 275)
      var_z = SE_z ^ 2

      # Transform z variance to r variance
      var_r = tanh(var_z)  # from wikipedia (https://en.wikipedia.org/wiki/Fisher_transformation) for convert z -> r, consistent with Howell

      # Transform r to d
      d_calc = 2 * r / (sqrt(1 - r ^ 2)) # from (Hunter and Schmidt, pg. 279)
      d_var_calc = (4 * var_r)/(1 - r ^ 2) ^ 3 # from https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf (pg. 4)

      es_method  <- "r_one"

    } else if (mv_complete(r)) {
      #if r instead of d is reported, transform for standardization
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)

      es_method  <- "r_one"

    } else  if (mv_complete(d)) { # MLL added d_var_calc from data
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) # this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)
      es_method  <- "d_one"
    }
  }

  df <- if (participant_design == "between") {
    sum(n_1, n_2, na.rm = TRUE) - 2
  } else {
    n_1 - 1
  }
  J <- 1 - 3 / (4 * (df - 1))
  g_calc <- d_calc * J
  g_var_calc <- J ^ 2 * d_var_calc

  if (participant_design == "between") {
    a <- (sum(n_1, n_2, na.rm = TRUE) ^ 2) / prod(n_1, n_2, na.rm = TRUE)
  } else {
    a <- 4
  }
  r_calc <- d_calc / sqrt(d_calc ^ 2 + a)
  r_var_calc <- a ^ 2 * d_var_calc / (d_calc ^ 2 + a) ^ 3

  z_calc <- 0.5 * log((1 + r_calc) / (1 - r_calc))
  z_var_calc = 1 / (n_1 - 3)

  log_odds_calc <- d_calc * pi / sqrt(3)
  log_odds_var_calc <- d_var_calc * pi ^ 2 / 3


  return(data_frame("d_calc" = d_calc, "d_var_calc" = d_var_calc,
                    "g_calc" = g_calc, "g_var_calc" = g_var_calc,
                    "r_calc" = r_calc, "r_var_calc" = r_var_calc,
                    "z_calc" = z_calc, "z_var_calc" = z_var_calc,
                    "log_odds_calc" = log_odds_calc,
                    "log_odds_var_calc" = log_odds_var_calc,
                    "es_method" = es_method))

}

#complete
mv_complete <- function(...) {
  args <- list(...)
  !any(unlist(purrr::map(args, ~(is.null(.x) || is.na(.x)))))
}

#get_metalab_domain_info
get_metavoice_domain_info <- function(domain_file = paste0(mv_metadata_url, "domains.yaml")) {
  yaml::yaml.load_file(domain_file)
}

#get_metalab_report_info
get_metavoice_report_info <- function(report_file = paste0(mv_metadata_url, "reports.yaml")) {
  yaml::yaml.load_file(report_file)
}

#get_metalab_derived_field_info
get_metavoice_derived_field_info <- function(derived_field_file = paste0(mv_metadata_url, "spec_derived.yaml")) {
  yaml::yaml.load_file(derived_field_file) %>%
    transpose() %>%
    simplify_all() %>%
    dplyr::as_data_frame()
}

#get_metalab_dataset_info
get_metavoice_dataset_info <- function(dataset_file = paste0(mv_metadata_url, "datasets.yaml")) {
  datasets <- yaml::yaml.load_file(dataset_file)

  datasets <- datasets %>% purrr::map(function(x) {
    x$moderators <- list(x$moderators)
    x$features <- list(x$features)
    #x$subset <- list(x$subset)
    #x$reliability <- as.logical(x$reliability)
    x
  })

  bind_rows(datasets)
}

#add_metalab_summary_info
add_metavoice_summary_info <- function(metavoice_dataset_info, metavoice_data) {
  studies <- metavoice_data %>%
    group_by(dataset) %>%
    summarise(
      num_experiments = length(unique(expt_unique)),#n(), #maybe check if it looks like a reasonable number - does it count rows?
      num_papers = length(unique(study_ID)), .groups = "drop_last")

  subjects <- metavoice_data %>%
    distinct(dataset, same_sample, .keep_all = TRUE) %>% #add/remove study_ID?
    group_by(dataset) %>%
    summarise(num_subjects = sum(n_1, n_2, na.rm = TRUE), .groups = "drop_last")

  metavoice_dataset_info %>%
    rename(dataset = name) %>%
    left_join(studies, by = "dataset") %>%
    left_join(subjects, by = "dataset") %>%
    rename(name = dataset)
}

#save_dataset
mv_save_dataset <- function(dataset_meta, dataset_data) {
  write.csv(dataset_data, here("data", paste0(dataset_meta$filename, ".csv")), row.names = FALSE)
  cat(sprintf("Dataset '%s' saved successfully.\n", dataset_meta$name))
}

#load_cached_dataset
mv_load_cached_dataset <- function(filename) {
  read.csv(
    file.path(here("data", paste0(filename, ".csv"))),
    stringsAsFactors = FALSE) %>%
    mutate(
      filename = filename,
      # year = ifelse(
      #   test = grepl("submitted", study_ID),
      #   yes = Inf,
      #   no = stringr::str_extract(study_ID, "([:digit:]{4})"))
    ) %>%
    mutate(
      study_ID = as.character(study_ID),
      same_sample = as.character(same_sample),
      expt_unique = as.character(expt_unique), #factor?
      expt_condition = as.character(expt_condition))
}



