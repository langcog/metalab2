################################################################################
## CONSTANTS
alpha <- .05
CRIT_95 <- 1.96
CRIT_99 <- 2.58

shinyServer(function(input, output, session) {

  ##############################################################################
  # MODELS AND REACTIVES

  ########### DATA ###########

  categorical_mods <- reactive({
    if (is.null(input$moderators)) {
      return(NULL)
    }
    keep(input$moderators, function(mod) {
      # assumes all derived fields are non-categorical, which may change?
      !(mod %in% fields_derived$field) &&
        keep(fields, ~.x$field == mod)[[1]]$type %in% c("string", "options")
    })
  })

  mod_group <- reactive({
    if (length(categorical_mods())) {
      paste(categorical_mods(), collapse = "_")
    } else {
      "all_mod"
    }
  })

  combine_mods <- function(df, cols) {
    if (mod_group() != "all_mod" && length(cols) > 1) {
      df[[mod_group()]] <- do.call(paste, c(map(cols, ~df[[.x]]), sep = "\n"))
    }
    df
  }

  subsets <- reactive({
    req(input$dataset_name)
    datasets %>%
      filter(name == input$dataset_name) %>%
      .$subset %>%
      unlist()
  })

  dataset_names <- reactive({
    req(input$domain)
    datasets %>%
      filter(domain == input$domain) %>%
      pull(name)
  })

  data <- reactive({
    req(input$dataset_name)
    # TODO also filter by subset, when available
    all_data %>% filter(dataset == input$dataset_name, mean_age < 3000)
  })

  mod_data <- reactive({
    dots <- if (is.null(input$moderators)) {
      NULL
    } else {
      sprintf("!is.na(%s)", input$moderators)
    }
    data() %>%
      filter_(.dots = dots) %>%
      combine_mods(categorical_mods())
  })

  table_data <- reactive({
    all_data %>%
      filter(dataset == input$table_dataset_name) %>%
      select(-long_cite, -dataset, -short_name, -filename, -all_mod)
  })

  ########### MODELS ###########

  es <- reactive({
    sprintf("%s_calc", input$es_type)
  })

  es_var <- reactive({
    sprintf("%s_var_calc", input$es_type)
  })

  model <- reactive({
    if (length(input$moderators) == 0) {
      no_mod_model()
    } else {
      mods <- paste(input$moderators, collapse = "+")
      rma_formula <- as.formula(sprintf("%s ~ %s", es(), mods))
      if (input$ma_method == "REML_mv") {
        metafor::rma.mv(rma_formula, V = mod_data()[[es_var()]],
                        random = ~ 1 | short_cite,
                        slab = short_cite, data = mod_data(),
                        method = "REML")
      } else {
        metafor::rma(rma_formula, vi = mod_data()[[es_var()]],
                     slab = short_cite, data = mod_data(),
                     method = input$ma_method)
      }
    }
  })

  no_mod_model <- reactive({
    if (input$ma_method == "REML_mv") {
      metafor::rma.mv(yi = data()[[es()]], V = data()[[es_var()]],
                      random = ~ 1 | data()[["short_cite"]],
                      slab = data()[["short_cite"]],
                      method = "REML")
    } else {
      metafor::rma(yi = data()[[es()]], vi = data()[[es_var()]],
                   slab = data()[["short_cite"]],
                   method = input$ma_method)

    }
  })

  ########### UI ELEMENTS ###########

  display_name <- function(fields) {
    sp <- gsub("_", " ", fields)
    paste0(toupper(substring(sp, 1, 1)), substring(sp, 2))
  }

  output$dataset_name <- renderUI({
    selectInput(inputId = "dataset_name",
                   label = "Dataset",
                   choices = dataset_names()
    )
  })

  output$domain_selector <- renderUI({
    selectInput(inputId = "domain",
                label = "Domain",
                choices = datasets$domain %>%
                  unique %>%
                  set_names(display_name(.))
    )
  })

  output$link_to_dataset <- renderUI({
    req(input$dataset_name)
    base_url <- "https://langcog.github.io/metalab2/dataset/"
    short_name <- datasets %>%
      filter(name == input$dataset_name) %>%
      select(short_name)
    HTML(paste0("<i class=\"text-muted\">For more information see
                <a href='https://langcog.github.io/metalab2/documentation.html#datasets' target='_blank'>
                Documentation</a> or <a href='", base_url, short_name, ".html', target='_blank'>
                View raw dataset</a></i>"))
  })

  output$moderator_input <- renderUI({
    req(input$dataset_name)
    custom_mods <- datasets %>%
      filter(name == input$dataset_name) %>%
      .$moderators %>%
      unlist()
    mod_choices <- c("mean_age", "response_mode", "exposure_phase", custom_mods)
    valid_mod_choices <- mod_choices %>%
      set_names(display_name(.)) %>%
      keep(~length(unique(data()[[.x]])) > 1)
    checkboxGroupInput("moderators", label = "Moderators", valid_mod_choices,
                       inline = TRUE)
  })

  output$ma_help_text <- renderUI({
    req(input$ma_method)
    ma_help_texts <- c("REML" = "Assumes that true effect can vary between studies",
                       "REML_mv" = "Random effects model assuming studies within a paper share variance",
                       "FE" = "Assumes that all studies measure one true effect",
                       "EB" = "Estimates prior distribution of effect sizes")
    HTML(paste0("<i class=\"text-muted\">", ma_help_texts[input$ma_method], "</i>"))
  })

  output$es_help_text <- renderUI({
    req(input$es_type)
    es_help_texts <- c("d" = "Standardized difference between two means (or one mean and chance)",
                       "g" = "Cohen's d corrected for small sample sizes",
                       "r" = "Correlation coefficient of paired data",
                       "z" = "Correlation coefficient normalized to vary between -1 and 1",
                       "log_odds" = "Association between two categorical variables, log transformed")
    HTML(paste0("<i class=\"text-muted\">", es_help_texts[input$es_type], "</i>"))
  })

  output$studies_box <- renderValueBox({
    valueBox(
      nrow(data()), "Conditions", icon = icon("list", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", no_mod_model()$b[,1][["intrcpt"]]), "Effect Size",
      icon = icon("record", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_var_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", no_mod_model()$se[1]), "Effect Size SE",
      icon = icon("resize-horizontal", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_t2_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", no_mod_model()$tau2[1]), "Tau^2",
      icon = icon("resize-full", lib = "glyphicon"),
      color = "red"
    )
  })

  output$viz_boxes <- renderUI({
    if (!input$ma_method == "REML_mv") {
      list(
        valueBoxOutput("studies_box", width = 3),
        valueBoxOutput("effect_size_box", width = 3),
        valueBoxOutput("effect_size_var_box", width = 3),
        valueBoxOutput("effect_size_t2_box", width = 3))
    } else {
      list(
        valueBoxOutput("studies_box", width = 4),
        valueBoxOutput("effect_size_box", width = 4),
        valueBoxOutput("effect_size_var_box", width = 4))
    }
  })

  observeEvent(categorical_mods(), {
    if (!is.null(categorical_mods())) {
      updateSelectInput(session, "scatter_curve", selected = "lm")
    }
  })

  output$subset_selector <- renderUI({
    radioButtons("subset_input", "Subset", append(subsets(), "All data", 0))
  })

  # TODO use observe
  output$subset_options <- reactive({
    subsets()
  })
  outputOptions(output, "subset_options", suspendWhenHidden = FALSE)


  #############################################################################
  # PLOTS

  ########### SCATTER PLOT ###########

  scatter <- function() {
    req(input$scatter_curve)

    labels <- if (mod_group() == "all_mod") NULL else
      setNames(paste(mod_data()[[mod_group()]], "  "), mod_data()[[mod_group()]])

    guide <- if (mod_group() == "all_mod") FALSE else "legend"
    p <- ggplot(mod_data(), aes_string(x = "mean_age_months", y = es(),
                                       colour = mod_group(), label = "short_cite")) +
      geom_jitter(aes(size = n), alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      scale_colour_solarized(name = "", labels = labels, guide = guide) +
      scale_size_continuous(guide = FALSE) +
      xlab("\nMean Subject Age (Months)") +
      ylab("Effect Size\n")

    #curve <- if (is.null(categorical_mods())) input$scatter_curve else "lm"
    if (input$scatter_curve == "lm") {
      p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                      method = "lm", se = FALSE)
    } else if (input$scatter_curve == "loess") {
      p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                      method = "loess", se = FALSE, span = 1)
    }

    ggplotly(p, tooltip = c("label")) %>%
      layout(showlegend = FALSE)

  }

  output$scatter <- renderPlotly(scatter())

  output$longitudinal <- reactive({
    req(input$dataset_name)
    filter(datasets, name == input$dataset_name)$longitudinal
  })

  outputOptions(output, "longitudinal", suspendWhenHidden = FALSE)

  ########### VIOLIN PLOT ###########

  violin <- function() {
    plt_data <- mod_data()
    mod_factor <- factor(plt_data[[mod_group()]])
    plt_data[[mod_group()]] <- factor(plt_data[[mod_group()]],
                                      levels = rev(levels(mod_factor)))
    plt <- ggplot(plt_data, aes_string(x = mod_group(), y = es(),
                                       colour = mod_group(), label = "short_cite")) +
      coord_flip() +
      geom_violin() +
      geom_jitter(height = 0) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      scale_colour_solarized(name = "", guide = FALSE) +
      xlab("") +
      ylab("Effect Size\n")
    if (mod_group() == "all_mod") {
      plt <- plt + theme(axis.ticks.y = element_blank())
    } else {
      plt <- plt
    }

    ggplotly(plt, height = length(unique(mod_data()[[mod_group()]])) * 160 + 70,
             tooltip = c("label")) %>%
      layout(showlegend = FALSE)
  }

  output$violin <- renderPlotly(
    violin()
  )

  ########### FOREST PLOT ###########

  forest <- function() {
    f <- fitted(model())
    p <- predict(model())

    forest_data <- data.frame(effects = as.numeric(model()$yi.f),
                              variances = model()$vi.f, stringsAsFactors = FALSE) %>%
      mutate(effects.cil = effects -
               qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
             effects.cih = effects +
               qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
             estimate = as.numeric(f),
             short_cite = names(f),
             estimate.cil = p$ci.lb,
             estimate.cih = p$ci.ub,
             inverse_vars = 1/variances,
             identity = 1) %>%
      left_join(mutate(mod_data(), short_cite = make.unique(short_cite))) %>%
      arrange_(.dots = list(sprintf("desc(%s)", input$forest_sort),
                            "desc(effects)")) %>%
      mutate(short_cite = factor(short_cite, levels = short_cite))

    labels <- if (mod_group() == "all_mod") NULL else
      setNames(paste(mod_data()[[mod_group()]], "  "),
               mod_data()[[mod_group()]])
    guide <- if (mod_group() == "all_mod") FALSE else "legend"

    qplot(short_cite, effects, ymin = effects.cil, ymax = effects.cih,
          geom = "linerange",
          data = forest_data) +
      geom_point(aes(y = effects, size = inverse_vars)) +
      geom_pointrange(aes_string(x = "short_cite", y = "estimate",
                                 ymin = "estimate.cil", ymax = "estimate.cih",
                                 colour = mod_group()),
                      pch = 17) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      coord_flip() +
      scale_size_continuous(guide = FALSE) +
      scale_colour_solarized(name = "", labels = labels, guide = guide) +
      xlab("") +
      ylab("Effect Size")
  }

  forest_summary <- function() {
    pred_data <- data.frame(predictor = names(coef(model())),
                            coef = coef(model()),
                            ci.lb = summary(model())$ci.lb,
                            ci.ub = summary(model())$ci.ub, stringsAsFactors = FALSE)

    predictors <- data_frame(moderator = "", value = "", predictor = "intrcpt",
                             print_predictor = "intercept")
    if (!is.null(categorical_mods()) && length(categorical_mods())) {
      mod_vals <- map_df(categorical_mods(),
                         ~data_frame(moderator = .x,
                                     value = unique(mod_data()[[.x]]))) %>%
        mutate(predictor = paste0(moderator, value),
               print_predictor = sprintf("%s: %s", moderator, value))
      predictors <- predictors %>%
        bind_rows(mod_vals)
    }
    if ("mean_age" %in% input$moderators) {
      predictors <- predictors %>%
        bind_rows(data_frame(moderator = "", value = "", predictor = "mean_age",
                             print_predictor = "mean_age"))
    }
    pred_data %>% left_join(predictors) %>%
      ggplot(aes(x = print_predictor, y = coef, ymin = ci.lb,
                 ymax = ci.ub)) +
      geom_pointrange() +
      coord_flip() +
      xlab("") +
      ylab("Effect Size") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
  }

  output$forest <- renderPlot(forest(),
                              height = function() nrow(mod_data()) * 10 + 100)

  output$forest_summary <- renderPlot(forest_summary(),
                                      height = 200)

  output$forest_summary_text <- renderPrint({
    summary(model())
  })

  ########### FUNNEL PLOT ###########

  funnel <- function() {
    if (length(input$moderators) == 0) {
      d <- data_frame(se = sqrt(model()$vi), es = model()$yi)
      center <- mean(d$es)
      xlabel <- "\nEffect Size"
      ylabel <- "Standard Error\n"
    } else {
      r <- rstandard(model())
      d <- data_frame(se = r$se, es = r$resid)
      center <- 0
      xlabel <- "\nResidual Effect Size"
      ylabel <- "Residual Standard Error\n"
    }
    d[[mod_group()]] <- mod_data()[[mod_group()]]

    lower_lim <- max(d$se) + .05 * max(d$se)
    funnel95 <- data.frame(x = c(center - lower_lim * CRIT_95, center,
                                 center + lower_lim * CRIT_95),
                           y = c(-lower_lim, 0, -lower_lim),
                           stringsAsFactors = FALSE)

    left_lim99 <- ifelse(center - lower_lim * CRIT_99 < min(d$es),
                         center - lower_lim * CRIT_99,
                         min(d$es))
    right_lim99 <- ifelse(center + lower_lim * CRIT_99 > max(d$es),
                          center + lower_lim * CRIT_99,
                          max(d$es))
    funnel99 <- data.frame(x = c(center - lower_lim * CRIT_99, center,
                                 center + lower_lim * CRIT_99),
                           y = c(-lower_lim, 0, -lower_lim),
                           stringsAsFactors = FALSE)

    labels <- if (mod_group() == "all_mod") NULL else
      setNames(paste(mod_data()[[mod_group()]], "  "),
               mod_data()[[mod_group()]])
    guide <- if (mod_group() == "all_mod") FALSE else "legend"

    p <- ggplot(d, aes(x = es, y = -se)) +
      scale_colour_solarized(name = "", labels = labels, guide = guide) +
      scale_x_continuous(limits = c(left_lim99, right_lim99)) +
      scale_y_continuous(labels = function(x){abs(x)}) +
      geom_polygon(aes(x = x, y = y), data = funnel95, alpha = .5,
                   fill = "white") +
      geom_polygon(aes(x = x, y = y), data = funnel99, alpha = .5,
                   fill = "white") +
      geom_vline(xintercept = center, linetype = "dotted", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      geom_point(aes_string(colour = mod_group())) +
      xlab(xlabel) +
      ylab(ylabel) +
      geom_text(x = center + lower_lim * CRIT_95 / 2,
                y = -lower_lim + lower_lim / 30, #family = font,
                label = "p < .05", vjust = "bottom", hjust = "center") +
      geom_text(x = (center + lower_lim * CRIT_95) + (lower_lim * CRIT_99 - lower_lim * CRIT_95) / 2,
                y = -lower_lim + lower_lim / 30, #family = font,
                label = "p < .01", vjust = "bottom", hjust = "center") +
      theme(panel.background = element_rect(fill = "grey"),
            panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
            panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))

    ggplotly(p, tooltip = c("es", "-se")) %>%
      layout(showlegend = FALSE)
  }

  output$funnel <- renderPlotly(funnel())
  output$funnel_test <- renderText({
    funnel_test <- metafor::regtest(model())
    sprintf("Regression test for funnel plot asymmetry: z = %.3g, p = %.3g.
            Interpret with caution due to the possible presence of confounding
            moderators.", funnel_test$zval, funnel_test$pval)
  })


  #############################################################################
  # DOWNLOAD HANDLERS

  # LEAVE
  output$download_data <- downloadHandler(
    filename = function() sprintf("%s.csv", input$dataset_name),
    content = function(file) {
      readr::write_csv(data(), file)
    }
  )
})

