################################################################################
## CONSTANTS
alpha <- .05
CRIT_95 <- 1.96
CRIT_99 <- 2.58

shinyServer(function(input, output, session) {

  ##############################################################################
  # MODELS AND REACTIVES

  ########### DATA ###########
  ma_method <- "REML_mv"

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

  feature_options <- reactive({
    req(input$dataset_name)
    dataset_info %>%
      filter(name == input$dataset_name) %>%
      .$features %>%
      unlist()
  })

  dataset_names <- reactive({
    req(input$domain)
    dataset_info %>%
      filter(domain == input$domain) %>%
      pull(name)
  })

  data <- reactive({
    req(input$dataset_name)
    req(input$feature_option)
    result <- metavoice_data %>%
      filter(dataset == input$dataset_name) %>% ## MLL changed 3000 -> 4000 #mean_age < 4000
      filter(feature == input$feature_option)

    # subset <- input$subset_input
    # if (!is.null(subset)) {
    #   if (subset != "All data") {
    #     result %>% filter_(paste(subset, "== TRUE"))
    #   } else {
    #     result
    #   }
    # } else {
    #   result
    # }
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
    metavoice_data %>%
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
      if (ma_method == "REML_mv") {
        metafor::rma.mv(rma_formula, V = mod_data()[[es_var()]],
                        random = ~ 1 | same_sample / short_cite / unique_row,
                        #Cluster by paper, then participant group, then add random effect for each effect size
                        slab = expt_unique, data = mod_data(),
                        method = "REML")
      # } else {
      #   metafor::rma(rma_formula, vi = mod_data()[[es_var()]],
      #                slab = make.unique(short_cite), data = mod_data(),
      #                method = ma_method)
      }
    }
  })

  no_intercept_model <- reactive({
    if (length(input$moderators) == 0) {
      no_mod_model()
    } else {
      mods <- paste(input$moderators, collapse = "+")
      rma_formula <- as.formula(sprintf("%s ~ 0 + %s", es(), mods))
      if (ma_method == "REML_mv") {
        metafor::rma.mv(rma_formula, V = mod_data()[[es_var()]],
                        random = ~ 1 | same_sample / short_cite / unique_row,
                        #Cluster by participant group, paper, then participant group, and for each row/effect size
                        slab = expt_unique, data = mod_data(),
                        method = "REML")
      # } else {
      #   metafor::rma(rma_formula, vi = mod_data()[[es_var()]],
      #                slab = expt_unique, data = mod_data(),
      #                method = ma_method)
      }
    }
  })

  no_mod_model <- reactive({
    if (ma_method == "REML_mv") {
      metafor::rma.mv(yi = data()[[es()]], V = data()[[es_var()]],
                      random = ~ 1 | data()[["same_sample"]] /data()[["short_cite"]] / data()[["unique_row"]],
                      slab = data()[["expt_unique"]],
                      method = "REML")
    # } else {
    #   metafor::rma(yi = data()[[es()]], vi = data()[[es_var()]],
    #                slab = make.unique(data()[["short_cite"]]),
    #                method = ma_method)

    }
  })

  ########### UI ELEMENTS ###########

  display_name <- function(fields) {
    sp <- gsub("_", " ", fields)
    paste0(toupper(substring(sp, 1, 1)), substring(sp, 2))
  }

  output$domain_selector <- renderUI({
    selectInput(inputId = "domain",
                label = "Domain",
                choices = dataset_info$domain %>%
                  unique %>%
                  set_names(display_name(.))
    )
  })

  output$dataset_name <- renderUI({
    selectInput(inputId = "dataset_name",
                label = "Dataset",
                choices = dataset_names()
    )
  })

  output$feature_selector <- renderUI({
    selectInput(inputId = "feature_option",
                label = "Feature",
                choices = feature_options() %>%
                  set_names(display_name(.))
    )
  })

  output$link_to_dataset <- renderUI({
    req(input$dataset_name)
    base_url <- "https://langcog.github.io/metalab2/dataset/" # change to our website
    short_name <- dataset_info %>%
      filter(name == input$dataset_name) %>%
      select(short_name)
    HTML(paste0("<i class=\"text-muted\">For more information see
                <a href='https://langcog.github.io/metalab2/documentation.html#dataset_info' target='_blank'>
                Documentation</a> or <a href='", base_url, short_name, ".html', target='_blank'>
                View raw dataset</a>. Please cite the dataset_info that you use following <a href='https://langcog.github.io/metalab2/publications.html' target='_blank'> our citation policy.</a> </a></i>"))
  })

    output$data_description <- renderText({
    req(input$dataset_name)
    short_desc <- dataset_info %>%
      filter(name == input$dataset_name) %>%
      select(short_desc)
    paste(short_desc)})

   output$data_citation <- renderText({
     req(input$dataset_name)
     full_citation <- dataset_info %>%
       filter(name == input$dataset_name) %>%
       select(full_citation)
     paste(full_citation)})

  output$ma_model_blurb <- renderUI({
    HTML(paste0("Random effects model assuming studies within a paper share variance. For details, see
                <a href='https://metalab.stanford.edu/documentation.html#statistical_approach' target='_blank'>
                Statistical Approach</a>."))
  })

  # output$dataset_name <- renderUI({
  #   selectInput(inputId = "dataset_name",
  #               label = "Dataset",
  #               choices = dataset_names()
  #   )
  # }

  output$moderator_input <- renderUI({
    req(input$dataset_name)
    custom_mods <- dataset_info %>%
      filter(name == input$dataset_name) %>%
      .$moderators %>%
      unlist()
    mod_choices <- custom_mods #c("mean_age", "response_mode", "exposure_phase", custom_mods)
    valid_mod_choices <- mod_choices %>%
      set_names(display_name(.)) %>%
      keep(~length(unique(data()[[.x]])) > 1)
    checkboxGroupInput("moderators", label = "Moderators", valid_mod_choices,
                       inline = FALSE)
  })

  output$ma_help_text <- renderUI({
    req(ma_method)
    ma_help_texts <- c("REML_mv" = "Random effects model assuming studies within a paper share variance",
                       "REML" = "Assumes that true effect can vary between studies",
                       "FE" = "Assumes that all studies measure one true effect",
                       "EB" = "Estimates prior distribution of effect sizes")
    HTML(paste0("<i class=\"text-muted\">", ma_help_texts[ma_method], "</i>"))
  })

  output$es_help_text <- renderUI({
    req(input$es_type)
    es_help_texts <- c("g" = "Cohen's d corrected for small sample sizes",
                       "d" = "Standardized difference between two means (or one mean and chance)",
                       "r" = "Correlation coefficient of paired data",
                       "log_odds" = "Association between two categorical variables, log transformed")
    HTML(paste0("<i class=\"text-muted\">", es_help_texts[input$es_type], "</i>"))
  })

  output$feature_help_text <- renderUI({
    req(input$feature_option)
    feature_help_texts <- c("pitch_f0" = "Mean pitch",
                       "speech_duration" = "Mean speech duration / or total?",
                       "pause_duration" = "Pause dur")
    HTML(paste0("<i class=\"text-muted\">", feature_help_texts[input$feature_option], "</i>"))
  })

  output$studies_box <- renderValueBox({
    valueBox(
      nrow(data()), "Experiments", icon = icon("list", lib = "glyphicon"),
      color = "blue"
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
    if (!ma_method == "REML_mv") {
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
  # output$subset_selector <- renderUI({
  #   radioButtons("subset_input", "Subset", append(feature_options(), "All data", 0))
  # })

  # TODO use observe
  # output$subset_options <- reactive({
  #   feature_options()
  # })
  # outputOptions(output, "subset_options", suspendWhenHidden = FALSE)


  #############################################################################
  # PLOTS

  ########### SCATTER PLOT ###########

  scatter <- function() {
    req(input$scatter_curve)

    labels <- if (mod_group() == "all_mod") NULL else
      setNames(paste(mod_data()[[mod_group()]], "  "), mod_data()[[mod_group()]])

    guide <- if (mod_group() == "all_mod") FALSE else "legend"

    p <- if ("mean_age" %in% input$moderators) {
       p <- ggplot(mod_data(), aes_string(x = "mean_age", y = es(), color = mod_group())) +
        geom_point(aes(size = n, text = paste(expt_unique), alpha=0.5)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        scale_colour_solarized(name = "", labels = labels, guide = guide) +
        scale_size_continuous(guide = FALSE) +
        xlab("\nMean Subject Age (Months)") +
        ylab("Effect Size\n")

        if (input$scatter_curve == "lm") {
          p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                             method = "lm", se = FALSE)
        } else if (input$scatter_curve == "loess") {
        p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                              method = "loess", se = FALSE, span = 1)
        }

    } else {
       ggplot(mod_data(), aes_string(x = mod_group(), y = es(), color = mod_group())) +
                  geom_point(position = "jitter", aes(size = n, text = paste(expt_unique), alpha=0.5)) +
                  geom_boxplot(fill = "white", alpha=0.5) +
                  labs(x = "Task Type", y = "Effect Size") +
                  scale_colour_solarized(name = "", labels = labels, guide = guide) +
                  scale_size_continuous(guide = FALSE)
    }

       # p <- ggplot(mod_data(), aes_string(x = "task_type", y = es(),
    #                                    colour = mod_group())) +
    #   geom_jitter(aes(size = n, text = paste(short_cite, expt_num)), alpha = 0.5) +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    #   scale_colour_solarized(name = "", labels = labels, guide = guide) +
    #   scale_size_continuous(guide = FALSE) +
    #   xlab("\nMean Subject Age (Months)") +
    #   ylab("Effect Size\n")

    #curve <- if (is.null(categorical_mods())) input$scatter_curve else "lm"
    # if (input$scatter_curve == "lm") {
    #   p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
    #                   method = "lm", se = FALSE)
    # } else if (input$scatter_curve == "loess") {
    #   p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
    #                   method = "loess", se = FALSE, span = 1)
    # }

    p <- ggplotly(p, tooltip = c("text"))

    if (mod_group() != "all_mod") {
      p
    } else {
      p %>%
        layout(showlegend = FALSE)
    }

  }

  output$scatter <- renderPlotly(scatter())

  output$longitudinal <- reactive({
    req(input$dataset_name)
    filter(dataset_info, name == input$dataset_name)$longitudinal
  })

  outputOptions(output, "longitudinal", suspendWhenHidden = FALSE)

  ########### VIOLIN PLOT ###########

  violin <- function() {
    plt_data <- mod_data()
    mod_factor <- factor(plt_data[[mod_group()]])
    plt_data[[mod_group()]] <- factor(plt_data[[mod_group()]],
                                      levels = rev(levels(mod_factor)))
    plt <- ggplot(plt_data, aes_string(x = mod_group(), y = es(),
                                       colour = mod_group())) +
      coord_flip() +
      geom_violin() +
      geom_jitter(aes(text = expt_unique), height = 0) +
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
             tooltip = c("text")) %>%
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

    plt <- ggplot(data = forest_data) +
      geom_point(aes(x = short_cite, y = effects, size = inverse_vars, text = expt_unique)) +
      geom_linerange(aes(x = short_cite, y = effects, ymin = effects.cil, ymax = effects.cih)) +
      geom_point(aes_string(x = "short_cite", y = "estimate", colour = mod_group()),
                 shape = 17) +
      geom_linerange(aes_string(x = "short_cite", y = "estimate", ymin = "estimate.cil",
                                ymax = "estimate.cih", colour = mod_group(), alpha = 0.7)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      coord_flip() +
      scale_size_continuous(range = c(1, 3), guide = FALSE) +
      scale_colour_solarized(name = "", labels = labels, guide = guide) +
      xlab("") +
      ylab("Effect Size")

    # ggplotly(plt, tooltip = c("text")) %>%
    #   layout(showlegend = FALSE)
    plt <- ggplotly(plt, tooltip = c("text"))

    if (mod_group() != "all_mod") {
      plt
    } else {
      plt %>%
        layout(showlegend = FALSE)
    }

  }

  # INTERCEPT MODEL
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

  output$forest <- renderPlotly({
    session$sendCustomMessage(type = "heightCallback", paste0(nrow(mod_data()) * 12 + 100, "px"))
    forest()
  })

  ## This is the rendering function for the meta-analytic model summery of the effect size
  output$forest_summary <- renderPlot(forest_summary(), height = 200)

  ## This is the output function for the model summary
  output$forest_summary_text <- renderPrint({
    summary(model())
  })

  # NO INTERCEPT MODEL
  forest_no_intercept_summary <- function() {
    pred_data <- data.frame(predictor = names(coef(no_intercept_model())),
                            coef = coef(no_intercept_model()),
                            ci.lb = summary(no_intercept_model())$ci.lb,
                            ci.ub = summary(no_intercept_model())$ci.ub, stringsAsFactors = FALSE)

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

  output$forest <- renderPlotly({
    session$sendCustomMessage(type = "heightCallback", paste0(nrow(mod_data()) * 12 + 100, "px"))
    forest()
  })

  ## This is the rendering function for the meta-analytic model summery of the effect size
  output$forest_no_intercept_summary <- renderPlot(forest_no_intercept_summary(), height = 200)

  ## This is the output function for the model summary
  output$forest_no_intercept_summary_text <- renderPrint({
    summary(no_intercept_model())
  })

  output$no_intercept_text <- renderText({
    "Please select a moderator to display model summary and plot for model without intercept."
  })

  output$no_intercept <- renderUI({
    if (length(input$moderators) == 0){
      list(br(),
           textOutput("no_intercept_text"),
           br())
    } else {
      list(
        tabsetPanel(
          tabPanel("Plot",
                 plotOutput("forest_no_intercept_summary", height = "auto")),
          tabPanel("Model",
                 p(verbatimTextOutput("forest_no_intercept_summary_text")))
      ),
      br(),
      helpText("Plot and model output for chosen meta-analytic model
                                    (selected at the top of this page)."))
    }
    })

  ########### FUNNEL PLOT ###########

  funnel <- function() {
    if (length(input$moderators) == 0) {
      d <- data_frame(se = sqrt(model()$vi), es = model()$yi, slab = model()$slab)
      center <- mean(d$es)
      xlabel <- "\nEffect Size"
      ylabel <- "Standard Error\n"
    } else {
      r <- rstandard(model())
      d <- data_frame(se = r$se, es = r$resid, slab = r$slab)
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
    p <- ggplot(d) +
      geom_polygon(aes(x = x, y = y), data = funnel95, alpha = .5,
                   fill = "white") +
      geom_polygon(aes(x = x, y = y), data = funnel99, alpha = .5,
                   fill = "white") +
      geom_point(aes_string(x = "es", y = "-se", colour = mod_group(), text = "slab")) +
      #aes(size = n, text = paste(expt_unique), alpha=0.5)
      geom_vline(aes(), xintercept = center, linetype = "dotted", color = "black") +
      xlab(xlabel) +
      ylab(ylabel) +
      scale_colour_solarized(name = "", labels = labels, guide = guide) +
      scale_x_continuous(limits = c(left_lim99, right_lim99)) +
      scale_y_continuous(labels = function(x){abs(x)}) +
      theme(panel.background = element_rect(fill = "grey"),
            panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
            panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))

    # ggplotly hack - avoid weird lines by preventing overlapping geom_vlines
    ## if (center != 0) {
    ##   p <- p + geom_vline(aes(), xintercept = 0, linetype = "dashed", color = "grey")
    ## }

    p <- ggplotly(p, tooltip = c("text"))

    if (mod_group() != "all_mod") {
      p
    } else {
      p %>%
        layout(showlegend = FALSE)
    }
  }

  output$funnel <- renderPlotly(funnel())
  output$funnel_test <- renderText({
    funnel_test <- metafor::regtest(model())
    sprintf("Regression test for funnel plot asymmetry: z = %.3g, p = %.3g.
            Interpret with caution due to the possible presence of confounding
            moderators.", funnel_test$zval, funnel_test$pval)
  })

  #### CONDITIONAL LIMIT FOR NUMBER OF EXPERIMENTS
  # Conditional left column
  output$experiment_limit_left <- renderUI({
    if (nrow(data()) > 1){
      list(
        box(width = NULL, #status = "danger",
            fluidRow(
              column(width = 10,
                     p(strong("Funnel plot"), "of bias in effect sizes"))
            ),
            plotlyOutput("funnel"),
            div(class = "text-center", textOutput("funnel_test")),
            br(),
            helpText("Studies with high precision should be close to the average,
                       while studies with low precision can be spread evenly on both sides of
                       the average when there is no publication bias. An asymmetric shape can be
                       an indicator of publication bias. Shaded regions show p <.05 and p <.01
                       regions, respectively.")),
        box(width = NULL, #status = "danger",
            fluidRow(
              column(width = 10,
                     p(strong("Violin plot"), "of effect size density"))
            ),
            plotlyOutput("violin", height = "auto"),
            br(),
            helpText("The probability density of the data at different values.")
        )
      )
    } else {
    }
  })

  # Conditional right column
  output$fewstudies <- renderText({"Currently, there is less than 1 study investigating this feature. Therefore, no model can be run for this feature. For a visualisation of the single study, see the box plot on the left."})
  output$contribute <- renderText({"If you would like to contribute with a study to the database, see CONTRIBUTELINK"})

  output$experiment_limit_right <- renderUI({
    if (nrow(data()) > 1){
      list(
        fluidRow(
          box(width = NULL, #status = "danger",
              fluidRow(
                column(
                  width = 10,
                  p(strong("Forest plot"),
                    "of effect sizes and meta-analysis model estimates"))
              ),
              fluidRow(width=10,
                       column(
                         width = 10,
                         selectInput("forest_sort", label = "Sort order",
                                     choices = c("weight (1/variance)" = "variances",
                                                 "effect size" = "effects",
                                                 "model estimate" = "estimate",
                                                 "alphabetical" = "study_ID",
                                                 "chronological" = "year")),
                         bsPopover("forest_sort", title = NULL,
                                   content = HTML("<small>Method to sort results</small>"),
                                   placement = "right")
                       )),
              plotlyOutput("forest"),
              # ggplotly hack - renderPlotly does not take height param; must alter in UI
              tags$script('
                  Shiny.addCustomMessageHandler("heightCallback",
                    function(height) {
                      document.getElementById("forest").style.height = height;
                    });
                '),
              #tags$style(type="text/css", "#forest { float:right;}"),
              br(),
              helpText("Estimated results and their confidence intervals in a particular order.
                         Colored points represent meta-analytic model summary. The size of the dots reflects the weight of the particular experiment in the model.")),
          box(id = "forest_summary_box", width = NULL, #status = "danger",
              fluidRow(
                column(width = 12,
                       p(strong("Summary of meta-analytic model"), "with intercept"),
                       tabsetPanel(
                         tabPanel("Plot",
                                  plotOutput("forest_summary", height = "auto")),
                         tabPanel("Model",
                                  p(verbatimTextOutput("forest_summary_text")))
                       ),
                       br(),
                       helpText("Plot and model output for chosen meta-analytic model
                                  (selected at the top of this page).")
                )
              )
          ),
          box(id = "forest_no_intercept_summary_box", width = NULL, #status = "danger",
              fluidRow(
                column(width = 12,
                       p(strong("Summary of meta-analytic model"), "without intercept"),
                       uiOutput("no_intercept")
                )
              )
          ))
      )
    } else { # KEEP THIS
      box(width = NULL, fluidRow(column(width = 12, textOutput("fewstudies"), br(), textOutput("contribute"))))
    }
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

