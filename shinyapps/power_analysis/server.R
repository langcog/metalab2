shinyServer(function(input, output, session) {
  #############################################################################
  # DOWNLOAD HANDLERS

  # plot_download_handler <- function(plot_name, plot_fun) {
  #   downloadHandler(
  #     filename = function() {
  #       sprintf("%s [%s].pdf", input$dataset_name, plot_name)
  #     },
  #     content = function(file) {
  #       cairo_pdf(file, width = 10, height = 7)
  #       print(plot_fun())
  #       dev.off()
  #     }
  #   )
  # }
  #
  # output$download_power <- plot_download_handler("power", power)

  # ########### DATA ##########

  pwrdata <- reactive({
    req(input$dataset_name_pwr)
    req(input$feature_option)
    result <- metavoice_data %>%
      filter(dataset == input$dataset_name_pwr) %>%
      filter(feature == input$feature_option)
    # result <- result[!is.infinite(result$d_calc),] #added to remove in rhd

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

  # subsets <- reactive({
  #   req(input$dataset_name_pwr)
  #   dataset_info %>%
  #     filter(name == input$dataset_name_pwr) %>%
  #     .$subset %>%
  #     unlist()
  # })

  dataset_names <- reactive({
    req(input$domain)
    dataset_info %>%
      filter(domain == input$domain) %>%
      pull(name)
  })

  feature_options <- reactive({
    req(input$dataset_name_pwr)
    dataset_info %>%
      filter(name == input$dataset_name_pwr) %>%
      .$features %>%
      unlist()
  })


  # ########### PWR MODEL ###########
  pwr_no_mod_model <- reactive({
    metafor::rma(d_calc, vi = d_var_calc, slab = as.character(expt_unique), #changed to expt_unique from study_ID (also below)
                 data = pwrdata(), method = "REML")
  })

  pwrmodel <- reactive({
    if (length(input$pwr_moderators) == 0) {
      pwr_no_mod_model()
    } else {
      mods <- paste(input$pwr_moderators, collapse = "+")
      metafor::rma(as.formula(paste("d_calc ~", mods)), vi = d_var_calc,
                   slab = as.character(expt_unique), data = pwrdata(), #CHANGE STUDY ID?
                   method = "REML")
    }
  })

  output$pwr_moderator_input <- renderUI({
    req(input$dataset_name_pwr) #CHANGE MODERATORS IN FOLLOWING ROW (maybe make if for different domains?)
    possible_mods <- list("Task type" = "task_type",
                        "Native language" = "native_language",
                        "Prosody type" = "prosody_type",
                        "Mean age (months)" = "mean_age")
    custom_mods <- dataset_info %>%
      filter(name == input$dataset_name_pwr) %>%
      .$moderators %>%
      unlist()
    print(custom_mods)
    mod_choices <- keep(possible_mods, ~ any(custom_mods %in% .x))
    valid_mod_choices <- mod_choices %>%
      set_names(display_name(.)) %>%
      keep(~length(unique(pwrdata()[[.x]])) > 1)

    # remove age moderator in longitudinal
    # if (filter(dataset_info, name == input$dataset_name_pwr)$longitudinal) { #CHANGE: COULD COMMENT OUT
    #   valid_mod_choices <- valid_mod_choices %>%
    #     keep(~.x != "mean_age_months")
    # }

    checkboxGroupInput("pwr_moderators", label = "Moderators",
                       valid_mod_choices,
                       inline = FALSE)
  })

  ########### RENDER UI FOR MODERATOR CHOICES #############
  output$pwr_moderator_choices <- renderUI({
    uis <- list()

    if (any(input$pwr_moderators == "mean_age_months")) {
      uis <- c(uis, list(
        sliderInput("pwr_age_months",
                    "Age of experimental participants (months)",
                    min = 0, max = ceiling(max(pwrdata()$mean_age_months)),
                    value = round(mean(pwrdata()$mean_age_months)),
                    step = 1)
      ))
    }

    #added this (copied from above + adjusted)
    if (any(input$pwr_moderators == "mean_age")) {
      uis <- c(uis, list(
        sliderInput("pwr_age_months",
                    "Age of experimental participants (months)",
                    min = 0, max = ceiling(max(pwrdata()$mean_age)),
                    value = round(mean(pwrdata()$mean_age)),
                    step = 1)
      ))
    }

    if (any(input$pwr_moderators == "task_type")) { #CHANGE ACCORDING TO TOK
      uis <- c(uis, list(
        selectInput("pwr_task_type",
                    "Task type",
                    choices = unique(pwrdata()$task_type)) #CHANGE ALL
      ))
    }

    if (any(input$pwr_moderators == "native_language")) {
      uis <- c(uis, list(
        selectInput("pwr_native_language",
                    "Native language",
                    choices = unique(pwrdata()$native_language)) #CHANGE ALL
      ))
    }

    if (any(input$pwr_moderators == "prosody_type")) {
      uis <- c(uis, list(
        selectInput("pwr_prosody_type",
                    "Prosody type",
                    choices = unique(pwrdata()$prosody_type)) #CHANGE ALL
      ))
    }

    return(uis)
  })

  ########### POWER COMPUTATIONS #############

  # this is awful because RMA makes factors and dummy-codes them, so newpred
  # needs to have this structure.
  d_pwr <- reactive({
    if (length(input$pwr_moderators > 0)) {
      newpred_mat <- matrix(nrow = 0, ncol = 0)

      if (any(input$pwr_moderators == "mean_age_months")) { #CHANGE: ADJUST TO OUT AGE?
        req(input$pwr_age_months)
        newpred_mat <- c(newpred_mat, input$pwr_age_months)
      }

      #added: copied from above + adjusted
      if (any(input$pwr_moderators == "mean_age")) { #CHANGE: ADJUST TO OUT AGE?
        req(input$pwr_age_months)
        newpred_mat <- c(newpred_mat, input$pwr_age_months)
      }

      if (any(input$pwr_moderators == "task_type")) { #CHANGE
        req(input$pwr_task_type)

        f_task_type <- factor(pwrdata()$task_type) #CHANGE
        n <- length(levels(f_task_type)) #CHANGE

        task_type_pred <- rep(0, n)
        pred_seq <- seq(1:n)[levels(f_task_type) == input$pwr_task_type] #CHANGE
        task_type_pred[pred_seq] <- 1

        # remove intercept
        task_type_pred <- task_type_pred[-1]

        newpred_mat <- c(newpred_mat, task_type_pred)
      }

      if (any(input$pwr_moderators == "native_language")) { #CHANGE ALL BELOW FROM EXPOSURE
        req(input$pwr_native_language)

        f_native_language <- factor(pwrdata()$native_language)
        n <- length(levels(f_native_language))

        native_language_pred <- rep(0, n)
        native_language_pred[seq(1:n)[levels(f_native_language) == input$pwr_native_language]] <- 1

        # remove intercept
        native_language_pred <- native_language_pred[-1]

        newpred_mat <- c(newpred_mat, native_language_pred)
      }

      if (any(input$pwr_moderators == "prosody_type")) { #CHANGE ALL BELOW FROM EXPOSURE
        req(input$pwr_prosody_type)

        f_prosody_type <- factor(pwrdata()$prosody_type)
        n <- length(levels(f_prosody_type))

        prosody_type_pred <- rep(0, n)
        prosody_type_pred[seq(1:n)[levels(f_prosody_type) == input$pwr_prosody_type]] <- 1

        # remove intercept
        prosody_type_pred <- prosody_type_pred[-1]

        newpred_mat <- c(newpred_mat, prosody_type_pred)
      }

      predict(pwrmodel(), newmods = newpred_mat)$pred
    } else {
      # special case when there are no predictors, predict doesn't work
      pwrmodel()$b[,1][["intrcpt"]]
    }
  })

  # pwr_80 <- reactive({
  #   pwr::pwr.p.test(h = d_pwr(),
  #                   sig.level = .05,
  #                   power = .8)$n
  # })

  pwr_80 <- reactive({
    pwr::pwr.2p.test(h = d_pwr(), sig.level = 0.05, power = .8)$n

  })



  ## now do the actual power analysis plot

  # output$power <- renderPlot({
  #   max_n <- min(max(60,
  #                    pwr::pwr.p.test(h = d_pwr(),
  #                                    sig.level = .05,
  #                                    power = .9)$n),
  #                200)

  output$power <- renderPlot({
    max_n <- min(max(60,
                     pwr::pwr.2p.test(h = d_pwr(),
                                     sig.level = .05,
                                     power = .9)$n), 200)



    # pwrs <- data.frame(ns = seq(5, max_n, 5),
    #                    ps = pwr::pwr.p.test(h = d_pwr(),
    #                                         n = seq(5, max_n, 5),
    #                                         sig.level = .05)$power,
    #                    stringsAsFactors = FALSE)

    pwrs <- data.frame(ns = seq(5, max_n, 5),
                       ps = pwr::pwr.2p.test(h = d_pwr(),
                                            n = seq(5, max_n, 5),
                                            sig.level = .05)$power,
                       stringsAsFactors = FALSE)


    qplot(ns, ps, geom = c("point","line"),
          data = pwrs) +
      geom_hline(yintercept = .8, lty = 2) +
      geom_vline(xintercept = pwr_80(), lty = 3) +
      ylim(c(0,1)) +
      xlim(c(0,max_n)) +
      ylab("Power to reject the null at p < .05") +
      #xlab("Number of participants (N)")
      xlab("Number of participants per group")
  })

  ### UI ELEMENTS

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
    selectInput(inputId = "dataset_name_pwr",
                label = "Meta-analysis",
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

  output$feature_help_text <- renderUI({
    req(input$feature_option)
    feature_help_texts <- c("pitch_f0" = "Mean pitch",
                            "speech_duration" = "Mean speech duration / or total?",
                            "pause_duration" = "Pause dur")
    HTML(paste0("<i class=\"text-muted\">", feature_help_texts[input$feature_option], "</i>"))
  })

  # output$subset_selector <- renderUI({
  #   radioButtons("subset_input", "Subset", append(subsets(), "All data", 0)) #COULD REMOVE
  # })

  # output$subset_options <- reactive({
  #   subsets()
  # })
  # outputOptions(output, "subset_options", suspendWhenHidden = FALSE)

  ### POWER BOXES
  output$power_d <- renderValueBox({
    valueBox(
      round(d_pwr(), digits = 2), "Effect Size", icon = icon("record", lib = "glyphicon"),
      color = "red"
    )
  })

  output$power_n <- renderValueBox({
    valueBox(
      if(pwr_80() < 200) {round(pwr_80(), digits = 2) } else { "> 200"}, "N for 80% power",
      icon = icon("list", lib = "glyphicon"),
      color = "blue"
    )
  })
})
