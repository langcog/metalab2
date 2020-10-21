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
  })

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
    metafor::rma(d_calc, vi = d_var_calc, slab = as.character(expt_unique),
                 data = pwrdata(), method = "REML")
  })

  pwrmodel <- reactive({
    if (length(input$pwr_moderators) == 0) {
      pwr_no_mod_model()
    } else {
      mods <- paste(input$pwr_moderators, collapse = "+")
      metafor::rma(as.formula(paste("d_calc ~", mods)), vi = d_var_calc,
                   slab = as.character(expt_unique), data = pwrdata(),
                   method = "REML")
    }
  })

  output$pwr_moderator_input <- renderUI({
    req(input$dataset_name_pwr)
    possible_mods <- list("Task type" = "task_type",
                        "Native language" = "native_language",
                        "Prosody type" = "prosody_type",
                        "Mean age (months)" = "mean_age",
                        "Diagnosis specification" = "diagnosis_specification")
    custom_mods <- dataset_info %>%
      filter(name == input$dataset_name_pwr) %>%
      .$moderators %>%
      unlist()
    print(custom_mods)
    mod_choices <- keep(possible_mods, ~ any(custom_mods %in% .x))
    valid_mod_choices <- mod_choices %>%
      set_names(display_name(.)) %>%
      keep(~length(unique(pwrdata()[[.x]])) > 1)

    checkboxGroupInput("pwr_moderators", label = "Moderators",
                       valid_mod_choices,
                       inline = FALSE)
  })

  ########### RENDER UI FOR MODERATOR CHOICES #############
  output$pwr_moderator_choices <- renderUI({
    uis <- list()

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

    if (any(input$pwr_moderators == "task_type")) {
      uis <- c(uis, list(
        selectInput("pwr_task_type",
                    "Task type",
                    choices = unique(pwrdata()$task_type))
      ))
    }

    if (any(input$pwr_moderators == "native_language")) {
      uis <- c(uis, list(
        selectInput("pwr_native_language",
                    "Native language",
                    choices = unique(pwrdata()$native_language))
      ))
    }

    if (any(input$pwr_moderators == "prosody_type")) {
      uis <- c(uis, list(
        selectInput("pwr_prosody_type",
                    "Prosody type",
                    choices = unique(pwrdata()$prosody_type))
      ))
    }

    if (any(input$pwr_moderators == "diagnosis_specification")) {
      uis <- c(uis, list(
        selectInput("pwr_diagnosis_specification",
                    "Diagnosis specification",
                    choices = unique(pwrdata()$diagnosis_specification))
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

      if (any(input$pwr_moderators == "mean_age")) {
        req(input$pwr_age_months)
        newpred_mat <- c(newpred_mat, input$pwr_age_months)
      }

      if (any(input$pwr_moderators == "task_type")) {
        req(input$pwr_task_type)

        f_task_type <- factor(pwrdata()$task_type)
        n <- length(levels(f_task_type))

        task_type_pred <- rep(0, n)
        pred_seq <- seq(1:n)[levels(f_task_type) == input$pwr_task_type]
        task_type_pred[pred_seq] <- 1

        # remove intercept
        task_type_pred <- task_type_pred[-1]

        newpred_mat <- c(newpred_mat, task_type_pred)
      }

      if (any(input$pwr_moderators == "native_language")) {
        req(input$pwr_native_language)

        f_native_language <- factor(pwrdata()$native_language)
        n <- length(levels(f_native_language))

        native_language_pred <- rep(0, n)
        native_language_pred[seq(1:n)[levels(f_native_language) == input$pwr_native_language]] <- 1

        # remove intercept
        native_language_pred <- native_language_pred[-1]

        newpred_mat <- c(newpred_mat, native_language_pred)
      }

      if (any(input$pwr_moderators == "prosody_type")) {
        req(input$pwr_prosody_type)

        f_prosody_type <- factor(pwrdata()$prosody_type)
        n <- length(levels(f_prosody_type))

        prosody_type_pred <- rep(0, n)
        prosody_type_pred[seq(1:n)[levels(f_prosody_type) == input$pwr_prosody_type]] <- 1

        # remove intercept
        prosody_type_pred <- prosody_type_pred[-1]

        newpred_mat <- c(newpred_mat, prosody_type_pred)
      }

      if (any(input$pwr_moderators == "diagnosis_specification")) {
        req(input$pwr_diagnosis_specification)

        f_diagnosis_specification <- factor(pwrdata()$diagnosis_specification)
        n <- length(levels(f_diagnosis_specification))

        diagnosis_specification_pred <- rep(0, n)
        diagnosis_specification_pred[seq(1:n)[levels(f_diagnosis_specification) == input$pwr_diagnosis_specification]] <- 1

        # remove intercept
        diagnosis_specification_pred <- diagnosis_specification_pred[-1]

        newpred_mat <- c(newpred_mat, diagnosis_specification_pred)
      }

      predict(pwrmodel(), newmods = newpred_mat)$pred

    } else {
      # special case when there are no predictors, predict doesn't work
      pwrmodel()$b[,1][["intrcpt"]]
    }
  })

  # calculate n per group for 80% power
  pwr_80 <- reactive({
    pwr::pwr.t.test(d = d_pwr(), sig.level = 0.05, power = .8)$n

  })


  ## now do the actual power analysis plot

  output$power <- renderPlot({
    max_n <- min(max(60, pwr::pwr.t.test(d = d_pwr(), sig.level = .05, power = .9)$n), 200)

    pwrs <- data.frame(ns = seq(5, max_n, 5),
                       ps = pwr::pwr.t.test(d = d_pwr(), n = seq(5, max_n, 5), sig.level = .05)$power,
                       stringsAsFactors = FALSE)


    qplot(ns, ps, geom = c("point","line"),
          data = pwrs) +
      geom_hline(yintercept = .8, lty = 2) +
      geom_vline(xintercept = pwr_80(), lty = 3) +
      ylim(c(0,1)) +
      xlim(c(0,max_n)) +
      ylab("Power to reject the null hypothesis at p < .05") +
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


  ### POWER BOXES
  output$power_d <- renderValueBox({
    valueBox(
      round(d_pwr(), digits = 2), "Effect Size", icon = icon("record", lib = "glyphicon"),
      color = "navy"
    )
  })

  output$power_n <- renderValueBox({
    valueBox(
      if(pwr_80() < 200) {round(pwr_80(), digits = 2) } else { "> 200"}, "N for 80% power",
      tags$i(class = "fas fa-users",
        style = "color: rgb(54,72,105)"),
      icon("users"),
      color = "navy"
    )
  })

  output$power_s <- renderValueBox({
    valueBox(
      nrow(pwrdata()), "Experiments", icon = icon("list", lib = "glyphicon"),
      color = "navy"
    )
    })

  output$fewstudies <- renderText({"There are less than two studies, which have investigated this feature. Please choose a different feature."})
  output$contribute <- renderText({"If you would like to contribute with a study, see CONTRIBUTELINK"})

  # output$link_to_dataset <- renderUI({
  #   req(input$dataset_name)
  #   base_url <- "https://langcog.github.io/metalab2/dataset/" # change to our website
  #   short_name <- dataset_info %>%
  #     filter(name == input$dataset_name) %>%
  #     select(short_name)
  #   HTML(paste0("<i class=\"text-muted\">For more information see
  #               <a href='https://langcog.github.io/metalab2/documentation.html#dataset_info' target='_blank'>
  #               Documentation</a> or <a href='", base_url, short_name, ".html', target='_blank'>
  #               View raw dataset</a>. Please cite the dataset_info that you use following <a href='https://langcog.github.io/metalab2/publications.html' target='_blank'> our citation policy.</a> </a></i>"))
  # })

  output$conditional_results <- renderUI({
    if (nrow(pwrdata()) > 1){
      list(fluidRow(valueBoxOutput("power_d", width = 4),
                    valueBoxOutput("power_n", width = 4),
                    valueBoxOutput("power_s", width = 4)),
            box(width = NULL, #status = "danger",
             fluidRow(
               column(width = 10,
                      p(strong("Power plot"), "of N necessary to achieve p < .05"),
                      plotOutput("power"),
                      p("Statistical power to detect a difference between conditions at p < .05. Dashed line shows 80% power, dotted lineshows necessary sample size to achieve that level of power.")
               ))))}
      else {
        box(width = NULL, fluidRow(column(width = 12, textOutput("fewstudies"), br(), textOutput("contribute"))))
      }
  })
  })
