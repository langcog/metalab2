shinyServer(function(input, output, session) {
  #############################################################################
  # DOWNLOAD HANDLERS

  plot_download_handler <- function(plot_name, plot_fun) {
    downloadHandler(
      filename = function() {
        sprintf("%s [%s].pdf", input$dataset_name, plot_name)
      },
      content = function(file) {
        cairo_pdf(file, width = 10, height = 7)
        print(plot_fun())
        dev.off()
      }
    )
  }

  output$download_power <- plot_download_handler("power", power)

  # ########### DATA ###########
  pwrdata <- reactive({
    req(input$dataset_name_pwr)
    result <- metalab_data %>%
      filter(dataset == input$dataset_name_pwr)

    subset <- input$subset_input
    if (!is.null(subset)) {
      if (subset != "All data") {
        result %>% filter_(paste(subset, "== TRUE"))
      } else {
        result
      }
    } else {
      result
    }
  })

  subsets <- reactive({
    req(input$dataset_name_pwr)
    dataset_info %>%
      filter(name == input$dataset_name_pwr) %>%
      .$subset %>%
      unlist()
  })

  dataset_names <- reactive({
    req(input$domain)
    dataset_info %>%
      filter(domain == input$domain) %>%
      pull(name)
  })

  # ########### PWR MODEL ###########
  pwr_no_mod_model <- reactive({
    metafor::rma(d_calc, vi = d_var_calc, slab = as.character(study_ID),
                 data = pwrdata(), method = "REML")
  })

  pwrmodel <- reactive({
    if (length(input$pwr_moderators) == 0) {
      pwr_no_mod_model()
    } else {
      mods <- paste(input$pwr_moderators, collapse = "+")
      metafor::rma(as.formula(paste("d_calc ~", mods)), vi = d_var_calc,
                   slab = as.character(study_ID), data = pwrdata(),
                   method = "REML")
    }
  })

  output$pwr_moderator_input <- renderUI({
    req(input$dataset_name_pwr)
    mod_choices <- list("Age" = "mean_age_months",
                        "Response mode" = "response_mode",
                        "Exposure phase" = "exposure_phase")
    valid_mod_choices <- mod_choices %>%
      keep(~length(unique(pwrdata()[[.x]])) > 1)

    # remove age moderator in longitudinal
    if (filter(dataset_info, name == input$dataset_name_pwr)$longitudinal) {
      valid_mod_choices <- valid_mod_choices %>%
        keep(~.x != "mean_age_months")
    }

    checkboxGroupInput("pwr_moderators", label = "Moderators",
                       valid_mod_choices,
                       inline = TRUE)
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

    if (any(input$pwr_moderators == "response_mode")) {
      uis <- c(uis, list(
        selectInput("pwr_response_mode",
                    "Response mode",
                    choices = unique(pwrdata()$response_mode))
      ))
    }

    if (any(input$pwr_moderators == "exposure_phase")) {
      uis <- c(uis, list(
        selectInput("pwr_exposure_phase",
                    "Exposure phase",
                    choices = unique(pwrdata()$exposure_phase))
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

      if (any(input$pwr_moderators == "mean_age_months")) {
        req(input$pwr_age_months)
        newpred_mat <- c(newpred_mat, input$pwr_age_months)
      }

      if (any(input$pwr_moderators == "response_mode")) {
        req(input$pwr_response_mode)

        f_response_mode <- factor(pwrdata()$response_mode)
        n <- length(levels(f_response_mode))

        response_pred <- rep(0, n)
        pred_seq <- seq(1:n)[levels(f_response_mode) == input$pwr_response_mode]
        response_pred[pred_seq] <- 1

        # remove intercept
        response_pred <- response_pred[-1]

        newpred_mat <- c(newpred_mat, response_pred)
      }

      if (any(input$pwr_moderators == "exposure_phase")) {
        req(input$pwr_exposure_phase)

        f_exp_phase <- factor(pwrdata()$exposure_phase)
        n <- length(levels(f_exp_phase))

        exposure_pred <- rep(0, n)
        exposure_pred[seq(1:n)[levels(fep) == input$pwr_exposure_phase]] <- 1

        # remove intercept
        exposure_pred <- exposure_pred[-1]

        newpred_mat <- c(newpred_mat, exposure_pred)
      }

      predict(pwrmodel(), newmods = newpred_mat)$pred
    } else {
      # special case when there are no predictors, predict doesn't work
      pwrmodel()$b[,1][["intrcpt"]]
    }
  })

  pwr_80 <- reactive({
    pwr::pwr.p.test(h = d_pwr(),
                    sig.level = .05,
                    power = .8)$n
  })

  ## now do the actual power analysis plot
  output$power <- renderPlot({
    max_n <- min(max(60,
                     pwr::pwr.p.test(h = d_pwr(),
                                     sig.level = .05,
                                     power = .9)$n),
                 200)

    pwrs <- data.frame(ns = seq(5, max_n, 5),
                       ps = pwr::pwr.p.test(h = d_pwr(),
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
      xlab("Number of participants (N)")
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

  output$subset_selector <- renderUI({
    radioButtons("subset_input", "Subset", append(subsets(), "All data", 0))
  })

  output$subset_options <- reactive({
    subsets()
  })
  outputOptions(output, "subset_options", suspendWhenHidden = FALSE)

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
      color = "red"
    )
  })
})
