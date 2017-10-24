################################################################################
## CONSTANTS

pwrmu <- 10
pwrsd <- 5

################################################################################
## HELPER FUNCTIONS

sem <- function(x) {
  sd(x) / sqrt(length(x))
}

ci95.t <- function(x) {
  qt(.975, length(x) - 1) * sem(x)
}

pretty.p <- function(x) {
  as.character(signif(x, digits = 3))
}

shinyServer(function(input, output, session) {
  conds <- reactive({
    if (input$control) {
      groups <- factor(c("Experimental","Control",
                         levels = c("Experimental", "Control")))
    } else {
      groups <- factor("Experimental")
    }
    expand.grid(group = groups,
                condition = factor(c("Longer looking predicted",
                                     "Shorter looking predicted"))) %>%
      group_by(group, condition)
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

  # ########### GENERATE DATA #############
  pwr_sim_data <- reactive({
    if (input$go | !input$go) {
      conds() %>%
        do(data.frame(
          looking.time = ifelse(
            rep(.$group == "Experimental" &
                  .$condition == "Longer looking predicted", input$N),
            rnorm(n = input$N,
                  mean = pwrmu + (input$d_pwr * pwrsd) / 2,
                  sd = pwrsd),
            rnorm(n = input$N,
                  mean = pwrmu - (input$d_pwr * pwrsd) / 2,
                  sd = pwrsd)
          ),
          stringsAsFactors = FALSE
        )
        )
    }
  })

  # ########### MEANS FOR PLOTTING #############
  pwr_ms <- reactive({
    pwr_sim_data() %>%
      group_by(group, condition) %>%
      summarise(mean = mean(looking.time),
                ci = ci95.t(looking.time),
                sem = sem(looking.time)) %>%
      rename_("interval" = input$interval)
  })

  # ########### POWER ANALYSIS - BAR GRAPH #############
  output$pwr_bar <- renderPlot({
    req(input$d_pwr)

    pos <- position_dodge(width = .25)

    ggplot(pwr_ms(), aes(x = group, y = mean, fill = condition,
                         colour = condition)) +
      geom_bar(position = pos,
               stat = "identity",
               width = .25) +
      geom_linerange(data = pwr_ms(),
                     aes(x = group, y = mean,
                         fill = condition,
                         ymin = mean - interval,
                         ymax = mean + interval),
                     position = pos,
                     colour = "black") +
      xlab("Group") +
      ylab("Simulated Looking Time") +
      ylim(c(0,20)) +
      scale_fill_solarized(name = "",
                           labels = setNames(paste(pwr_ms()$condition, "  "),
                                             pwr_ms()$condition)) +
      scale_colour_solarized(name = "",
                             labels = setNames(paste(pwr_ms()$condition, "  "),
                                               pwr_ms()$condition))
  })

  # ########### POWER ANALYSIS - SCATTER PLOT #############
  output$pwr_scatter <- renderPlot({
    req(input$d_pwr)

    pos <- position_jitterdodge(jitter.width = .1,
                                dodge.width = .25)

    ggplot(pwr_sim_data(),
           aes(x = group, y = looking.time, fill = condition,
               colour = condition)) +
      geom_point(position = pos) +
      geom_linerange(data = pwr_ms(),
                     aes(x = group, y = mean,
                         fill = condition,
                         ymin = mean - interval,
                         ymax = mean + interval),
                     position = pos,
                     size = 2,
                     colour = "black") +
      xlab("Group") +
      ylab("Looking Time") +
      ylim(c(0, ceiling(max(pwr_sim_data()$looking.time) / 5) * 5))
  })

  # ########### STATISTICAL TEST OUTPUTS #############
  output$stat <- renderText({

    longer_exp <- filter(pwr_sim_data(),
                         condition == "Longer looking predicted",
                         group == "Experimental")$looking.time
    shorter_exp <- filter(pwr_sim_data(),
                          condition == "Shorter looking predicted",
                          group == "Experimental")$looking.time
    p.e <- t.test(longer_exp, shorter_exp, paired = TRUE)$p.value

    stat.text <- paste("A t test of the experimental condition is ",
                       ifelse(p.e > .05, "non", ""),
                       "significant at p = ",
                       pretty.p(p.e),
                       ". ",
                       sep = "")

    if (input$control) {

      longer_ctl <- filter(pwr_sim_data(),
                           condition == "Longer looking predicted",
                           group == "Control")$looking.time
      shorter_ctl <- filter(pwr_sim_data(),
                            condition == "Shorter looking predicted",
                            group == "Control")$looking.time
      p.c <- t.test(longer_ctl, shorter_ctl, paired = TRUE)$p.value

      a <- anova(lm(looking.time ~ group * condition, data = pwr_sim_data()))

      return(paste(stat.text,
                   "A t test of the control condition is ",
                   ifelse(p.c > .05, "non", ""),
                   "significant at p = ",
                   pretty.p(p.c),
                   ". An ANOVA ",
                   ifelse(a$"Pr(>F)"[3] < .05,
                          "shows a", "does not show a"),
                   " significant interaction at p = ",
                   pretty.p(a$"Pr(>F)"[3]),
                   ".",
                   sep = ""))
    }

    return(stat.text)
  })
})
