
# This file is a generated template, your changes will not be overwritten

mSRVClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mSRVClass",
    inherit = mSRVBase,
    private = list(
      .init = function() {
          # Initialization logic
          # Clear the Schoenfeld array state on start
          self$results$coxSection$schoenfeldPlots$setState(NULL)

          # Add comments to tables with bold keywords
          kmTable <- self$results$kmSection$kmSummaryTable
          kmTable$setNote('km_note', .('<b>Kaplan-Meier estimator</b> (Kaplan & Meier, 1958) is a non-parametric statistic used to estimate the survival function from lifetime data, accounting for censored observations. <b>Median survival</b> is the estimated time at which 50% of the subjects have experienced the event (survival probability drops to 0.5). <b>Lower/Upper 95% CI</b> represent the 95% confidence interval limits for this median survival time.'))

          lrTable <- self$results$logRankTable
          lrTable$setNote('lr_note', .('<b>Log-rank (Mantel-Cox) test</b> is a non-parametric hypothesis test that compares the survival distributions of two or more independent groups. It evaluates the null hypothesis that there is no difference in survival curves between groups. <b>Chi-square</b> is the test statistic based on observed vs. expected events, and the <b>p-value</b> indicates the statistical significance of the differences under the null hypothesis.'))

          fitTable <- self$results$coxSection$coxFitTable
          fitTable$setNote('fit_note', .('<b>Likelihood Ratio</b>, <b>Wald</b>, and <b>Score (Log-rank)</b> tests assess the global null hypothesis that all regression coefficients (beta) in the model are equal to zero. Statistically significant p-values (p < 0.05) indicate that the predictors collectively improve the model fit relative to a null model without covariates.'))

          coefTable <- self$results$coxSection$coxCoefTable
          coefTable$setNote('coef_note', .('<b>Hazard Ratio (HR)</b> represents the ratio of the hazard rates corresponding to the conditions described by two levels of an explanatory variable (Cox, 1972). It indicates the relative risk of event occurrence per unit change in the predictor. An <b>HR > 1</b> indicates an increased hazard (higher risk of the event), while an <b>HR < 1</b> indicates a decreased hazard (protective effect). <b>Lower/Upper 95% CI</b> represent the confidence interval for the Hazard Ratio.'))

          assumpTable <- self$results$coxSection$coxAssumpTable
          assumpTable$setNote('assump_note', .('<b>Schoenfeld residuals test</b> evaluates the proportional hazards (PH) assumption of the Cox model for each covariate individually and globally. It tests whether the effect of the covariates is constant over time. A statistically significant <b>p-value (p < 0.05)</b> indicates a violation of the PH assumption, meaning the hazard ratio changes over time.'))

          rocTable <- self$results$rocSection$rocTable
          rocTable$setNote('roc_note', .('<b>Time-dependent ROC</b> analysis (Heagerty et al., 2000) evaluates the predictive accuracy of continuous markers for time-to-event outcomes at a specific time point <i>t</i>, classifying patients into event-occurring or event-free states. <b>AUC</b> (Area Under the Curve) measures the discriminative ability (ranging from 0.5 to 1.0). <b>Cut-off</b> represents the optimal threshold determined by the Youden index (J = Sensitivity + Specificity - 1).'))
      },

      .run = function() {
          if (is.null(self$options$elapsed) || is.null(self$options$status)) {
              return()
          }

          dat <- data.frame(self$data, check.names=FALSE)
          private$.errorCheck(dat)

          # Prepare clean data
          time_var <- self$options$elapsed
          status_var <- self$options$status
          group_var <- self$options$group
          covs <- self$options$covariates

          # Handle status variable and map event level
          status_col <- dat[[status_var]]
          event_indicator <- NULL

          # Convert character to factor for consistent level handling
          if (is.character(status_col)) {
              status_col <- as.factor(status_col)
          }

          if (is.factor(status_col)) {
              event_level <- self$options$statusEvent
              if (is.null(event_level) || event_level == "") {
                  levels_status <- levels(status_col)
                  if (length(levels_status) >= 2) event_level <- levels_status[2]
                  else event_level <- levels_status[1]
              }
              event_indicator <- as.numeric(status_col == event_level)
          } else {
              event_level <- self$options$statusEvent
              if (!is.null(event_level) && event_level != "") {
                  event_indicator <- as.numeric(as.character(status_col) == event_level)
              } else {
                  # If event_level is not specified for a numeric status column:
                  # For 0/1, 1 is the event. For 1/2, 2 is the event.
                  # We treat the maximum unique value as the event.
                  vals <- unique(stats::na.omit(status_col))
                  if (length(vals) == 2) {
                      event_val <- max(vals)
                      event_indicator <- as.numeric(status_col == event_val)
                  } else {
                      # Fallback if only 1 value or more than 2 values
                      event_indicator <- as.numeric(status_col == 1)
                  }
              }
          }

          dat$..time <- as.numeric(dat[[time_var]])
          dat$..event <- event_indicator

          # ----------------------------------------------------
          # 1. Kaplan-Meier Survival Summary
          # ----------------------------------------------------
          if (!is.null(group_var)) {
              dat$..group <- as.factor(dat[[group_var]])
              dat_km <- dat[!is.na(dat$..time) & !is.na(dat$..event) & !is.na(dat$..group), ]
              formula_km <- survival::Surv(..time, ..event) ~ ..group
          } else {
              dat_km <- dat[!is.na(dat$..time) & !is.na(dat$..event), ]
              formula_km <- survival::Surv(..time, ..event) ~ 1
          }

          if (self$options$showKm && nrow(dat_km) > 0) {
              # Store state for plot
              self$results$kmSection$kmPlot$setState(list(data = dat_km))

              km_fit <- try(survival::survfit(formula_km, data = dat_km), silent = TRUE)
              if (!inherits(km_fit, "try-error")) {
                  kmTable <- self$results$kmSection$kmSummaryTable
                  kmTable$deleteRows()
                  sum_km <- summary(km_fit)$table

                  get_val <- function(vec, names_to_try) {
                      for (name in names_to_try) {
                          if (name %in% names(vec)) return(as.numeric(vec[name]))
                      }
                      return(NA_real_)
                  }

                  if (!is.null(group_var)) {
                      if (is.matrix(sum_km)) {
                          for (i in 1:nrow(sum_km)) {
                              row_name <- rownames(sum_km)[i]
                              clean_group <- gsub("^\\.\\.group=", "", row_name)
                              vec <- sum_km[i, ]
                              kmTable$addRow(rowKey=row_name, values=list(
                                  group  = clean_group,
                                  n      = as.integer(get_val(vec, c("records", "n"))),
                                  events = as.integer(get_val(vec, c("events"))),
                                  median = get_val(vec, c("median")),
                                  lower  = get_val(vec, c("0.95LCL", "lower")),
                                  upper  = get_val(vec, c("0.95UCL", "upper"))
                              ))
                          }
                      } else {
                          # Single group after dropping missing/empty levels
                          row_name <- "All"
                          kmTable$addRow(rowKey=row_name, values=list(
                              group  = row_name,
                              n      = as.integer(get_val(sum_km, c("records", "n"))),
                              events = as.integer(get_val(sum_km, c("events"))),
                              median = get_val(sum_km, c("median")),
                              lower  = get_val(sum_km, c("0.95LCL", "lower")),
                              upper  = get_val(sum_km, c("0.95UCL", "upper"))
                          ))
                      }
                  } else {
                      kmTable$addRow(rowKey="All", values=list(
                          group  = "All",
                          n      = as.integer(get_val(sum_km, c("records", "n"))),
                          events = as.integer(get_val(sum_km, c("events"))),
                          median = get_val(sum_km, c("median")),
                          lower  = get_val(sum_km, c("0.95LCL", "lower")),
                          upper  = get_val(sum_km, c("0.95UCL", "upper"))
                      ))
                  }

                  # Populate kmRiskTable if requested
                  kmRiskTable <- self$results$kmSection$kmRiskTable
                  kmRiskTable$deleteRows()
                  if (self$options$kmTable) {
                      max_time <- max(dat_km$..time, na.rm = TRUE)
                      time_points <- pretty(c(0, max_time))
                      time_points <- time_points[time_points <= max_time]
                      if (length(time_points) == 0) {
                          time_points <- c(0)
                      }
                      sum_risk <- try(summary(km_fit, times = time_points, extend = TRUE), silent = TRUE)
                      if (!inherits(sum_risk, "try-error")) {
                          # Compute cumulative events per stratum
                          events <- sum_risk$n.event
                          if (!is.null(group_var) && !is.null(sum_risk$strata)) {
                              strata <- sum_risk$strata
                              cum_ev <- numeric(length(events))
                              for (st in unique(strata)) {
                                  idx <- which(strata == st)
                                  cum_ev[idx] <- cumsum(events[idx])
                              }
                          } else {
                              cum_ev <- cumsum(events)
                          }

                          for (i in seq_along(sum_risk$time)) {
                              grp_val <- "All"
                              if (!is.null(group_var) && !is.null(sum_risk$strata)) {
                                  grp_val <- gsub("^\\.\\.group=", "", as.character(sum_risk$strata[i]))
                              }
                              kmRiskTable$addRow(rowKey = as.character(i), values = list(
                                  group = grp_val,
                                  time = sum_risk$time[i],
                                  nAtRisk = as.integer(sum_risk$n.risk[i]),
                                  nEvent = as.integer(cum_ev[i])
                              ))
                          }
                      }
                  }
              }
          }

          # ----------------------------------------------------
          # 2. Log-rank Test
          # ----------------------------------------------------
          if (self$options$showLogRank && !is.null(group_var) && nrow(dat_km) > 0) {
              ug <- unique(dat_km$..group)
              if (length(ug) >= 2) {
                  sdiff <- try(survival::survdiff(survival::Surv(..time, ..event) ~ ..group, data = dat_km), silent = TRUE)
                  if (!inherits(sdiff, "try-error")) {
                      df <- length(sdiff$n) - 1
                      chisq <- sdiff$chisq
                      pval <- 1 - stats::pchisq(chisq, df)

                      logRankTable <- self$results$logRankTable
                      logRankTable$deleteRows()
                      logRankTable$addRow(rowKey="test", values=list(
                          chisq = chisq,
                          df    = df,
                          p     = pval
                      ))
                  }
              }
          }

          # ----------------------------------------------------
          # 3. Cox Proportional Hazards Regression
          # ----------------------------------------------------
          if (self$options$showCox) {
              terms <- c()
              if (!is.null(group_var)) {
                  dat$..group <- as.factor(dat[[group_var]])
                  terms <- c(terms, "..group")
              }
              if (length(covs) > 0) {
                  for (i in seq_along(covs)) {
                      cov_name <- covs[i]
                      col_val <- dat[[cov_name]]
                      if (is.factor(col_val) || is.character(col_val)) {
                          dat[[paste0("..cov_", i)]] <- as.factor(col_val)
                      } else {
                          dat[[paste0("..cov_", i)]] <- as.numeric(col_val)
                      }
                      terms <- c(terms, paste0("..cov_", i))
                  }
              }

              if (length(terms) > 0) {
                  # Drop NAs
                  cols_to_check <- c("..time", "..event", if (!is.null(group_var)) "..group" else NULL)
                  if (length(covs) > 0) {
                      for (i in seq_along(covs)) {
                          cols_to_check <- c(cols_to_check, paste0("..cov_", i))
                      }
                  }
                  dat_cox <- dat[stats::complete.cases(dat[, cols_to_check, drop=FALSE]), ]

                  if (nrow(dat_cox) > 0) {
                      formula_cox <- paste("survival::Surv(..time, ..event) ~", paste(terms, collapse = " + "))
                      cox_fit <- try(survival::coxph(as.formula(formula_cox), data = dat_cox), silent = TRUE)

                      if (!inherits(cox_fit, "try-error")) {
                          # Save state for forest plot
                          self$results$coxSection$coxForestPlot$setState(list(fit = cox_fit, terms = terms))

                          # Model fit summaries
                          sum_cox <- summary(cox_fit)
                          fitTable <- self$results$coxSection$coxFitTable
                          fitTable$deleteRows()

                          if (!is.null(sum_cox$logtest)) {
                              fitTable$addRow(rowKey="lr", values=list(
                                  test = "Likelihood Ratio Test",
                                  stat = as.numeric(sum_cox$logtest["test"]),
                                  df   = as.integer(sum_cox$logtest["df"]),
                                  p    = as.numeric(sum_cox$logtest["pvalue"])
                              ))
                          }
                          if (!is.null(sum_cox$waldtest)) {
                              fitTable$addRow(rowKey="wald", values=list(
                                  test = "Wald Test",
                                  stat = as.numeric(sum_cox$waldtest["test"]),
                                  df   = as.integer(sum_cox$waldtest["df"]),
                                  p    = as.numeric(sum_cox$waldtest["pvalue"])
                              ))
                          }
                          if (!is.null(sum_cox$sctest)) {
                              fitTable$addRow(rowKey="score", values=list(
                                  test = "Score (Log-rank) Test",
                                  stat = as.numeric(sum_cox$sctest["test"]),
                                  df   = as.integer(sum_cox$sctest["df"]),
                                  p    = as.numeric(sum_cox$sctest["pvalue"])
                              ))
                          }

                          # Coefficients
                          coefTable <- self$results$coxSection$coxCoefTable
                          coefTable$deleteRows()

                          coefs <- sum_cox$coefficients
                          conf_int <- sum_cox$conf.int

                          if (!is.null(coefs)) {
                              row_names <- rownames(coefs)
                              for (i in seq_along(row_names)) {
                                  raw_name <- row_names[i]
                                  clean_name <- raw_name
                                  if (grepl("^\\.\\.group", raw_name)) {
                                      lvl <- gsub("^\\.\\.group", "", raw_name)
                                      clean_name <- paste0(group_var, " (", lvl, ")")
                                  } else if (grepl("^\\.\\.cov_([0-9]+)", raw_name)) {
                                      match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", raw_name))
                                      rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx), "", raw_name)
                                      cov_name <- covs[match_idx]
                                      clean_name <- paste0(cov_name, rem_suffix)
                                  }

                                  coefTable$addRow(rowKey=raw_name, values=list(
                                      var     = clean_name,
                                      coef    = as.numeric(coefs[i, "coef"]),
                                      se      = as.numeric(coefs[i, "se(coef)"]),
                                      z       = as.numeric(coefs[i, "z"]),
                                      p       = as.numeric(coefs[i, "Pr(>|z|)"]),
                                      hr      = as.numeric(conf_int[i, "exp(coef)"]),
                                      hrLower = as.numeric(conf_int[i, "lower .95"]),
                                      hrUpper = as.numeric(conf_int[i, "upper .95"])
                                  ))
                              }
                          }

                          # Proportionality check
                          if (self$options$coxAssump) {
                              zph <- try(survival::cox.zph(cox_fit), silent = TRUE)
                              if (!inherits(zph, "try-error")) {
                                  assumpTable <- self$results$coxSection$coxAssumpTable
                                  assumpTable$deleteRows()

                                  zph_table <- zph$table
                                  zph_rows <- rownames(zph_table)

                                  # Save state for individual plots
                                  self$results$coxSection$schoenfeldPlots$setState(list(zph = zph, row_names = zph_rows))

                                  schoenfeldPlots <- self$results$coxSection$schoenfeldPlots
                                  schoenfeldPlots$clear()
                                  plot_vars <- c()
                                  if (self$options$showSchoenfeldPlot) {
                                      plot_vars <- zph_rows[zph_rows != "GLOBAL"]
                                  }
                                  for (pv in plot_vars) {
                                      schoenfeldPlots$addItem(key = pv)
                                  }

                                  if (length(plot_vars) > 0) {
                                      for (pv in plot_vars) {
                                          clean_pv <- pv
                                          if (grepl("^\\.\\.group", pv)) {
                                              lvl <- gsub("^\\.\\.group", "", pv)
                                              clean_pv <- paste0(group_var, " (", lvl, ")")
                                          } else if (grepl("^\\.\\.cov_([0-9]+)", pv)) {
                                              match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", pv))
                                              rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx), "", pv)
                                              cov_name <- covs[match_idx]
                                              clean_pv <- paste0(cov_name, rem_suffix)
                                          }
                                          schoenfeldPlots$get(key=pv)$setTitle(paste("Schoenfeld residuals -", clean_pv))
                                      }
                                  }

                                  for (i in seq_along(zph_rows)) {
                                      row_name <- zph_rows[i]
                                      clean_name <- row_name
                                      if (row_name == "GLOBAL") {
                                          clean_name <- "GLOBAL"
                                      } else if (grepl("^\\.\\.group", row_name)) {
                                          lvl <- gsub("^\\.\\.group", "", row_name)
                                          clean_name <- paste0(group_var, " (", lvl, ")")
                                      } else if (grepl("^\\.\\.cov_([0-9]+)", row_name)) {
                                          match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", row_name))
                                          rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx), "", row_name)
                                          cov_name <- covs[match_idx]
                                          clean_name <- paste0(cov_name, rem_suffix)
                                      }

                                      assumpTable$addRow(rowKey=row_name, values=list(
                                          var    = clean_name,
                                          chisq  = as.numeric(zph_table[i, "chisq"]),
                                          df     = as.integer(zph_table[i, "df"]),
                                          p      = as.numeric(zph_table[i, "p"])
                                      ))
                                  }
                              }
                          }
                      }
                  }
              }
          }

          # ----------------------------------------------------
          # 4. Time-dependent ROC Analysis
          # ----------------------------------------------------
          roc_preds <- self$options$rocPredictors
          if (self$options$showRoc && length(roc_preds) > 0) {
              rocTable <- self$results$rocSection$rocTable
              rocTable$deleteRows()

              roc_time <- as.numeric(self$options$rocTime)
              roc_plot_list <- list()

              # Data check
              cols_to_check_roc <- c("..time", "..event", roc_preds)
              dat_roc <- dat[stats::complete.cases(dat[, cols_to_check_roc, drop=FALSE]), ]

              if (nrow(dat_roc) > 0) {
                  for (pred in roc_preds) {
                      pred_col <- dat_roc[[pred]]
                      time_col <- dat_roc$..time
                      ev_col   <- dat_roc$..event

                      class_val <- rep(NA_integer_, length(time_col))
                      # Case: Event occurred at or before t
                      class_val[time_col <= roc_time & ev_col == 1] <- 1
                      # Control: Survived past t
                      class_val[time_col > roc_time] <- 0

                      valid_idx <- !is.na(class_val)
                      if (sum(valid_idx & class_val == 1) >= 1 && sum(valid_idx & class_val == 0) >= 1) {
                          sub_class <- class_val[valid_idx]
                          sub_pred  <- pred_col[valid_idx]

                          roc_obj <- try(pROC::roc(response = sub_class, predictor = sub_pred, quiet = TRUE), silent = TRUE)
                          if (!inherits(roc_obj, "try-error")) {
                              ci_width <- self$options$rocWidth / 100
                              auc_ci <- try(pROC::ci.auc(roc_obj, conf.level = ci_width), silent = TRUE)
                              auc_lower <- if (inherits(auc_ci, "try-error")) NA else auc_ci[1]
                              auc_upper <- if (inherits(auc_ci, "try-error")) NA else auc_ci[3]

                              coords <- try(pROC::coords(roc_obj, "best", best.method = "youden",
                                                         ret = c("threshold", "sensitivity", "specificity")), silent = TRUE)

                              cutoff <- NA; se <- NA; sp <- NA
                              if (!is.null(coords) && !inherits(coords, "try-error")) {
                                  if (is.data.frame(coords) || is.matrix(coords)) {
                                      cutoff <- as.numeric(coords[1, "threshold"])
                                      se     <- as.numeric(coords[1, "sensitivity"])
                                      sp     <- as.numeric(coords[1, "specificity"])
                                  } else {
                                      cutoff <- as.numeric(coords["threshold"])
                                      se     <- as.numeric(coords["sensitivity"])
                                      sp     <- as.numeric(coords["specificity"])
                                  }
                              }

                              rocTable$addRow(rowKey=pred, values=list(
                                  predictor = pred,
                                  time      = roc_time,
                                  auc       = as.numeric(roc_obj$auc),
                                  aucLower  = auc_lower,
                                  aucUpper  = auc_upper,
                                  cutoff    = cutoff,
                                  se        = se,
                                  sp        = sp
                              ))

                              roc_plot_list[[pred]] <- list(
                                  roc    = roc_obj,
                                  pred   = pred,
                                  time   = roc_time,
                                  cutoff = cutoff
                              )
                          }
                      }
                  }
                  self$results$rocSection$rocPlot$setState(roc_plot_list)
              }

              # Set warning note if table is empty
              if (self$results$rocSection$rocTable$rowCount == 0) {
                  self$results$rocSection$rocTable$setNote('empty_warning',
                      .('Not enough events or survivors at the selected analysis time point t to perform ROC analysis.'))
              } else {
                  self$results$rocSection$rocTable$setNote('empty_warning', NULL)
              }
          }
      },

      .kmPlot = function(image, ggtheme, theme, ...) {
          state <- image$state
          if (is.null(state)) return(FALSE)

          dat_plot <- state$data
          if (is.null(dat_plot) || nrow(dat_plot) == 0) return(FALSE)

          group_var <- self$options$group
          if (!is.null(group_var)) {
              formula <- survival::Surv(..time, ..event) ~ ..group
          } else {
              formula <- survival::Surv(..time, ..event) ~ 1
          }

          fit <- try(ggsurvfit::survfit2(formula, data = dat_plot), silent = TRUE)
          if (inherits(fit, "try-error")) return(FALSE)

          km_type <- self$options$kmType
          if (km_type == "hazard") km_type <- "cumhaz"

          p <- try({
              ggsurvfit::ggsurvfit(fit, type = km_type) +
                  ggplot2::labs(
                      x = self$options$elapsed,
                      y = ifelse(km_type == "survival", .("Survival probability"), .("Cumulative hazard"))
                  )
          }, silent = TRUE)

          if (inherits(p, "try-error")) return(FALSE)

          if (self$options$kmCI) {
              p <- p + ggsurvfit::add_confidence_interval()
          }

          # Risk table is moved to a separate jamovi table
          # if (self$options$kmTable) {
          #     p <- p + ggsurvfit::add_risktable()
          # }

          pal <- self$options$palBrewer
          if (pal != "none" && !is.null(group_var)) {
              n_levs <- length(levels(dat_plot$..group))
              cols <- try(RColorBrewer::brewer.pal(n = max(3, n_levs), name = pal)[1:n_levs], silent = TRUE)
              if (!inherits(cols, "try-error") && !any(is.na(cols))) {
                  p <- p + ggplot2::scale_color_manual(values = cols) + ggplot2::scale_fill_manual(values = cols)
              }
          }

          p <- p + ggtheme + ggplot2::theme(legend.position = "bottom")
          print(p)
          return(TRUE)
      },

      .coxForestPlot = function(image, ggtheme, theme, ...) {
          state <- image$state
          if (is.null(state)) return(FALSE)

          fit <- state$fit
          terms <- state$terms
          if (is.null(fit) || is.null(terms)) return(FALSE)

          sum_cox <- summary(fit)
          coef_df <- as.data.frame(sum_cox$conf.int)
          if (nrow(coef_df) == 0) return(FALSE)

          coef_df$raw_var <- rownames(coef_df)
          coef_df$coef <- sum_cox$coefficients[, "coef"]
          coef_df$se <- sum_cox$coefficients[, "se(coef)"]

          coef_df$var <- coef_df$raw_var
          group_var <- self$options$group
          covs <- self$options$covariates

          for (i in seq_along(coef_df$raw_var)) {
              rv <- coef_df$raw_var[i]
              if (grepl("^\\.\\.group", rv)) {
                  lvl <- gsub("^\\.\\.group", "", rv)
                  coef_df$var[i] <- paste0(group_var, " (", lvl, ")")
              } else if (grepl("^\\.\\.cov_([0-9]+)", rv)) {
                  match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", rv))
                  rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx), "", rv)
                  cov_name <- covs[match_idx]
                  coef_df$var[i] <- paste0(cov_name, rem_suffix)
              }
          }

          p <- ggplot2::ggplot(coef_df, ggplot2::aes(x = stats::reorder(var, coef), y = `exp(coef)`)) +
              ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
              ggplot2::geom_errorbar(ggplot2::aes(ymin = `lower .95`, ymax = `upper .95`), width = 0.2, color = "#3366B2", linewidth = 0.8) +
              ggplot2::geom_point(color = "#3366B2", size = 3) +
              ggplot2::coord_flip() +
              ggplot2::labs(
                  title = .("Hazard Ratios (95% CI)"),
                  x = .("Covariates"),
                  y = .("Hazard Ratio")
              ) +
              ggtheme

          print(p)
          return(TRUE)
      },

      .schoenfeldPlot = function(image, ggtheme, theme, ...) {
          state <- image$parent$state
          if (is.null(state)) return(FALSE)

          zph <- state$zph
          if (is.null(zph)) return(FALSE)

          var_name <- image$key
          if (is.null(var_name) || !(var_name %in% rownames(zph$table))) return(FALSE)

          var_idx <- which(rownames(zph$table) == var_name)
          if (length(var_idx) == 0) return(FALSE)

          # Plot base R plot.cox.zph
          plot(zph[var_idx], resid = TRUE, se = TRUE, df = 4, nsmo = 40, col = "#3366B2", lwd = 2)
          abline(h = 0, col = "#E54028", lty = 2, lwd = 1.5)

          return(TRUE)
      },

      .rocPlot = function(image, ggtheme, theme, ...) {
          roc_list <- image$state
          if (is.null(roc_list) || length(roc_list) == 0) return(FALSE)

          # Set explicit margins to ensure proper spacing and prevent title overlap
          op <- par(mar = c(5, 5, 4.5, 2))
          on.exit(par(op))

          n_preds <- length(roc_list)
          pal <- self$options$palBrewer
          cols <- jmvcore::colorPalette(n = n_preds, theme$palette, type = "fill")
          if (pal != "none") {
              cols <- RColorBrewer::brewer.pal(n = max(3, n_preds), name = pal)[1:n_preds]
          }

          add <- FALSE
          for (i in seq_along(roc_list)) {
              item <- roc_list[[i]]
              roc_obj <- item$roc
              cutoff <- item$cutoff
              has_cutoff <- !is.null(cutoff) && !is.na(cutoff)

              if (self$options$rocCI && has_cutoff) {
                  conf_level <- self$options$rocWidth / 100
                  # Pre-compute threshold CI and attach it to the roc object
                  roc_obj$ci <- try(pROC::ci.thresholds(roc_obj, thresholds = cutoff, conf.level = conf_level), silent = TRUE)
                  if (inherits(roc_obj$ci, "try-error")) {
                      roc_obj$ci <- NULL
                  }
              } else {
                  roc_obj$ci <- NULL
              }

              pROC::plot.roc(
                  roc_obj,
                  col = cols[i],
                  add = add,
                  main = if (!add) jmvcore::format(.("Time-dependent ROC Curves (t = {time})"), time = roc_list[[1]]$time) else NULL,
                  cex.main = 1.3,
                  cex.lab = 1.4,
                  cex.axis = 1.4,
                  lwd = 3,
                  legacy.axes = TRUE,
                  xlab = .("1 - Specificity"),
                  ylab = .("Sensitivity"),
                  grid = !add,
                  print.thres = has_cutoff,
                  print.thres.col = cols[i],
                  print.thres.pch = 19,
                  print.thres.cex = 1.3,
                  print.thres.pattern = "%.2f (%.3f, %.3f)",
                  print.thres.best.method = "youden",
                  ci = (self$options$rocCI && has_cutoff && !is.null(roc_obj$ci)),
                  ci.col = cols[i],
                  ci.type = "bars"
              )
              add <- TRUE
          }

          leg_labels <- sapply(roc_list, function(x) paste0(x$pred, " (AUC = ", sprintf("%.3f", x$roc$auc), ")"))
          legend(
              "bottomright",
              legend = leg_labels,
              col = cols,
              lwd = 3,
              cex = 1.0,
              bty = "o",
              bg = "white"
          )

          return(TRUE)
      },

      .errorCheck = function(dat) {
          # Verification helper
          time_var <- self$options$elapsed
          status_var <- self$options$status

          if (!is.numeric(dat[[time_var]])) {
              jmvcore::reject(.("Time variable must be numeric."))
          }

          status_col <- dat[[status_var]]
          if (!is.numeric(status_col) && !is.factor(status_col) && !is.character(status_col)) {
              jmvcore::reject(.("Status variable must be factor, character, or numeric."))
          }
      }
    )
)
