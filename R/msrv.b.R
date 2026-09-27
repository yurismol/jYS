
# This file is a generated template, your changes will not be overwritten

mSRVClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mSRVClass",
    inherit = mSRVBase,
    public = list(
        .savePart = function(path, part, ...) {
            smart_lookup <- function(results, p_str, options = NULL) {
                if (is.null(results) || is.null(p_str)) return(NULL)
                if (length(p_str) > 1) p_str <- paste(p_str, collapse = "/")
                if (!nzchar(p_str)) return(NULL)

                covs <- tryCatch(options$covariates, error = function(e) NULL)
                group_var <- tryCatch(options$group, error = function(e) NULL)
                vars <- tryCatch(options$vars, error = function(e) NULL)

                find_child <- function(curr, seg) {
                    if (is.null(curr) || is.null(seg) || !nzchar(seg)) return(NULL)
                    clean <- gsub('^["\']|["\']$', '', seg)

                    if (inherits(curr, 'Array')) {
                        nxt <- tryCatch(curr$get(key = clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)

                        nxt <- tryCatch(curr$get(name = paste0('"', clean, '"')), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(name = seg), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(name = clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)

                        items <- tryCatch(curr$items, error = function(e) NULL)
                        if (!is.null(items) && length(items) > 0) {
                            # 1. Exact match by key, name, or title
                            for (it in items) {
                                it_key <- tryCatch(it$key, error = function(e) NULL)
                                it_name <- tryCatch(it$name, error = function(e) NULL)
                                it_title <- tryCatch(it$title, error = function(e) NULL)
                                if (identical(it_key, clean) || identical(it_key, seg) ||
                                    identical(it_name, clean) || identical(it_name, seg) ||
                                    identical(it_title, clean) || identical(it_title, seg)) {
                                    return(it)
                                }
                            }

                            # 2. Match covariate by index if clean is in covs or group_var
                            if (!is.null(covs) && clean %in% covs) {
                                c_idx <- which(covs == clean)
                                target_key <- paste0("..cov_", c_idx, "_")
                                for (it in items) {
                                    if (identical(it$key, target_key) || grepl(paste0("^\\.\\.cov_", c_idx), as.character(it$key))) {
                                        return(it)
                                    }
                                }
                            }
                            if (!is.null(group_var) && (clean == group_var || grepl(paste0("^", clean), group_var))) {
                                for (it in items) {
                                    if (identical(it$key, "..group") || grepl("^\\.\\.group", as.character(it$key))) {
                                        return(it)
                                    }
                                }
                            }

                            # 3. Match suffix of title after dash
                            for (it in items) {
                                it_title <- tryCatch(as.character(it$title), error = function(e) "")
                                title_var <- trimws(sub(".*[\u2013\u2014-]\\s*", "", it_title))
                                title_var_clean <- gsub("[_()]", "", title_var)
                                clean_nopunct <- gsub("[_()]", "", clean)
                                if (nzchar(title_var) && (identical(title_var, clean) || identical(title_var_clean, clean_nopunct))) {
                                    return(it)
                                }
                            }

                            # 4. Word boundary match in title
                            for (it in items) {
                                it_title <- tryCatch(as.character(it$title), error = function(e) "")
                                pattern <- paste0("(^|[^a-zA-Z0-9_])", clean, "([^a-zA-Z0-9_]|$)")
                                if (grepl(pattern, it_title)) {
                                    return(it)
                                }
                            }

                            # 5. Check integer index
                            idx <- suppressWarnings(as.integer(clean))
                            if (!is.na(idx)) {
                                items_len <- length(items)
                                if (idx == 0 && items_len >= 1) return(items[[1]])
                                if (idx >= 1 && idx <= items_len) return(items[[idx]])
                            }
                        }
                    }

                    nxt <- tryCatch(curr$.lookup(seg), error = function(e) NULL)
                    if (!is.null(nxt)) return(nxt)
                    if (seg != clean) {
                        nxt <- tryCatch(curr$.lookup(clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                    }

                    if (inherits(curr, 'Group')) {
                        nxt <- tryCatch(curr$get(clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(seg), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                    }

                    items <- tryCatch(curr$items, error = function(e) NULL)
                    if (!is.null(items) && length(items) > 0) {
                        if (clean %in% names(items)) return(items[[clean]])
                        if (seg %in% names(items)) return(items[[seg]])
                        for (it in items) {
                            it_name <- tryCatch(it$name, error = function(e) NULL)
                            it_key <- tryCatch(it$key, error = function(e) NULL)
                            it_title <- tryCatch(it$title, error = function(e) NULL)
                            if (identical(it_name, clean) || identical(it_name, seg) ||
                                identical(it_key, clean) || identical(it_key, seg)) {
                                return(it)
                            }
                        }
                    }
                    NULL
                }

                traverse <- function(node, segs) {
                    if (is.null(node) || length(segs) == 0) return(node)
                    child <- find_child(node, segs[1])
                    if (!is.null(child)) {
                        return(traverse(child, segs[-1]))
                    }
                    NULL
                }

                search_recursive <- function(node, segs) {
                    if (is.null(node)) return(NULL)
                    res <- traverse(node, segs)
                    if (!is.null(res)) return(res)

                    items <- tryCatch(node$items, error = function(e) NULL)
                    if (!is.null(items) && length(items) > 0) {
                        for (child in items) {
                            if (inherits(child, 'Group') || inherits(child, 'Array')) {
                                res <- search_recursive(child, segs)
                                if (!is.null(res)) return(res)
                            }
                        }
                    }
                    NULL
                }

                norm_p <- gsub('\\[[\'"]?([^\'"]+?)[\'"]?\\]', '/\\1', p_str)
                parts <- strsplit(norm_p, '/+', perl = TRUE)[[1]]
                parts <- parts[parts != ""]
                if (length(parts) == 0) return(NULL)

                # Strip analysisId or 'results' prefix if present
                if (length(parts) > 1 && (parts[1] == "results" || !is.na(suppressWarnings(as.integer(parts[1]))))) {
                    if (is.null(find_child(results, parts[1]))) {
                        parts <- parts[-1]
                    }
                }

                target <- search_recursive(results, parts)
                if (inherits(target, 'Array') && length(target$items) > 0) {
                    target <- target$items[[1]]
                }
                target
            }

            element <- smart_lookup(self$results, part, self$options)
            if (is.null(element)) {
                return(FALSE)
            }

            if (inherits(element, 'Array')) {
                if (length(element$items) > 0) {
                    element <- element$items[[1]]
                } else {
                    return(FALSE)
                }
            }

            if (inherits(element, 'Image')) {
                if (is.null(element$state) && !is.null(element$parent$state)) {
                    parent_state <- element$parent$state
                    if (!is.null(parent_state$zph)) {
                        var_name <- element$key
                        var_idx <- if (!is.null(var_name) && var_name %in% rownames(parent_state$zph$table)) {
                            which(rownames(parent_state$zph$table) == var_name)
                        } else 1
                        element$setState(list(zph = parent_state$zph, var_idx = var_idx, var_name = var_name))
                    } else if (!is.null(element$key) && !is.null(parent_state[[element$key]])) {
                        element$setState(parent_state[[element$key]])
                    } else if (is.list(parent_state) && length(parent_state) > 0) {
                        element$setState(parent_state[[1]])
                    }
                }
            }

            if (element$requiresData && is.function(private$.ensureData)) {
                private$.ensureData()
            }
            save_ok <- tryCatch({
                element$saveAs(path, ...)
                file.exists(path) && file.info(path)$size > 0
            }, error = function(e) {
                tryCatch({
                    element$saveAs(path)
                    file.exists(path) && file.info(path)$size > 0
                }, error = function(e2) {
                    FALSE
                })
            })
            return(save_ok)
        }
    ),
    private = list(
      .km_sig = NULL,
      .lr_sig = NULL,
      .cox_model_sig = NULL,
      .cached_cox_fit = NULL,
      .cached_cox_terms = NULL,
      .cached_zph = NULL,
      .cached_zph_rows = NULL,
      .timedep_sig = NULL,
      .adj_sig = NULL,
      .roc_sig = NULL,
      .cr_sig = NULL,

      .ensureData = function() {
          if (is.null(private$.data) || nrow(private$.data) == 0) {
              d <- tryCatch(self$readDataset(headerOnly = FALSE), error = function(e) NULL)
              if (!is.null(d) && nrow(d) > 0)
                  private$.data <- d
          }
          return(!is.null(private$.data) && nrow(private$.data) > 0)
      },

      .init = function() {
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

          adjSummaryTable <- self$results$adjSection$adjSummaryTable
          adjSummaryTable$setNote('adj_note', .('<b>Direct adjustment (G-computation)</b> calculates counterfactual survival curves for each exposure level by averaging individual predicted survival curves over the empirical covariate distribution of the sample (Denz et al., 2023). <b>RMST</b> represents the Restricted Mean Survival Time evaluated up to \u03c4 (area under the adjusted survival curve).'))

          cifSummaryTable <- self$results$compRisksSection$cifSummaryTable
          cifSummaryTable$setNote('cif_note', .('<b>Cumulative Incidence Function (CIF)</b> estimated via the non-parametric Aalen–Johansen estimator (Aalen & Johansen, 1978) calculates the probability of experiencing a specific event over time in the presence of competing risks. Unlike Kaplan–Meier survival (1 – KM), which overestimates risk by treating competing events as censored, CIF properly accounts for competing failure types.'))

          grayTestTable <- self$results$compRisksSection$grayTestTable
          grayTestTable$setNote('gray_note', .("<b>Gray's test</b> (Gray, 1988) is a non-parametric K-sample test for comparing cumulative incidence functions of a specific cause between groups. It evaluates subdistribution hazards under competing risks."))

          fineGrayTable <- self$results$compRisksSection$fineGrayTable
          fineGrayTable$setNote('fg_note', .('<b>Fine–Gray proportional subdistribution hazards regression</b> (Fine & Gray, 1999) models the effect of covariates directly on the cumulative incidence function of the event of interest. <b>sHR</b> (subdistribution Hazard Ratio) represents the relative change in the subdistribution hazard. Robust standard errors are used to account for inverse-probability weighting.'))

          rocTable <- self$results$rocSection$rocTable
          rocTable$setNote('roc_note', .('<b>Time-dependent ROC</b> analysis evaluates the predictive accuracy of continuous markers for time-to-event outcomes via landmark dichotomization at a specific time point <i>t</i> (subjects censored prior to <i>t</i> are excluded). <b>AUC</b> measures the discriminative ability. <b>Cut-off</b> represents the optimal threshold determined by the Youden index (J = Sensitivity + Specificity – 1).'))

          timeDepTable <- self$results$coxSection$timeDepTable
          timeDepTable$setNote('timedep_note', .('<b>Time-dependent Cox regression</b> models non-proportional hazards by interacting covariates with a function of time (Therneau & Grambsch, 2000). The hazard function is modeled as \u03bb(t; z) = \u03bb\u2080(t) exp(\u03b2\u2080 z + \u03b2_td [z \u00d7 f(t)]). A statistically significant time interaction (\u03b2_td, p < 0.05) indicates that the hazard ratio changes over time: \u03b2_td < 0 indicates diminishing relative risk, while \u03b2_td > 0 indicates increasing relative risk.'))
      },

      .run = function() {
          if (is.null(self$options$elapsed) || is.null(self$options$status)) {
              return()
          }

          dat <- data.frame(self$data, check.names=FALSE)
          private$.errorCheck(dat)

          # Prepare clean data
          time_var <- self$options$elapsed
          tstart_var <- self$options$tstart
          status_var <- self$options$status
          subject_id_var <- self$options$subjectId
          group_var <- self$options$group
          covs <- self$options$covariates

          has_tstart <- !is.null(tstart_var) && tstart_var != "" && tstart_var %in% names(dat)
          has_cluster <- !is.null(subject_id_var) && subject_id_var != "" && subject_id_var %in% names(dat)

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
          if (has_tstart) {
              dat$..tstart <- as.numeric(dat[[tstart_var]])
          }
          if (has_cluster) {
              dat$..cluster <- as.factor(dat[[subject_id_var]])
          }

          # ----------------------------------------------------
          # 1. Kaplan-Meier Survival Summary
          # ----------------------------------------------------
          surv_km_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..event)" else "survival::Surv(..time, ..event)"
          km_filter <- !is.na(dat$..time) & !is.na(dat$..event)
          if (has_tstart) km_filter <- km_filter & !is.na(dat$..tstart)

          if (!is.null(group_var)) {
              dat$..group <- as.factor(dat[[group_var]])
              dat_km <- dat[km_filter & !is.na(dat$..group), ]
              formula_km <- stats::as.formula(paste(surv_km_str, "~ ..group"))
          } else {
              dat_km <- dat[km_filter, ]
              formula_km <- stats::as.formula(paste(surv_km_str, "~ 1"))
          }

          km_sig <- list(
              self$options$showKm,
              self$options$kmPlot,
              self$options$kmTable,
              self$options$kmMaxTime,
              time_var,
              tstart_var,
              status_var,
              self$options$statusEvent,
              group_var,
              nrow(dat_km)
          )

          km_needs_run <- !identical(private$.km_sig, km_sig) ||
                          (self$options$showKm && self$results$kmSection$kmSummaryTable$rowCount == 0)

          if (km_needs_run) {
              if (self$options$showKm && self$options$kmPlot && nrow(dat_km) > 0) {
                  # Store state for plot
                  self$results$kmSection$kmPlot$setState(list(data = dat_km))
              } else {
                  self$results$kmSection$kmPlot$setState(NULL)
              }

              if (self$options$showKm && nrow(dat_km) > 0) {

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
                              group  = jmvcore::.("All"),
                              n      = as.integer(get_val(sum_km, c("records", "n"))),
                              events = as.integer(get_val(sum_km, c("events"))),
                              median = get_val(sum_km, c("median")),
                              lower  = get_val(sum_km, c("0.95LCL", "lower")),
                              upper  = get_val(sum_km, c("0.95UCL", "upper"))
                          ))
                      }

                      if (has_tstart) {
                          kmTable$setNote('cp_note', .('<b>Counting process / Delayed entry</b>: Kaplan–Meier curves estimated with left truncation Surv(tstart, time, event).'))
                      } else {
                          kmTable$setNote('cp_note', NULL)
                      }

                      # Populate kmRiskTable if requested
                      kmRiskTable <- self$results$kmSection$kmRiskTable
                      kmRiskTable$deleteRows()
                      if (self$options$kmTable) {
                          max_time <- max(dat_km$..time, na.rm = TRUE)
                          limit_time <- self$options$kmMaxTime
                          if (!is.null(limit_time) && length(limit_time) > 0) {
                              limit_val <- suppressWarnings(as.numeric(limit_time))
                              if (!is.na(limit_val) && limit_val > 0) {
                                  max_time <- limit_val
                              }
                          }
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
                  private$.km_sig <- km_sig
              } else {
                  self$results$kmSection$kmSummaryTable$deleteRows()
                  self$results$kmSection$kmRiskTable$deleteRows()
                  self$results$kmSection$kmPlot$setState(NULL)
                  private$.km_sig <- NULL
              }
          }

          # ----------------------------------------------------
          # 2. Log-rank Test
          # ----------------------------------------------------
          lr_sig <- list(
              self$options$showLogRank,
              time_var,
              tstart_var,
              status_var,
              self$options$statusEvent,
              group_var,
              nrow(dat_km)
          )

          lr_needs_run <- !identical(private$.lr_sig, lr_sig) ||
                          (self$options$showLogRank && !is.null(group_var) && self$results$logRankTable$rowCount == 0)

          if (lr_needs_run) {
              if (self$options$showLogRank && !is.null(group_var) && nrow(dat_km) > 0) {
                  ug <- unique(dat_km$..group)
                  if (length(ug) >= 2) {
                      logRankTable <- self$results$logRankTable
                      logRankTable$deleteRows()
                      if (has_tstart) {
                          fit_lr <- try(survival::coxph(survival::Surv(..tstart, ..time, ..event) ~ ..group, data = dat_km), silent = TRUE)
                          if (!inherits(fit_lr, "try-error")) {
                              s_lr <- summary(fit_lr)
                              stat_val <- as.numeric(s_lr$sctest["test"])
                              df_val <- as.integer(s_lr$sctest["df"])
                              p_val <- as.numeric(s_lr$sctest["pvalue"])
                              logRankTable$addRow(rowKey="test", values=list(
                                  chisq = stat_val,
                                  df    = df_val,
                                  p     = p_val
                              ))
                              logRankTable$setNote('cp_lr_note', .('Score (log-rank) test from Cox model is reported, as standard Mantel–Cox log-rank is not defined for counting process data.'))
                          }
                      } else {
                          sdiff <- try(survival::survdiff(survival::Surv(..time, ..event) ~ ..group, data = dat_km), silent = TRUE)
                          if (!inherits(sdiff, "try-error")) {
                              df <- length(sdiff$n) - 1
                              chisq <- sdiff$chisq
                              pval <- 1 - stats::pchisq(chisq, df)
                              logRankTable$addRow(rowKey="test", values=list(
                                  chisq = chisq,
                                  df    = df,
                                  p     = pval
                              ))
                              logRankTable$setNote('cp_lr_note', NULL)
                          }
                      }
                      private$.lr_sig <- lr_sig
                  } else {
                      self$results$logRankTable$deleteRows()
                      private$.lr_sig <- NULL
                  }
              } else {
                  self$results$logRankTable$deleteRows()
                  private$.lr_sig <- NULL
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
                          dat[[paste0("..cov_", i, "_")]] <- as.factor(col_val)
                      } else {
                          dat[[paste0("..cov_", i, "_")]] <- as.numeric(col_val)
                      }
                      terms <- c(terms, paste0("..cov_", i, "_"))
                  }
              }

              if (length(terms) > 0) {
                  # Drop NAs
                  cols_to_check <- c("..time", "..event", if (!is.null(group_var)) "..group" else NULL)
                  if (has_tstart) cols_to_check <- c(cols_to_check, "..tstart")
                  if (has_cluster) cols_to_check <- c(cols_to_check, "..cluster")
                  if (length(covs) > 0) {
                      for (i in seq_along(covs)) {
                          cols_to_check <- c(cols_to_check, paste0("..cov_", i, "_"))
                      }
                  }
                  dat_cox <- dat[stats::complete.cases(dat[, cols_to_check, drop=FALSE]), ]

                  cox_model_sig <- list(
                      time_var,
                      tstart_var,
                      status_var,
                      self$options$statusEvent,
                      group_var,
                      covs,
                      subject_id_var,
                      nrow(dat_cox)
                  )

                  cox_needs_refit <- !identical(private$.cox_model_sig, cox_model_sig) ||
                                     is.null(private$.cached_cox_fit) ||
                                     self$results$coxSection$coxFitTable$rowCount == 0

                  if (cox_needs_refit) {
                      if (nrow(dat_cox) > 0) {
                          surv_cox_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..event)" else "survival::Surv(..time, ..event)"
                          formula_cox <- paste(surv_cox_str, "~", paste(terms, collapse = " + "))
                          cluster_arg <- if (has_cluster) dat_cox$..cluster else NULL
                          cox_fit <- try(survival::coxph(as.formula(formula_cox), data = dat_cox, cluster = cluster_arg, x = TRUE), silent = TRUE)

                          if (!inherits(cox_fit, "try-error")) {
                              private$.cached_cox_fit <- cox_fit
                              private$.cached_cox_terms <- terms
                              private$.cox_model_sig <- cox_model_sig
                              private$.cached_zph <- NULL
                              private$.cached_zph_rows <- NULL

                              # Model fit summaries
                              sum_cox <- summary(cox_fit)
                              fitTable <- self$results$coxSection$coxFitTable
                              fitTable$deleteRows()

                              if (!is.null(sum_cox$logtest)) {
                                  fitTable$addRow(rowKey="lr", values=list(
                                      test = jmvcore::.("Likelihood Ratio Test"),
                                      stat = as.numeric(sum_cox$logtest["test"]),
                                      df   = as.integer(sum_cox$logtest["df"]),
                                      p    = as.numeric(sum_cox$logtest["pvalue"])
                                  ))
                              }
                              if (!is.null(sum_cox$waldtest)) {
                                  fitTable$addRow(rowKey="wald", values=list(
                                      test = jmvcore::.("Wald Test"),
                                      stat = as.numeric(sum_cox$waldtest["test"]),
                                      df   = as.integer(sum_cox$waldtest["df"]),
                                      p    = as.numeric(sum_cox$waldtest["pvalue"])
                                  ))
                              }
                              if (!is.null(sum_cox$sctest)) {
                                  fitTable$addRow(rowKey="score", values=list(
                                      test = jmvcore::.("Score (Log-rank) Test"),
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
                                  se_col <- if ("robust se" %in% colnames(coefs)) "robust se" else "se(coef)"
                                  for (i in seq_along(row_names)) {
                                      raw_name <- row_names[i]
                                      clean_name <- raw_name
                                      if (grepl("^\\.\\.group", raw_name)) {
                                          lvl <- gsub("^\\.\\.group_?", "", raw_name)
                                          clean_name <- if (nzchar(lvl)) paste0(group_var, " (", lvl, ")") else group_var
                                      } else if (grepl("^\\.\\.cov_([0-9]+)", raw_name)) {
                                          match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", raw_name))
                                          rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx, "_?"), "", raw_name)
                                          cov_name <- if (match_idx >= 1 && match_idx <= length(covs)) covs[match_idx] else paste0("cov_", match_idx)
                                          clean_name <- if (nzchar(rem_suffix)) paste0(cov_name, " (", rem_suffix, ")") else cov_name
                                      }

                                      coefTable$addRow(rowKey=raw_name, values=list(
                                          var     = clean_name,
                                          coef    = as.numeric(coefs[i, "coef"]),
                                          se      = as.numeric(coefs[i, se_col]),
                                          z       = as.numeric(coefs[i, "z"]),
                                          p       = as.numeric(coefs[i, "Pr(>|z|)"]),
                                          hr      = as.numeric(conf_int[i, "exp(coef)"]),
                                          hrLower = as.numeric(conf_int[i, "lower .95"]),
                                          hrUpper = as.numeric(conf_int[i, "upper .95"])
                                      ))
                                  }

                                  if (has_cluster) {
                                      coefTable$setNote("cluster_note", paste0(jmvcore::.("Standard errors and confidence intervals are adjusted for clustering by"), " ", subject_id_var, " (", jmvcore::.("Huber–White robust sandwich estimator"), ")."))
                                  } else {
                                      coefTable$setNote("cluster_note", NULL)
                                  }
                                  if (has_tstart) {
                                      coefTable$setNote("cp_note", jmvcore::.("Counting process formulation with left-truncation (tstart, elapsed)."))
                                  } else {
                                      coefTable$setNote("cp_note", NULL)
                                  }
                              }
                          } else {
                              private$.cached_cox_fit <- NULL
                              private$.cached_cox_terms <- NULL
                              private$.cached_zph <- NULL
                              private$.cached_zph_rows <- NULL
                              private$.cox_model_sig <- NULL
                              self$results$coxSection$coxFitTable$deleteRows()
                              self$results$coxSection$coxCoefTable$deleteRows()
                          }
                      } else {
                          private$.cached_cox_fit <- NULL
                          private$.cached_cox_terms <- NULL
                          private$.cached_zph <- NULL
                          private$.cached_zph_rows <- NULL
                          private$.cox_model_sig <- NULL
                          self$results$coxSection$coxFitTable$deleteRows()
                          self$results$coxSection$coxCoefTable$deleteRows()
                      }
                  }

                  cox_fit <- private$.cached_cox_fit
                  terms <- private$.cached_cox_terms

                  if (!is.null(cox_fit)) {
                      # Forest plot state
                      if (self$options$coxForest) {
                          if (is.null(self$results$coxSection$coxForestPlot$state)) {
                              self$results$coxSection$coxForestPlot$setState(list(fit = cox_fit, terms = terms))
                          }
                      } else {
                          self$results$coxSection$coxForestPlot$setState(NULL)
                      }

                      # Proportionality check
                      if (self$options$coxAssump) {
                          if (is.null(private$.cached_zph)) {
                              zph <- try(survival::cox.zph(cox_fit), silent = TRUE)
                              if (!inherits(zph, "try-error")) {
                                  private$.cached_zph <- zph
                                  private$.cached_zph_rows <- rownames(zph$table)
                              }
                          } else {
                              zph <- private$.cached_zph
                          }

                          if (!is.null(zph)) {
                              assumpTable <- self$results$coxSection$coxAssumpTable
                              zph_table <- zph$table
                              zph_rows <- private$.cached_zph_rows

                              if (assumpTable$rowCount == 0) {
                                  for (i in seq_along(zph_rows)) {
                                      row_name <- zph_rows[i]
                                      clean_name <- row_name
                                      if (row_name == "GLOBAL") {
                                          clean_name <- "GLOBAL"
                                      } else if (grepl("^\\.\\.group", row_name)) {
                                          lvl <- gsub("^\\.\\.group_?", "", row_name)
                                          clean_name <- if (nzchar(lvl)) paste0(group_var, " (", lvl, ")") else group_var
                                      } else if (grepl("^\\.\\.cov_([0-9]+)", row_name)) {
                                          match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", row_name))
                                          rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx, "_?"), "", row_name)
                                          cov_name <- if (match_idx >= 1 && match_idx <= length(covs)) covs[match_idx] else paste0("cov_", match_idx)
                                          clean_name <- if (nzchar(rem_suffix)) paste0(cov_name, " (", rem_suffix, ")") else cov_name
                                      }

                                      assumpTable$addRow(rowKey=row_name, values=list(
                                          var    = clean_name,
                                          chisq  = as.numeric(zph_table[i, "chisq"]),
                                          df     = as.integer(zph_table[i, "df"]),
                                          p      = as.numeric(zph_table[i, "p"])
                                      ))
                                  }
                              }

                              schoenfeldPlots <- self$results$coxSection$schoenfeldPlots
                              if (self$options$showSchoenfeldPlot) {
                                  plot_vars <- zph_rows[zph_rows != "GLOBAL"]
                                  existing_keys <- names(schoenfeldPlots$items)
                                  if (!identical(existing_keys, plot_vars) || is.null(schoenfeldPlots$state)) {
                                      schoenfeldPlots$clear()
                                      schoenfeldPlots$setState(list(zph = zph, row_names = zph_rows))
                                      for (pv in plot_vars) {
                                          schoenfeldPlots$addItem(key = pv)
                                      }
                                      for (pv in plot_vars) {
                                          clean_pv <- pv
                                          if (grepl("^\\.\\.group", pv)) {
                                              lvl <- gsub("^\\.\\.group_?", "", pv)
                                              clean_pv <- if (nzchar(lvl)) paste0(group_var, " (", lvl, ")") else group_var
                                          } else if (grepl("^\\.\\.cov_([0-9]+)", pv)) {
                                              match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+).*", "\\1", pv))
                                              rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx, "_?"), "", pv)
                                              cov_name <- if (match_idx >= 1 && match_idx <= length(covs)) covs[match_idx] else paste0("cov_", match_idx)
                                              clean_pv <- if (nzchar(rem_suffix)) paste0(cov_name, " (", rem_suffix, ")") else cov_name
                                          }
                                          item <- schoenfeldPlots$get(key=pv)
                                          item$setTitle(paste0(jmvcore::.("Schoenfeld residuals"), " \u2013 ", clean_pv))
                                          item$setState(list(zph = zph, var_idx = which(rownames(zph$table) == pv), var_name = pv))
                                      }
                                  }
                              } else {
                                  if (length(schoenfeldPlots$items) > 0 || !is.null(schoenfeldPlots$state)) {
                                      schoenfeldPlots$clear()
                                      schoenfeldPlots$setState(NULL)
                                  }
                              }
                          }
                      } else {
                          self$results$coxSection$coxAssumpTable$deleteRows()
                          self$results$coxSection$schoenfeldPlots$clear()
                          self$results$coxSection$schoenfeldPlots$setState(NULL)
                          private$.cached_zph <- NULL
                          private$.cached_zph_rows <- NULL
                      }
                  } else {
                      self$results$coxSection$coxFitTable$deleteRows()
                      self$results$coxSection$coxCoefTable$deleteRows()
                      self$results$coxSection$coxForestPlot$setState(NULL)
                      self$results$coxSection$coxAssumpTable$deleteRows()
                      self$results$coxSection$schoenfeldPlots$clear()
                      self$results$coxSection$schoenfeldPlots$setState(NULL)
                  }
              } else {
                  self$results$coxSection$coxFitTable$deleteRows()
                  self$results$coxSection$coxCoefTable$deleteRows()
                  self$results$coxSection$coxForestPlot$setState(NULL)
                  self$results$coxSection$coxAssumpTable$deleteRows()
                  self$results$coxSection$schoenfeldPlots$clear()
                  self$results$coxSection$schoenfeldPlots$setState(NULL)
                  private$.cached_cox_fit <- NULL
                  private$.cached_cox_terms <- NULL
                  private$.cached_zph <- NULL
                  private$.cached_zph_rows <- NULL
                  private$.cox_model_sig <- NULL
              }

              if (self$options$showTimeDepEffects) {
                  timedep_sig <- list(
                      time_var,
                      tstart_var,
                      status_var,
                      subject_id_var,
                      group_var,
                      covs,
                      self$options$timeDepVars,
                      self$options$timeDepFunc,
                      nrow(dat)
                  )
                  if (!identical(private$.timedep_sig, timedep_sig) || self$results$coxSection$timeDepTable$rowCount == 0) {
                      private$.runTimeDepCox(dat, time_var, tstart_var, subject_id_var, group_var, covs, has_tstart, has_cluster)
                      private$.timedep_sig <- timedep_sig
                  }
              } else {
                  self$results$coxSection$timeDepTable$deleteRows()
                  private$.timedep_sig <- NULL
              }
          } else {
              self$results$coxSection$coxFitTable$deleteRows()
              self$results$coxSection$coxCoefTable$deleteRows()
              self$results$coxSection$coxForestPlot$setState(NULL)
              self$results$coxSection$coxAssumpTable$deleteRows()
              self$results$coxSection$schoenfeldPlots$clear()
              self$results$coxSection$schoenfeldPlots$setState(NULL)
              self$results$coxSection$timeDepTable$deleteRows()
              private$.cached_cox_fit <- NULL
              private$.cached_cox_terms <- NULL
              private$.cached_zph <- NULL
              private$.cached_zph_rows <- NULL
              private$.cox_model_sig <- NULL
              private$.timedep_sig <- NULL
          }

          # ----------------------------------------------------
          # 4. Adjusted Survival Curves (Direct / G-computation)
          # ----------------------------------------------------
          adjSummaryTable <- self$results$adjSection$adjSummaryTable
          if (self$options$showAdjCurves) {
              exp_var <- self$options$adjExposure
              if (is.null(exp_var) || exp_var == "") {
                  exp_var <- group_var
              }

              adj_sig <- list(
                  exp_var,
                  group_var,
                  covs,
                  time_var,
                  tstart_var,
                  status_var,
                  self$options$statusEvent,
                  self$options$adjCI,
                  self$options$kmMaxTime,
                  nrow(dat)
              )

              adj_needs_run <- !identical(private$.adj_sig, adj_sig) ||
                               adjSummaryTable$rowCount == 0 ||
                               is.null(self$results$adjSection$adjPlot$state)

              if (adj_needs_run) {
                  adjSummaryTable$deleteRows()
                  if (is.null(exp_var) || exp_var == "" || !(exp_var %in% names(dat))) {
                      adjSummaryTable$setNote('exp_warn', .("Please select a Grouping Variable or Exposure Variable to compute adjusted survival curves."))
                      self$results$adjSection$adjPlot$setState(NULL)
                  } else {
                      exp_col <- dat[[exp_var]]
                      if (!is.factor(exp_col) && length(unique(stats::na.omit(exp_col))) > 10) {
                          adjSummaryTable$setNote('exp_warn', .("The selected exposure variable must be categorical or have 10 or fewer distinct values."))
                          self$results$adjSection$adjPlot$setState(NULL)
                      } else {
                          adjSummaryTable$setNote('exp_warn', NULL)
                          dat$..adj_exp <- as.factor(exp_col)
                          levels_z <- levels(dat$..adj_exp)
                          levels_z <- levels_z[levels_z %in% unique(as.character(stats::na.omit(dat$..adj_exp)))]

                          if (length(levels_z) < 2) {
                              adjSummaryTable$setNote('exp_warn', .("The exposure variable must have at least two distinct levels."))
                              self$results$adjSection$adjPlot$setState(NULL)
                          } else {
                              # Adjustment covariates (confounders)
                              adj_covs <- covs[covs != exp_var]
                              adj_terms <- c("..adj_exp")
                              if (length(adj_covs) > 0) {
                                  for (i in seq_along(adj_covs)) {
                                      c_name <- adj_covs[i]
                                      c_val <- dat[[c_name]]
                                      if (is.factor(c_val) || is.character(c_val)) {
                                          dat[[paste0("..adj_cov_", i, "_")]] <- as.factor(c_val)
                                      } else {
                                          dat[[paste0("..adj_cov_", i, "_")]] <- as.numeric(c_val)
                                      }
                                      adj_terms <- c(adj_terms, paste0("..adj_cov_", i, "_"))
                                  }
                              }

                              cols_needed <- c("..time", "..event", "..adj_exp")
                              if (has_tstart) cols_needed <- c(cols_needed, "..tstart")
                              if (length(adj_covs) > 0) {
                                  for (i in seq_along(adj_covs)) {
                                      cols_needed <- c(cols_needed, paste0("..adj_cov_", i, "_"))
                                  }
                              }
                              dat_adj <- dat[stats::complete.cases(dat[, cols_needed, drop = FALSE]), ]

                              if (nrow(dat_adj) > 0) {
                                  surv_adj_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..event)" else "survival::Surv(..time, ..event)"
                                  formula_adj <- paste(surv_adj_str, "~", paste(adj_terms, collapse = " + "))
                                  adj_fit <- try(survival::coxph(as.formula(formula_adj), data = dat_adj, x = TRUE), silent = TRUE)

                                  if (!inherits(adj_fit, "try-error")) {
                                      tau <- max(dat_adj$..time, na.rm = TRUE)
                                      limit_time <- self$options$kmMaxTime
                                      if (!is.null(limit_time) && limit_time > 0) {
                                          tau <- min(tau, limit_time)
                                      }
                                      adjSummaryTable$setNote('tau_note', jmvcore::format(.("Restricted mean survival time evaluated up to τ = {tau}."), tau = round(tau, 2)))

                                      bh <- try(survival::basehaz(adj_fit, centered = FALSE), silent = TRUE)
                                      if (!inherits(bh, "try-error") && nrow(bh) > 0) {
                                          tt <- stats::delete.response(stats::terms(adj_fit))
                                          time_range <- max(dat_adj$..time, na.rm = TRUE)
                                          eval_times <- sort(unique(c(0, pretty(c(0, time_range), n = 100), time_range)))
                                          h0_step <- stats::stepfun(bh$time[-1], bh$hazard)
                                          h0_eval <- c(0, h0_step(eval_times[-1]))

                                          adj_curves <- list()
                                          coef_fit <- stats::coef(adj_fit)

                                          for (z in levels_z) {
                                              df_cf <- dat_adj
                                              df_cf$..adj_exp <- factor(z, levels = levels(dat_adj$..adj_exp))
                                              mm_cf <- stats::model.matrix(tt, data = df_cf)
                                              if ("(Intercept)" %in% colnames(mm_cf)) {
                                                  mm_cf <- mm_cf[, -which(colnames(mm_cf) == "(Intercept)"), drop = FALSE]
                                              }
                                              common_cols <- intersect(names(coef_fit), colnames(mm_cf))
                                              eta <- as.vector(mm_cf[, common_cols, drop = FALSE] %*% coef_fit[common_cols])
                                              exp_eta <- exp(eta)

                                              s_eval <- rowMeans(exp(-outer(h0_eval, exp_eta, "*")))

                                              t_full <- c(0, bh$time)
                                              h0_full <- c(0, bh$hazard)
                                              s_full <- rowMeans(exp(-outer(h0_full, exp_eta, "*")))

                                              med_idx <- which(s_full <= 0.5)[1]
                                              med_val <- if (!is.na(med_idx)) t_full[med_idx] else NA_real_

                                              t_sub <- t_full[t_full <= tau]
                                              s_sub <- s_full[t_full <= tau]
                                              if (tail(t_sub, 1) < tau) {
                                                  s_sub <- c(s_sub, tail(s_sub, 1))
                                                  t_sub <- c(t_sub, tau)
                                              }
                                              rmst_val <- sum(s_sub[-length(s_sub)] * diff(t_sub))

                                              n_z <- sum(dat_adj$..adj_exp == z)
                                              events_z <- sum(dat_adj$..event[dat_adj$..adj_exp == z] == 1)

                                              adjSummaryTable$addRow(rowKey = as.character(z), values = list(
                                                  group  = as.character(z),
                                                  n      = as.integer(n_z),
                                                  events = as.integer(events_z),
                                                  median = med_val,
                                                  rmst   = rmst_val
                                              ))

                                              adj_curves[[z]] <- list(
                                                  times = eval_times,
                                                  surv  = s_eval,
                                                  lower = NULL,
                                                  upper = NULL
                                              )
                                          }

                                          if (self$options$adjCI) {
                                              B <- 100
                                              boot_arr <- array(NA_real_, dim = c(B, length(eval_times), length(levels_z)),
                                                                dimnames = list(NULL, NULL, levels_z))
                                              set.seed(42)
                                              N_adj <- nrow(dat_adj)

                                              for (b in seq_len(B)) {
                                                  if (b %% 10 == 0) private$.checkpoint()
                                                  idx_b <- sample(N_adj, replace = TRUE)
                                                  dat_b <- dat_adj[idx_b, ]
                                                  fit_b <- try(survival::coxph(as.formula(formula_adj), data = dat_b, x = TRUE), silent = TRUE)
                                                  if (inherits(fit_b, "try-error")) next
                                                  bh_b <- try(survival::basehaz(fit_b, centered = FALSE), silent = TRUE)
                                                  if (inherits(bh_b, "try-error") || nrow(bh_b) == 0) next

                                                  h0_step_b <- stats::stepfun(bh_b$time[-1], bh_b$hazard)
                                                  h0_eval_b <- c(0, h0_step_b(eval_times[-1]))
                                                  coef_b <- stats::coef(fit_b)

                                                  for (z in levels_z) {
                                                      df_b <- dat_b
                                                      df_b$..adj_exp <- factor(z, levels = levels(dat_adj$..adj_exp))
                                                      mm_b <- stats::model.matrix(tt, data = df_b)
                                                      if ("(Intercept)" %in% colnames(mm_b)) {
                                                          mm_b <- mm_b[, -which(colnames(mm_b) == "(Intercept)"), drop = FALSE]
                                                      }
                                                      common_cols_b <- intersect(names(coef_b), colnames(mm_b))
                                                      eta_b <- as.vector(mm_b[, common_cols_b, drop = FALSE] %*% coef_b[common_cols_b])
                                                      boot_arr[b, , z] <- rowMeans(exp(-outer(h0_eval_b, exp(eta_b), "*")))
                                                  }
                                              }

                                              for (z in levels_z) {
                                                  adj_curves[[z]]$lower <- apply(boot_arr[, , z], 2, stats::quantile, probs = 0.025, na.rm = TRUE)
                                                  adj_curves[[z]]$upper <- apply(boot_arr[, , z], 2, stats::quantile, probs = 0.975, na.rm = TRUE)
                                              }
                                          }

                                          self$results$adjSection$adjPlot$setState(list(
                                              curves    = adj_curves,
                                              levels    = levels_z,
                                              exp_var   = exp_var,
                                              time_var  = time_var,
                                              has_ci    = self$options$adjCI
                                          ))
                                      } else {
                                          self$results$adjSection$adjPlot$setState(NULL)
                                      }
                                  } else {
                                      self$results$adjSection$adjPlot$setState(NULL)
                                  }
                              } else {
                                  self$results$adjSection$adjPlot$setState(NULL)
                              }
                          }
                      }
                  }
                  private$.adj_sig <- adj_sig
              }
          } else {
              adjSummaryTable$deleteRows()
              self$results$adjSection$adjPlot$setState(NULL)
              private$.adj_sig <- NULL
          }

          # ----------------------------------------------------
          # 5. Time-dependent ROC Analysis
          # ----------------------------------------------------
          roc_preds <- self$options$rocPredictors
          if (self$options$showRoc && length(roc_preds) > 0) {
              rocTable <- self$results$rocSection$rocTable
              roc_time <- as.numeric(self$options$rocTime)

              roc_sig <- list(
                  roc_preds,
                  roc_time,
                  self$options$rocCI,
                  self$options$rocWidth,
                  time_var,
                  status_var,
                  self$options$statusEvent,
                  nrow(dat)
              )

              roc_needs_run <- !identical(private$.roc_sig, roc_sig) ||
                               rocTable$rowCount == 0 ||
                               (self$options$rocPlot && is.null(self$results$rocSection$rocPlot$state))

              if (roc_needs_run) {
                  rocTable$deleteRows()
                  roc_plot_list <- list()

                  # Data check
                  cols_to_check_roc <- c("..time", "..event", roc_preds)
                  dat_roc <- dat[stats::complete.cases(dat[, cols_to_check_roc, drop=FALSE]), ]

                  if (nrow(dat_roc) > 0) {
                      for (pred in roc_preds) {
                          private$.checkpoint()
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

                                  # Precompute threshold CI if requested, so rendering is instantaneous
                                  roc_ci <- NULL
                                  if (self$options$rocCI && !is.null(cutoff) && !is.na(cutoff)) {
                                      roc_ci <- try(pROC::ci.thresholds(roc_obj, thresholds = cutoff, conf.level = ci_width, boot.n = 500), silent = TRUE)
                                      if (inherits(roc_ci, "try-error")) roc_ci <- NULL
                                  }
                                  roc_obj$ci <- roc_ci

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
                                      cutoff = cutoff,
                                      ci     = roc_ci
                                  )
                              }
                          }
                      }
                      if (self$options$rocPlot) {
                          self$results$rocSection$rocPlot$setState(roc_plot_list)
                      } else {
                          self$results$rocSection$rocPlot$setState(NULL)
                      }
                  } else {
                      self$results$rocSection$rocPlot$setState(NULL)
                  }

                  # Set warning note if table is empty
                  if (self$results$rocSection$rocTable$rowCount == 0) {
                      self$results$rocSection$rocTable$setNote('empty_warning',
                          .('Not enough events or survivors at the selected analysis time point t to perform ROC analysis.'))
                  } else {
                      self$results$rocSection$rocTable$setNote('empty_warning', NULL)
                  }
                  private$.roc_sig <- roc_sig
              }
          } else {
              self$results$rocSection$rocTable$deleteRows()
              self$results$rocSection$rocPlot$setState(NULL)
              private$.roc_sig <- NULL
          }

          # ----------------------------------------------------
          # 6. Competing Risks Analysis
          # ----------------------------------------------------
          if (self$options$showCompRisks) {
              cr_sig <- list(
                  self$options$compCensor,
                  self$options$compEvent,
                  self$options$statusEvent,
                  status_var,
                  time_var,
                  tstart_var,
                  subject_id_var,
                  group_var,
                  covs,
                  self$options$kmMaxTime,
                  self$options$showCifTable,
                  self$options$showCifPlot,
                  self$options$cifCI,
                  self$options$showGrayTest,
                  self$options$showFineGray,
                  nrow(dat)
              )

              cifTable <- self$results$compRisksSection$cifSummaryTable
              cifPlot <- self$results$compRisksSection$cifPlot
              grayTable <- self$results$compRisksSection$grayTestTable
              fgTable <- self$results$compRisksSection$fineGrayTable

              cr_needs_run <- !identical(private$.cr_sig, cr_sig) ||
                              (self$options$showCifTable && cifTable$rowCount == 0) ||
                              (self$options$showCifPlot && is.null(cifPlot$state)) ||
                              (self$options$showGrayTest && !is.null(group_var) && grayTable$rowCount == 0) ||
                              (self$options$showFineGray && fgTable$rowCount == 0)

              if (cr_needs_run) {
                  private$.runCompetingRisks(dat, time_var, tstart_var, status_var, subject_id_var, group_var, covs, has_tstart, has_cluster)
                  private$.cr_sig <- cr_sig
              }
          } else {
              self$results$compRisksSection$cifSummaryTable$deleteRows()
              self$results$compRisksSection$cifPlot$setState(NULL)
              self$results$compRisksSection$grayTestTable$deleteRows()
              self$results$compRisksSection$fineGrayTable$deleteRows()
              private$.cr_sig <- NULL
          }
      },

      .kmPlot = function(image, ggtheme, theme, ...) {
          state <- image$state
          if (is.null(state)) return(FALSE)

          dat_plot <- state$data
          if (is.null(dat_plot) || nrow(dat_plot) == 0) return(FALSE)

          has_tstart <- "..tstart" %in% names(dat_plot)
          group_var <- self$options$group
          surv_km_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..event)" else "survival::Surv(..time, ..event)"
          if (!is.null(group_var)) {
              formula <- stats::as.formula(paste(surv_km_str, "~ ..group"))
          } else {
              formula <- stats::as.formula(paste(surv_km_str, "~ 1"))
          }

          km_type <- self$options$kmType
          if (km_type == "hazard") km_type <- "cumhaz"

          p <- NULL
          if (requireNamespace("ggsurvfit", quietly = TRUE)) {
              fit <- try(ggsurvfit::survfit2(formula, data = dat_plot), silent = TRUE)
              if (!inherits(fit, "try-error")) {
                  p <- try({
                      p_obj <- ggsurvfit::ggsurvfit(fit, type = km_type) +
                          ggplot2::labs(
                              x = self$options$elapsed,
                              y = ifelse(km_type == "survival", .("Survival probability"), .("Cumulative hazard"))
                          )
                      if (self$options$kmCI) {
                          p_obj <- p_obj + ggsurvfit::add_confidence_interval()
                      }
                      p_obj
                  }, silent = TRUE)
                  if (inherits(p, "try-error")) p <- NULL
              }
          }

          if (is.null(p)) {
              fit <- try(survival::survfit(formula, data = dat_plot), silent = TRUE)
              if (inherits(fit, "try-error")) return(FALSE)

              strata_names <- if (!is.null(fit$strata)) {
                  rep(names(fit$strata), fit$strata)
              } else {
                  rep("All", length(fit$time))
              }
              strata_clean <- sub("^\\.\\.group=", "", strata_names)

              df <- data.frame(
                  time = fit$time,
                  surv = fit$surv,
                  cumhaz = if (!is.null(fit$cumhaz)) fit$cumhaz else -log(pmax(fit$surv, 1e-10)),
                  lower = fit$lower,
                  upper = fit$upper,
                  n.censor = fit$n.censor,
                  strata = strata_clean,
                  stringsAsFactors = FALSE
              )

              u_strata <- unique(df$strata)
              t0_list <- lapply(u_strata, function(st) {
                  data.frame(time = 0, surv = 1, cumhaz = 0, lower = 1, upper = 1, n.censor = 0, strata = st, stringsAsFactors = FALSE)
              })
              df <- rbind(do.call(rbind, t0_list), df)
              df <- df[order(df$strata, df$time), ]
              df$strata <- factor(df$strata, levels = u_strata)

              y_var <- if (km_type == "cumhaz") "cumhaz" else "surv"
              y_label <- if (km_type == "cumhaz") .("Cumulative hazard") else .("Survival probability")

              if (!is.null(group_var)) {
                  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "time", y = y_var, color = "strata"))
              } else {
                  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "time", y = y_var))
              }

              if (self$options$kmCI) {
                  make_step_ribbon <- function(d) {
                      do.call(rbind, lapply(split(d, d$strata), function(subd) {
                          n <- nrow(subd)
                          if (n <= 1) return(subd)
                          idx <- rep(1:n, each = 2)
                          res <- subd[idx, ]
                          res$time <- c(subd$time[1], rep(subd$time[2:n], each = 2), subd$time[n])[1:nrow(res)]
                          res
                      }))
                  }
                  ribbon_df <- make_step_ribbon(df)
                  if (!is.null(group_var)) {
                      p <- p + ggplot2::geom_ribbon(data = ribbon_df, ggplot2::aes_string(ymin = "lower", ymax = "upper", fill = "strata"), alpha = 0.2, linetype = 0)
                  } else {
                      p <- p + ggplot2::geom_ribbon(data = ribbon_df, ggplot2::aes_string(ymin = "lower", ymax = "upper"), alpha = 0.2, linetype = 0, fill = "grey50")
                  }
              }

              p <- p + ggplot2::geom_step(linewidth = 0.8)

              cens_df <- df[df$n.censor > 0, ]
              if (nrow(cens_df) > 0) {
                  if (!is.null(group_var)) {
                      p <- p + ggplot2::geom_point(data = cens_df, ggplot2::aes_string(x = "time", y = y_var, color = "strata"), shape = 3, size = 2)
                  } else {
                      p <- p + ggplot2::geom_point(data = cens_df, ggplot2::aes_string(x = "time", y = y_var), shape = 3, size = 2)
                  }
              }

              p <- p + ggplot2::labs(
                  x = self$options$elapsed,
                  y = y_label,
                  color = group_var,
                  fill = group_var
              )
          }

          limit_time <- self$options$kmMaxTime
          if (!is.null(limit_time) && length(limit_time) > 0) {
              limit_val <- suppressWarnings(as.numeric(limit_time))
              if (!is.na(limit_val) && limit_val > 0) {
                  p <- p + ggplot2::coord_cartesian(xlim = c(0, limit_val))
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
          se_col_cox <- if ("robust se" %in% colnames(sum_cox$coefficients)) "robust se" else if ("se(coef)" %in% colnames(sum_cox$coefficients)) "se(coef)" else 2
          coef_df$se <- sum_cox$coefficients[, se_col_cox]

          coef_df$var <- coef_df$raw_var
          group_var <- self$options$group
          covs <- self$options$covariates
          exp_var <- self$options$adjExposure
          if (is.null(exp_var) || exp_var == "") exp_var <- group_var

          for (i in seq_along(coef_df$raw_var)) {
              rv <- coef_df$raw_var[i]
              if (grepl("^\\.\\.group", rv)) {
                  lvl <- gsub("^\\.\\.group", "", rv)
                  coef_df$var[i] <- paste0(group_var, " (", lvl, ")")
              } else if (grepl("^\\.\\.adj_exp", rv)) {
                  lvl <- gsub("^\\.\\.adj_exp", "", rv)
                  coef_df$var[i] <- paste0(exp_var, " (", lvl, ")")
              } else if (grepl("^\\.\\.cov_([0-9]+)_", rv)) {
                  match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+)_.*", "\\1", rv))
                  rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx, "_"), "", rv)
                  cov_name <- covs[match_idx]
                  coef_df$var[i] <- if (rem_suffix != "") paste0(cov_name, " (", rem_suffix, ")") else cov_name
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
          state <- image$state
          if (is.null(state)) {
              parent_state <- image$parent$state
              if (!is.null(parent_state) && !is.null(parent_state$zph)) {
                  zph <- parent_state$zph
                  var_name <- image$key
                  var_idx <- if (!is.null(var_name) && var_name %in% rownames(zph$table)) {
                      which(rownames(zph$table) == var_name)
                  } else {
                      1
                  }
                  state <- list(zph = zph, var_idx = var_idx, var_name = var_name)
              }
          }
          if (is.null(state) || is.null(state$zph)) return(FALSE)

          zph <- state$zph
          var_idx <- state$var_idx
          if (is.null(var_idx) || length(var_idx) == 0 || is.na(var_idx)) {
              var_name <- if (!is.null(state$var_name)) state$var_name else image$key
              var_idx <- which(rownames(zph$table) == var_name)
          }
          if (length(var_idx) == 0 || is.na(var_idx)) var_idx <- 1
          if (length(var_idx) == 0 || is.na(var_idx) || var_idx < 1 || var_idx > nrow(zph$table)) return(FALSE)

          # Plot base R plot.cox.zph
          tryCatch({
              survival:::plot.cox.zph(zph[var_idx], resid = TRUE, se = TRUE, df = 4, nsmo = 40, col = "#3366B2", lwd = 2)
              abline(h = 0, col = "#E54028", lty = 2, lwd = 1.5)
              TRUE
          }, error = function(e) {
              plot(zph[var_idx], resid = TRUE, se = TRUE, df = 4, nsmo = 40, col = "#3366B2", lwd = 2)
              abline(h = 0, col = "#E54028", lty = 2, lwd = 1.5)
              TRUE
          })
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
                  if (is.null(roc_obj$ci) && !is.null(item$ci)) {
                      roc_obj$ci <- item$ci
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

      .adjPlot = function(image, ggtheme, theme, ...) {
          state <- image$state
          if (is.null(state)) return(FALSE)

          curves <- state$curves
          levels_z <- state$levels
          exp_var <- state$exp_var
          time_var <- state$time_var
          has_ci <- state$has_ci

          if (is.null(curves) || length(curves) == 0) return(FALSE)

          df_list <- list()
          for (z in levels_z) {
              c_info <- curves[[z]]
              df_z <- data.frame(
                  time  = c_info$times,
                  surv  = c_info$surv,
                  group = factor(z, levels = levels_z)
              )
              if (has_ci && !is.null(c_info$lower) && !is.null(c_info$upper)) {
                  df_z$lower <- c_info$lower
                  df_z$upper <- c_info$upper
              }
              df_list[[z]] <- df_z
          }
          plot_df <- do.call(rbind, df_list)

          p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = surv, color = group, group = group))

          if (has_ci && "lower" %in% names(plot_df)) {
              p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = group),
                                            alpha = 0.2, linetype = 0)
          }

          p <- p + ggplot2::geom_step(linewidth = 0.8) +
              ggplot2::scale_y_continuous(
                  limits = c(0, 1),
                  labels = scales::percent_format(),
                  expand = c(0.01, 0.01)
              ) +
              ggplot2::labs(
                  x = time_var,
                  y = .("Adjusted Survival Probability"),
                  color = exp_var,
                  fill = exp_var
              )

          limit_time <- self$options$kmMaxTime
          if (!is.null(limit_time) && length(limit_time) > 0) {
              limit_val <- suppressWarnings(as.numeric(limit_time))
              if (!is.na(limit_val) && limit_val > 0) {
                  p <- p + ggplot2::coord_cartesian(xlim = c(0, limit_val), ylim = c(0, 1))
              }
          }

          p <- p + ggtheme + ggplot2::theme(legend.position = "bottom")
          print(p)
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

          tstart_var <- self$options$tstart
          if (!is.null(tstart_var) && tstart_var != "" && tstart_var %in% names(dat)) {
              if (!is.numeric(dat[[tstart_var]])) {
                  jmvcore::reject(.("Entry / Start time variable must be numeric."))
              }
              valid_times <- !is.na(dat[[tstart_var]]) & !is.na(dat[[time_var]])
              if (any(dat[[tstart_var]][valid_times] < 0)) {
                  jmvcore::reject(.("Entry / Start time must be non-negative (>= 0)."))
              }
              if (any(dat[[tstart_var]][valid_times] >= dat[[time_var]][valid_times])) {
                  jmvcore::reject(.("Entry / Start time must be strictly less than elapsed time (start < stop)."))
              }
          }
      },

      .runCompetingRisks = function(dat, time_var, tstart_var, status_var, subject_id_var, group_var, covs, has_tstart, has_cluster) {
          status_col <- dat[[status_var]]
          unique_vals <- if (is.factor(status_col)) levels(status_col) else unique(stats::na.omit(status_col))

          # Determine censored level
          comp_censor_opt <- self$options$compCensor
          if (!is.null(comp_censor_opt) && comp_censor_opt != "") {
              censor_val <- comp_censor_opt
          } else {
              if ("0" %in% as.character(unique_vals)) {
                  censor_val <- "0"
              } else {
                  cen_match <- grep("censor", as.character(unique_vals), ignore.case = TRUE, value = TRUE)
                  if (length(cen_match) > 0) censor_val <- cen_match[1]
                  else censor_val <- as.character(unique_vals[1])
              }
          }

          # Determine event of interest
          comp_event_opt <- self$options$compEvent
          if (!is.null(comp_event_opt) && comp_event_opt != "") {
              event_val <- comp_event_opt
          } else if (!is.null(self$options$statusEvent) && self$options$statusEvent != "") {
              event_val <- self$options$statusEvent
          } else {
              if ("1" %in% as.character(unique_vals)) {
                  event_val <- "1"
              } else {
                  non_cen <- setdiff(as.character(unique_vals), as.character(censor_val))
                  if (length(non_cen) > 0) event_val <- non_cen[1]
                  else event_val <- as.character(unique_vals[1])
              }
          }

          # Competing event levels
          comp_vals <- setdiff(as.character(unique_vals), c(as.character(censor_val), as.character(event_val)))

          # Recode status into factor: "censor", event_val, comp_vals
          status_chr <- as.character(status_col)
          event_coded <- character(length(status_chr))
          is_censor <- status_chr == as.character(censor_val)
          is_event <- status_chr == as.character(event_val)

          event_coded[is_censor] <- "censor"
          event_coded[is_event] <- as.character(event_val)
          for (cv in comp_vals) {
              event_coded[status_chr == cv] <- cv
          }
          event_coded[!is_censor & !is_event & !(status_chr %in% comp_vals)] <- NA_character_

          all_levels <- c("censor", as.character(event_val), comp_vals)
          event_factor <- factor(event_coded, levels = all_levels)
          dat$..comp_event <- event_factor

          if (!is.null(group_var)) {
              dat$..group <- as.factor(dat[[group_var]])
          }

          # Prepare valid data subset
          valid_comp <- !is.na(dat$..time) & !is.na(dat$..comp_event)
          if (has_tstart) valid_comp <- valid_comp & !is.na(dat$..tstart)
          if (has_cluster) valid_comp <- valid_comp & !is.na(dat$..cluster)
          if (!is.null(group_var)) {
              valid_comp <- valid_comp & !is.na(dat$..group)
          }
          dat_comp <- dat[valid_comp, ]
          if (nrow(dat_comp) == 0) return()

          # Fit Aalen-Johansen CIF via survival::survfit
          surv_comp_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..comp_event)" else "survival::Surv(..time, ..comp_event)"
          if (!is.null(group_var)) {
              cif_fit <- try(survival::survfit(stats::as.formula(paste(surv_comp_str, "~ ..group")), data = dat_comp), silent = TRUE)
          } else {
              cif_fit <- try(survival::survfit(stats::as.formula(paste(surv_comp_str, "~ 1")), data = dat_comp), silent = TRUE)
          }

          causes <- if (!inherits(cif_fit, "try-error") && !is.null(cif_fit$states)) {
              cif_fit$states[cif_fit$states != "(s0)"]
          } else {
              c(as.character(event_val), comp_vals)
          }

          # 1. Cumulative Incidence Summary Table
          cifTable <- self$results$compRisksSection$cifSummaryTable
          cifTable$deleteRows()

          if (self$options$showCifTable && !inherits(cif_fit, "try-error")) {
              limit_time <- self$options$kmMaxTime
              has_limit <- (!is.null(limit_time) && length(limit_time) > 0 && as.numeric(limit_time) > 0)
              t_eval <- if (has_limit) as.numeric(limit_time) else max(dat_comp$..time, na.rm = TRUE)

              if (length(comp_vals) == 0) {
                  cifTable$setNote('single_cause', .("Status variable has only one event type; cumulative incidence is equal to 1 – KM survival."))
              } else {
                  cifTable$setNote('single_cause', NULL)
              }

              for (cause in causes) {
                  col_idx <- which(cif_fit$states == cause)
                  if (length(col_idx) == 0) next

                  if (!is.null(cif_fit$strata)) {
                      strata_names <- names(cif_fit$strata)
                      strata_ends <- cumsum(cif_fit$strata)
                      strata_starts <- c(1, strata_ends[-length(strata_ends)] + 1)

                      for (si in seq_along(strata_names)) {
                          grp_name <- gsub("^.*=", "", strata_names[si])
                          st_range <- strata_starts[si]:strata_ends[si]
                          st_times <- cif_fit$time[st_range]
                          idx_match <- which(st_times <= t_eval)

                          if (length(idx_match) == 0) {
                              cif_val <- 0; se_val <- 0; lower_val <- 0; upper_val <- 0
                          } else {
                              best_idx <- strata_starts[si] - 1 + max(idx_match)
                              cif_val <- cif_fit$pstate[best_idx, col_idx]
                              se_val <- cif_fit$std.err[best_idx, col_idx]
                              lower_val <- if (!is.null(cif_fit$lower)) cif_fit$lower[best_idx, col_idx] else NA_real_
                              upper_val <- if (!is.null(cif_fit$upper)) cif_fit$upper[best_idx, col_idx] else NA_real_
                          }

                          n_grp <- cif_fit$n[si]
                          n_ev <- sum(dat_comp$..comp_event == cause & dat_comp$..group == grp_name, na.rm = TRUE)

                          row_key <- paste0(cause, "_", grp_name)
                          cifTable$addRow(rowKey = row_key, values = list(
                              group  = grp_name,
                              cause  = cause,
                              n      = as.integer(n_grp),
                              events = as.integer(n_ev),
                              cif    = cif_val,
                              se     = se_val,
                              lower  = lower_val,
                              upper  = upper_val
                          ))
                      }
                  } else {
                      st_times <- cif_fit$time
                      idx_match <- which(st_times <= t_eval)
                      if (length(idx_match) == 0) {
                          cif_val <- 0; se_val <- 0; lower_val <- 0; upper_val <- 0
                      } else {
                          best_idx <- max(idx_match)
                          cif_val <- cif_fit$pstate[best_idx, col_idx]
                          se_val <- cif_fit$std.err[best_idx, col_idx]
                          lower_val <- if (!is.null(cif_fit$lower)) cif_fit$lower[best_idx, col_idx] else NA_real_
                          upper_val <- if (!is.null(cif_fit$upper)) cif_fit$upper[best_idx, col_idx] else NA_real_
                      }

                      n_grp <- sum(cif_fit$n)
                      n_ev <- sum(dat_comp$..comp_event == cause, na.rm = TRUE)

                      row_key <- paste0(cause, "_All")
                      cifTable$addRow(rowKey = row_key, values = list(
                          group  = jmvcore::.("All"),
                          cause  = cause,
                          n      = as.integer(n_grp),
                          events = as.integer(n_ev),
                          cif    = cif_val,
                          se     = se_val,
                          lower  = lower_val,
                          upper  = upper_val
                      ))
                  }
              }
          }

          # 2. Cumulative Incidence Plot State
          if (self$options$showCifPlot && !inherits(cif_fit, "try-error")) {
              self$results$compRisksSection$cifPlot$setState(list(
                  fit = cif_fit,
                  causes = causes,
                  group_var = group_var,
                  time_var = time_var
              ))
          } else {
              self$results$compRisksSection$cifPlot$setState(NULL)
          }

          # 3. Gray's Test Table
          grayTable <- self$results$compRisksSection$grayTestTable
          grayTable$deleteRows()

          if (self$options$showGrayTest) {
              if (is.null(group_var)) {
                  grayTable$setNote('no_grp', .("A grouping variable with at least 2 levels is required for Gray's test."))
              } else if (length(unique(stats::na.omit(dat_comp$..group))) < 2) {
                  grayTable$setNote('few_grp', .("Grouping variable must have at least 2 levels for Gray's test."))
              } else {
                  grayTable$setNote('no_grp', NULL)
                  grayTable$setNote('few_grp', NULL)

                  for (cause in causes) {
                      status_012 <- ifelse(dat_comp$..comp_event == cause, 1L,
                                    ifelse(dat_comp$..comp_event == "censor", 0L, 2L))
                      gt <- private$.graysTest(dat_comp$..time, status_012, dat_comp$..group)
                      grayTable$addRow(rowKey = as.character(cause), values = list(
                          cause = as.character(cause),
                          stat  = gt$statistic,
                          df    = gt$df,
                          p     = gt$p.value
                      ))
                  }
              }
          }

          # 4. Fine-Gray Subdistribution Hazard Regression Table
          fgTable <- self$results$compRisksSection$fineGrayTable
          fgTable$deleteRows()

          if (self$options$showFineGray) {
              terms <- c()
              if (!is.null(group_var)) {
                  dat_comp$..group <- as.factor(dat_comp[[group_var]])
                  terms <- c(terms, "..group")
              }
              if (!is.null(covs) && length(covs) > 0) {
                  for (i in seq_along(covs)) {
                      col_v <- dat_comp[[covs[i]]]
                      if (is.factor(col_v) || is.character(col_v)) {
                          dat_comp[[paste0("..cov_", i, "_")]] <- as.factor(col_v)
                      } else {
                          dat_comp[[paste0("..cov_", i, "_")]] <- as.numeric(col_v)
                      }
                      terms <- c(terms, paste0("..cov_", i, "_"))
                  }
              }

              if (length(terms) == 0) {
                  fgTable$setNote('no_preds', .("At least one predictor (group or covariate) is required for Fine–Gray regression."))
              } else {
                  fgTable$setNote('no_preds', NULL)
                  target_etype <- as.character(event_val)
                  n_events_target <- sum(dat_comp$..comp_event == target_etype, na.rm = TRUE)

                  if (n_events_target == 0) {
                      fgTable$setNote('no_events', .("No events of the target cause observed for Fine–Gray regression."))
                  } else {
                      fgTable$setNote('no_events', NULL)

                      fg_vars <- c("..time", "..comp_event", terms)
                      if (has_tstart) fg_vars <- c(fg_vars, "..tstart")
                      if (has_cluster) fg_vars <- c(fg_vars, "..cluster")
                      dat_fg <- stats::na.omit(dat_comp[, fg_vars, drop = FALSE])

                      surv_fg_str <- if (has_tstart) "survival::Surv(..tstart, ..time, ..comp_event)" else "survival::Surv(..time, ..comp_event)"
                      fg_form_str <- paste(surv_fg_str, "~", paste(terms, collapse = " + "))
                      fg_formula <- stats::as.formula(fg_form_str)

                      id_arg <- if (has_cluster) dat_fg$..cluster else if (has_tstart) seq_len(nrow(dat_fg)) else NULL
                      pdata <- if (!is.null(id_arg)) {
                          try(survival::finegray(fg_formula, data = dat_fg, etype = target_etype, id = id_arg), silent = TRUE)
                      } else {
                          try(survival::finegray(fg_formula, data = dat_fg, etype = target_etype), silent = TRUE)
                      }

                      if (!inherits(pdata, "try-error")) {
                          cox_form_str <- paste("survival::Surv(fgstart, fgstop, fgstatus) ~", paste(terms, collapse = " + "))
                          cox_formula <- stats::as.formula(cox_form_str)

                          fg_fit <- if (has_cluster) {
                              try(survival::coxph(cox_formula, weight = fgwt, data = pdata, cluster = pdata$id), silent = TRUE)
                          } else {
                              try(survival::coxph(cox_formula, weight = fgwt, data = pdata), silent = TRUE)
                          }

                          if (!inherits(fg_fit, "try-error")) {
                              s_fg <- summary(fg_fit)
                              coef_mat <- s_fg$coefficients
                              conf_mat <- s_fg$conf.int

                              se_col <- if ("robust se" %in% colnames(coef_mat)) "robust se" else if ("se(coef)" %in% colnames(coef_mat)) "se(coef)" else 2
                              z_col <- if ("z" %in% colnames(coef_mat)) "z" else if ("t" %in% colnames(coef_mat)) "t" else 3
                              p_col <- if ("Pr(>|z|)" %in% colnames(coef_mat)) "Pr(>|z|)" else if ("Pr(>|t|)" %in% colnames(coef_mat)) "Pr(>|t|)" else ncol(coef_mat)

                              for (i in seq_len(nrow(coef_mat))) {
                                  row_v <- rownames(coef_mat)[i]
                                  clean_v <- row_v
                                  if (grepl("^\\.\\.group", row_v)) {
                                      lvl <- gsub("^\\.\\.group", "", row_v)
                                      clean_v <- paste0(group_var, " (", lvl, ")")
                                  } else if (grepl("^\\.\\.cov_([0-9]+)_", row_v)) {
                                      match_idx <- as.integer(gsub("^\\.\\.cov_([0-9]+)_.*", "\\1", row_v))
                                      rem_suffix <- gsub(paste0("^\\.\\.cov_", match_idx, "_"), "", row_v)
                                      cov_name <- covs[match_idx]
                                      clean_v <- if (rem_suffix != "") paste0(cov_name, " (", rem_suffix, ")") else cov_name
                                  }
                                  shr_lower <- if ("lower .95" %in% colnames(conf_mat)) conf_mat[i, "lower .95"] else conf_mat[i, 3]
                                  shr_upper <- if ("upper .95" %in% colnames(conf_mat)) conf_mat[i, "upper .95"] else conf_mat[i, 4]
                                  fgTable$addRow(rowKey = row_v, values = list(
                                      var      = clean_v,
                                      coef     = as.numeric(coef_mat[i, "coef"]),
                                      se       = as.numeric(coef_mat[i, se_col]),
                                      z        = as.numeric(coef_mat[i, z_col]),
                                      p        = as.numeric(coef_mat[i, p_col]),
                                      shr      = as.numeric(conf_mat[i, "exp(coef)"]),
                                      shrLower = as.numeric(shr_lower),
                                      shrUpper = as.numeric(shr_upper)
                                  ))
                              }

                              if (has_cluster) {
                                  fgTable$setNote("cluster_note", paste0(jmvcore::.("Standard errors and confidence intervals are adjusted for clustering by"), " ", subject_id_var, " (", jmvcore::.("Huber–White robust sandwich estimator"), ")."))
                              } else {
                                  fgTable$setNote("cluster_note", NULL)
                              }
                              if (has_tstart) {
                                  fgTable$setNote("cp_note", jmvcore::.("Counting process formulation with left-truncation (tstart, elapsed)."))
                              } else {
                                  fgTable$setNote("cp_note", NULL)
                              }

                              if (!is.null(s_fg$waldtest)) {
                                  stat_str <- sprintf("%.2f", s_fg$waldtest["test"])
                                  p_str <- format.pval(s_fg$waldtest["pvalue"], eps = 0.001)
                                  df_val <- as.integer(s_fg$waldtest["df"])
                                  fgTable$setNote('wald_note', jmvcore::format(
                                      .("Model fit (Wald test): \u03c7\u00b2 = {stat}, df = {df}, p = {p}; Events of interest = {nev}"),
                                      stat = stat_str, df = df_val, p = p_str, nev = s_fg$nevent))
                              }
                          } else {
                              fgTable$setNote('fit_error', .("Model fitting failed to converge."))
                          }
                      } else {
                          fgTable$setNote('finegray_error', .("Data weighting for Fine–Gray model failed."))
                      }
                  }
              }
          }
      },

      .cifPlot = function(image, ggtheme, theme, ...) {
          state <- image$state
          if (is.null(state)) return(FALSE)

          fit <- state$fit
          causes <- state$causes
          group_var <- state$group_var
          time_var <- state$time_var
          has_ci <- self$options$cifCI

          if (is.null(fit) || is.null(causes) || length(causes) == 0) return(FALSE)

          states <- fit$states
          if (!is.null(fit$strata)) {
              strata_rep <- rep(names(fit$strata), fit$strata)
              clean_strata <- gsub("^.*=", "", strata_rep)
          } else {
              clean_strata <- rep("All", length(fit$time))
          }

          plot_rows <- list()
          for (st in causes) {
              col_idx <- which(states == st)
              if (length(col_idx) == 0) next
              df_st <- data.frame(
                  time  = fit$time,
                  cif   = fit$pstate[, col_idx],
                  lower = if (!is.null(fit$lower)) fit$lower[, col_idx] else NA_real_,
                  upper = if (!is.null(fit$upper)) fit$upper[, col_idx] else NA_real_,
                  cause = st,
                  group = clean_strata,
                  stringsAsFactors = FALSE
              )
              plot_rows[[st]] <- df_st
          }

          if (length(plot_rows) == 0) return(FALSE)
          plot_data <- do.call(rbind, plot_rows)

          unique_grps <- unique(clean_strata)
          zero_rows <- expand.grid(
              time  = 0,
              cause = causes,
              group = unique_grps,
              stringsAsFactors = FALSE
          )
          zero_rows$cif <- 0
          zero_rows$lower <- 0
          zero_rows$upper <- 0

          full_plot_data <- rbind(zero_rows, plot_data)
          full_plot_data <- full_plot_data[order(full_plot_data$cause, full_plot_data$group, full_plot_data$time), ]

          limit_time <- self$options$kmMaxTime
          limit_val <- NULL
          if (!is.null(limit_time) && length(limit_time) > 0) {
              lv <- suppressWarnings(as.numeric(limit_time))
              if (!is.na(lv) && lv > 0) limit_val <- lv
          }
          if (!is.null(limit_val)) {
              full_plot_data <- full_plot_data[full_plot_data$time <= limit_val, ]
          }

          if (is.null(group_var)) {
              p <- ggplot2::ggplot(full_plot_data, ggplot2::aes(x = time, y = cif, color = cause, group = cause))
              if (has_ci) {
                  p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = cause),
                                                alpha = 0.2, linetype = 0)
              }
              p <- p + ggplot2::geom_step(linewidth = 0.8) +
                  ggplot2::labs(
                      x = time_var,
                      y = .("Cumulative Incidence"),
                      color = .("Cause"),
                      fill = .("Cause")
                  )
          } else {
              p <- ggplot2::ggplot(full_plot_data, ggplot2::aes(x = time, y = cif, color = group, group = group))
              if (has_ci) {
                  p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = group),
                                                alpha = 0.2, linetype = 0)
              }
              p <- p + ggplot2::geom_step(linewidth = 0.8) +
                  ggplot2::facet_wrap(~ cause) +
                  ggplot2::labs(
                      x = time_var,
                      y = .("Cumulative Incidence"),
                      color = group_var,
                      fill = group_var
                  )
          }

          p <- p + ggplot2::scale_y_continuous(
              labels = scales::percent_format(),
              expand = c(0.01, 0.01)
          )

          if (!is.null(limit_val)) {
              p <- p + ggplot2::coord_cartesian(xlim = c(0, limit_val))
          }

          p <- p + ggtheme + ggplot2::theme(legend.position = "bottom")
          print(p)
          return(TRUE)
      },

      .graysTest = function(time, status, group, rho = 0) {
          if (requireNamespace("cmprsk", quietly = TRUE)) {
              fit_c <- try(cmprsk::cuminc(ftime = time, fstatus = status, group = group, rho = rho), silent = TRUE)
              if (!inherits(fit_c, "try-error") && !is.null(fit_c$Tests)) {
                  tests <- fit_c$Tests
                  idx <- which(rownames(tests) %in% c("1", 1))
                  if (length(idx) > 0) {
                      return(list(
                          statistic = as.numeric(tests[idx[1], "stat"]),
                          df = as.integer(tests[idx[1], "df"]),
                          p.value = as.numeric(tests[idx[1], "pv"])
                      ))
                  }
              }
          }

          valid <- !is.na(time) & !is.na(status) & !is.na(group)
          time <- as.numeric(time[valid])
          status <- as.integer(status[valid])
          group <- as.factor(group[valid])

          groups <- levels(group)
          ng <- length(groups)
          if (ng < 2) {
              return(list(statistic = NA_real_, df = NA_integer_, p.value = NA_real_))
          }

          ig <- as.integer(group)
          no <- length(time)

          ord <- order(time)
          y <- time[ord]
          m <- status[ord]
          ig <- ig[ord]

          ng1 <- ng - 1
          s <- numeric(ng1)

          rs <- as.numeric(table(factor(ig, levels = 1:ng)))

          f1m <- numeric(ng)
          f1 <- numeric(ng)
          skmm <- rep(1.0, ng)
          skm <- rep(1.0, ng)
          v3 <- numeric(ng)
          v2 <- matrix(0.0, nrow = ng1, ncol = ng)
          c_mat <- matrix(0.0, nrow = ng, ncol = ng)
          a_mat <- matrix(0.0, nrow = ng, ncol = ng)
          v_packed <- numeric(ng * ng1 / 2)

          fm <- 0.0
          f <- 0.0

          ll <- 1
          while (ll <= no) {
              lu <- ll
              while (lu + 1 <= no && y[lu + 1] == y[ll]) {
                  lu <- lu + 1
              }

              d <- matrix(0L, nrow = 3, ncol = ng)
              for (i in ll:lu) {
                  j <- ig[i]
                  k <- m[i]
                  if (k >= 0 && k <= 2) {
                      d[k + 1, j] <- d[k + 1, j] + 1L
                  }
              }

              nd1 <- sum(d[2, ])
              nd2 <- sum(d[3, ])

              if (nd1 > 0 || nd2 > 0) {
                  tr <- 0.0
                  tq <- 0.0
                  for (i in 1:ng) {
                      if (rs[i] > 0) {
                          td <- d[2, i] + d[3, i]
                          skm[i] <- skmm[i] * (rs[i] - td) / rs[i]
                          f1[i] <- f1m[i] + (skmm[i] * d[2, i]) / rs[i]
                          tr <- tr + rs[i] / skmm[i]
                          tq <- tq + rs[i] * (1.0 - f1m[i]) / skmm[i]
                      }
                  }

                  f <- fm + nd1 / tr
                  fb <- (1.0 - fm)^rho

                  a_mat[,] <- 0.0
                  for (i in 1:ng) {
                      if (rs[i] > 0) {
                          t1 <- rs[i] / skmm[i]
                          a_mat[i, i] <- fb * t1 * (1.0 - t1 / tr)
                          if (a_mat[i, i] != 0.0 && (1.0 - fm) > 0) {
                              c_mat[i, i] <- c_mat[i, i] + a_mat[i, i] * nd1 / (tr * (1.0 - fm))
                          }
                          if (i + 1 <= ng) {
                              for (j in (i + 1):ng) {
                                  if (rs[j] > 0) {
                                      a_mat[i, j] <- -fb * t1 * rs[j] / (skmm[j] * tr)
                                      if (a_mat[i, j] != 0.0 && (1.0 - fm) > 0) {
                                          c_mat[i, j] <- c_mat[i, j] + a_mat[i, j] * nd1 / (tr * (1.0 - fm))
                                      }
                                  }
                              }
                          }
                      }
                  }

                  for (i in 2:ng) {
                      for (j in 1:(i - 1)) {
                          a_mat[i, j] <- a_mat[j, i]
                          c_mat[i, j] <- c_mat[j, i]
                      }
                  }

                  for (i in 1:ng1) {
                      if (rs[i] > 0 && tq > 0) {
                          s[i] <- s[i] + fb * (d[2, i] - nd1 * rs[i] * (1.0 - f1m[i]) / (skmm[i] * tq))
                      }
                  }

                  if (nd1 > 0) {
                      for (k in 1:ng) {
                          if (rs[k] > 0) {
                              t4 <- 1.0
                              if (skm[k] > 0) t4 <- 1.0 - (1.0 - f) / skm[k]
                              t5 <- 1.0
                              if (nd1 > 1) {
                                  denom <- tr * skmm[k] - 1.0
                                  if (denom > 0) t5 <- 1.0 - (nd1 - 1.0) / denom
                              }
                              t3 <- t5 * skmm[k] * nd1 / (tr * rs[k])
                              v3[k] <- v3[k] + t4 * t4 * t3
                              for (i in 1:ng1) {
                                  t1 <- a_mat[i, k] - t4 * c_mat[i, k]
                                  v2[i, k] <- v2[i, k] + t1 * t4 * t3
                                  for (j in 1:i) {
                                      l <- i * (i - 1) / 2 + j
                                      t2 <- a_mat[j, k] - t4 * c_mat[j, k]
                                      v_packed[l] <- v_packed[l] + t1 * t2 * t3
                                  }
                              }
                          }
                      }
                  }

                  if (nd2 > 0) {
                      for (k in 1:ng) {
                          if (skm[k] > 0 && d[3, k] > 0) {
                              t4 <- (1.0 - f) / skm[k]
                              t5 <- 1.0
                              if (d[3, k] > 1 && rs[k] > 1) {
                                  t5 <- 1.0 - (d[3, k] - 1.0) / (rs[k] - 1.0)
                              }
                              t6 <- rs[k]
                              t3 <- t5 * (skmm[k]^2 * d[3, k]) / (t6^2)
                              v3[k] <- v3[k] + t4 * t4 * t3
                              for (i in 1:ng1) {
                                  t1 <- t4 * c_mat[i, k]
                                  v2[i, k] <- v2[i, k] - t1 * t4 * t3
                                  for (j in 1:i) {
                                      l <- i * (i - 1) / 2 + j
                                      t2 <- t4 * c_mat[j, k]
                                      v_packed[l] <- v_packed[l] + t1 * t2 * t3
                                  }
                              }
                          }
                      }
                  }
              }

              for (i in ll:lu) {
                  j <- ig[i]
                  rs[j] <- rs[j] - 1
              }
              fm <- f
              for (i in 1:ng) {
                  f1m[i] <- f1[i]
                  skmm[i] <- skm[i]
              }

              ll <- lu + 1
          }

          for (i in 1:ng1) {
              for (j in 1:i) {
                  l <- i * (i - 1) / 2 + j
                  for (k in 1:ng) {
                      v_packed[l] <- v_packed[l] + c_mat[i, k] * c_mat[j, k] * v3[k] +
                                                   c_mat[i, k] * v2[j, k] +
                                                   c_mat[j, k] * v2[i, k]
                  }
              }
          }

          vs <- matrix(0.0, nrow = ng1, ncol = ng1)
          for (i in 1:ng1) {
              for (j in 1:i) {
                  l <- i * (i - 1) / 2 + j
                  vs[i, j] <- v_packed[l]
                  vs[j, i] <- v_packed[l]
              }
          }

          stat <- tryCatch({
              as.numeric(t(s) %*% solve(vs, s))
          }, error = function(e) {
              NA_real_
          })

          df <- ng1
          pval <- if (!is.na(stat) && stat >= 0) stats::pchisq(stat, df = df, lower.tail = FALSE) else NA_real_

          list(
              statistic = stat,
              df = df,
              p.value = pval
          )
      },

      .runTimeDepCox = function(dat, time_var, tstart_var, subject_id_var, group_var, covs, has_tstart, has_cluster) {
          timeDepTable <- self$results$coxSection$timeDepTable
          timeDepTable$deleteRows()

          td_vars <- self$options$timeDepVars
          if (length(td_vars) == 0) {
              timeDepTable$setNote('no_vars', jmvcore::.("Please specify one or more time-dependent covariates."))
              return()
          }
          timeDepTable$setNote('no_vars', NULL)

          all_preds <- unique(c(if (!is.null(group_var)) group_var else character(0), covs, td_vars))
          all_preds <- intersect(all_preds, names(dat))

          if (length(all_preds) == 0) {
              timeDepTable$setNote('no_preds', jmvcore::.("No valid predictors found for time-dependent Cox model."))
              return()
          }
          timeDepTable$setNote('no_preds', NULL)

          cols_needed <- c("..time", "..event", all_preds)
          if (has_tstart) cols_needed <- c(cols_needed, "..tstart")
          if (has_cluster) cols_needed <- c(cols_needed, "..cluster")

          dat_sub <- dat[stats::complete.cases(dat[, cols_needed, drop = FALSE]), ]
          if (nrow(dat_sub) == 0) {
              timeDepTable$setNote('no_data', jmvcore::.("No complete cases available for time-dependent Cox model."))
              return()
          }
          timeDepTable$setNote('no_data', NULL)

          time_func_opt <- self$options$timeDepFunc
          if (time_func_opt == "linear") {
              tt_fun <- function(x, t, ...) x * t
              fn_suffix <- " \u00d7 t"
          } else {
              tt_fun <- function(x, t, ...) x * log(pmax(t, 1e-5))
              fn_suffix <- " \u00d7 log(t)"
          }

          terms <- c()
          clean_names_map <- list()
          var_idx <- 0

          for (pv in all_preds) {
              var_idx <- var_idx + 1
              col_data <- dat_sub[[pv]]
              is_td <- pv %in% td_vars

              if (is.factor(col_data) || is.character(col_data)) {
                  f_data <- as.factor(col_data)
                  lvls <- levels(f_data)
                  ref_lvl <- lvls[1]

                  if (is_td) {
                      for (k in 2:length(lvls)) {
                          cur_lvl <- lvls[k]
                          dummy_col <- paste0("..td_d_", var_idx, "_", k)
                          dat_sub[[dummy_col]] <- as.numeric(f_data == cur_lvl)
                          terms <- c(terms, dummy_col, paste0("tt(", dummy_col, ")"))
                          clean_names_map[[dummy_col]] <- paste0(pv, " (", cur_lvl, " ", jmvcore::.("vs"), " ", ref_lvl, ")")
                          clean_names_map[[paste0("tt(", dummy_col, ")")]] <- paste0(pv, " (", cur_lvl, " ", jmvcore::.("vs"), " ", ref_lvl, ")", fn_suffix)
                      }
                  } else {
                      fact_col <- paste0("..td_f_", var_idx)
                      dat_sub[[fact_col]] <- f_data
                      terms <- c(terms, fact_col)
                      for (k in 2:length(lvls)) {
                          cur_lvl <- lvls[k]
                          clean_names_map[[paste0(fact_col, cur_lvl)]] <- paste0(pv, " (", cur_lvl, " ", jmvcore::.("vs"), " ", ref_lvl, ")")
                      }
                  }
              } else {
                  num_col <- paste0("..td_n_", var_idx)
                  dat_sub[[num_col]] <- as.numeric(col_data)
                  if (is_td) {
                      terms <- c(terms, num_col, paste0("tt(", num_col, ")"))
                      clean_names_map[[num_col]] <- paste0(pv, " (", jmvcore::.("baseline"), ")")
                      clean_names_map[[paste0("tt(", num_col, ")")]] <- paste0(pv, fn_suffix)
                  } else {
                      terms <- c(terms, num_col)
                      clean_names_map[[num_col]] <- pv
                  }
              }
          }

          surv_str <- if (has_tstart) {
              "survival::Surv(..tstart, ..time, ..event)"
          } else {
              "survival::Surv(..time, ..event)"
          }
          formula_str <- paste(surv_str, "~", paste(terms, collapse = " + "))
          cluster_arg <- if (has_cluster) dat_sub$..cluster else NULL

          td_fit <- try(survival::coxph(as.formula(formula_str), data = dat_sub, tt = tt_fun, cluster = cluster_arg, x = TRUE), silent = TRUE)

          if (inherits(td_fit, "try-error")) {
              err_msg <- as.character(attr(td_fit, "condition")$message)
              timeDepTable$setNote('fit_error', paste0(jmvcore::.("Model fitting failed to converge:"), " ", err_msg))
              return()
          }
          timeDepTable$setNote('fit_error', NULL)

          s_td <- summary(td_fit)
          coef_mat <- s_td$coefficients
          conf_mat <- s_td$conf.int
          se_col <- if ("robust se" %in% colnames(coef_mat)) "robust se" else "se(coef)"

          for (i in seq_along(rownames(coef_mat))) {
              raw_name <- rownames(coef_mat)[i]
              display_name <- if (!is.null(clean_names_map[[raw_name]])) clean_names_map[[raw_name]] else raw_name
              timeDepTable$addRow(rowKey = raw_name, values = list(
                  var     = display_name,
                  coef    = as.numeric(coef_mat[i, "coef"]),
                  se      = as.numeric(coef_mat[i, se_col]),
                  z       = as.numeric(coef_mat[i, "z"]),
                  p       = as.numeric(coef_mat[i, "Pr(>|z|)"]),
                  hr      = as.numeric(conf_mat[i, "exp(coef)"]),
                  hrLower = as.numeric(conf_mat[i, "lower .95"]),
                  hrUpper = as.numeric(conf_mat[i, "upper .95"])
              ))
          }

          if (has_cluster) {
              timeDepTable$setNote('cluster_note', paste0(jmvcore::.("Standard errors and confidence intervals are adjusted for clustering by"), " ", subject_id_var, " (", jmvcore::.("Huber–White robust sandwich estimator"), ")."))
          } else {
              timeDepTable$setNote('cluster_note', NULL)
          }

          if (has_tstart) {
              timeDepTable$setNote('cp_note', jmvcore::.("Counting process formulation with left-truncation (tstart, elapsed)."))
          } else {
              timeDepTable$setNote('cp_note', NULL)
          }
      }
    )
)
