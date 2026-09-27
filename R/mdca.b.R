# This file is a generated template, your changes will not be overwritten

mDCAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mDCAClass",
    inherit = mDCABase,
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
        .ensureData = function() {
            if (is.null(private$.data) || nrow(private$.data) == 0) {
                d <- tryCatch(self$readDataset(headerOnly = FALSE), error = function(e) NULL)
                if (!is.null(d) && nrow(d) > 0)
                    private$.data <- d
            }
            return(!is.null(private$.data) && nrow(private$.data) > 0)
        },

        .init = function() {
            # Initial instructions
            self$results$instructions$setContent(
                paste0(
                    "<p>",
                    .("Decision Curve Analysis (DCA) evaluates the clinical net benefit of prediction models and biomarkers across a range of decision thresholds."),
                    "</p><p><b>",
                    .("Getting started:"),
                    "</b> ",
                    .("Select a binary target variable (Outcome), target event level, and at least one model probability column (values between 0 and 1)."),
                    "</p>"
                )
            )

            # Initialize explanatory table notes
            private$.initNotes()
        },

        .run = function() {
            dep <- self$options$dep
            probs <- self$options$probs
            scores <- self$options$scores
            all_preds <- c(probs, scores)

            if (is.null(dep) || length(all_preds) == 0) {
                self$results$instructions$setVisible(TRUE)
                return()
            }

            self$results$instructions$setVisible(FALSE)

            # Extract data
            dat <- data.frame(self$data, check.names=FALSE)
            needed_vars <- c(dep, all_preds)
            dat <- dat[stats::complete.cases(dat[, needed_vars, drop=FALSE]), , drop=FALSE]

            if (nrow(dat) < 10) {
                jmvcore::reject(.("Too few valid observations for Decision Curve Analysis (at least 10 required)."))
            }

            # Target variable handling
            dep_factor <- as.factor(dat[[dep]])
            levels_dep <- levels(dep_factor)

            target_level <- self$options$targetLevel
            if (is.null(target_level) || !target_level %in% levels_dep) {
                # Auto-select target level (prefer "1", "Event", "Yes", "Positive", or second level)
                pref_idx <- grep("^(1|event|yes|pos|true)$", levels_dep, ignore.case=TRUE)
                if (length(pref_idx) > 0) {
                    target_level <- levels_dep[pref_idx[1]]
                } else {
                    target_level <- if (length(levels_dep) >= 2) levels_dep[2] else levels_dep[1]
                }
            }

            y <- as.numeric(dep_factor == target_level)
            n <- length(y)
            n_events <- sum(y)
            prevalence <- n_events / n

            if (n_events == 0 || n_events == n) {
                jmvcore::reject(.("Target variable must contain both positive and negative cases."))
            }

            # Process model probabilities and continuous scores
            model_probs <- list()

            # 1. Direct probabilities
            for (p_var in probs) {
                raw_p <- as.numeric(dat[[p_var]])
                if (any(raw_p < 0 | raw_p > 1, na.rm=TRUE)) {
                    jmvcore::reject(jmvcore::format(.("Values of '{var}' must be probabilities between 0 and 1."), var=p_var))
                }

                if (self$options$recalibrate) {
                    # Platt scaling / logistic recalibration
                    eps <- 1e-4
                    p_adj <- pmax(pmin(raw_p, 1 - eps), eps)
                    logit_p <- stats::qlogis(p_adj)
                    fit <- tryCatch(
                        stats::glm(y ~ logit_p, family=stats::binomial()),
                        error=function(e) NULL
                    )
                    if (!is.null(fit)) {
                        model_probs[[p_var]] <- as.numeric(stats::predict(fit, type="response"))
                    } else {
                        model_probs[[p_var]] <- raw_p
                    }
                } else {
                    model_probs[[p_var]] <- raw_p
                }
            }

            # 2. Continuous scores (converted to probabilities via univariate logistic model)
            for (s_var in scores) {
                raw_s <- as.numeric(dat[[s_var]])
                fit_score <- tryCatch(
                    stats::glm(y ~ raw_s, family=stats::binomial()),
                    error=function(e) NULL
                )
                if (!is.null(fit_score)) {
                    label <- paste0(s_var, " (", .("Score"), ")")
                    model_probs[[label]] <- as.numeric(stats::predict(fit_score, type="response"))
                }
            }

            # Threshold grid
            t_min <- self$options$threshMin
            t_max <- self$options$threshMax
            t_step <- self$options$threshStep
            harm <- self$options$harm

            thresholds <- seq(t_min, t_max, by=t_step)
            n_thresh <- length(thresholds)

            # Calculate Net Benefit for each threshold
            # Treat None: NB = 0
            # Treat All: NB = prevalence - (1 - prevalence) * (p_t / (1 - p_t)) - harm
            odds <- thresholds / (1 - thresholds)
            nb_treat_all <- prevalence - (1 - prevalence) * odds - harm
            nb_treat_none <- rep(0, n_thresh)

            dca_results <- list(
                thresholds = thresholds,
                prevalence = prevalence,
                n = n,
                harm = harm,
                nb_treat_all = nb_treat_all,
                nb_treat_none = nb_treat_none,
                models = list()
            )

            for (m_name in names(model_probs)) {
                private$.checkpoint()
                p_vec <- model_probs[[m_name]]
                nb_vec <- numeric(n_thresh)
                avoided_vec <- numeric(n_thresh)

                for (ti in seq_along(thresholds)) {
                    pt <- thresholds[ti]
                    weight <- pt / (1 - pt)

                    pred_pos <- (p_vec >= pt)
                    tp <- sum(pred_pos & (y == 1))
                    fp <- sum(pred_pos & (y == 0))

                    nb <- (tp / n) - (fp / n) * weight - harm
                    nb_vec[ti] <- nb

                    # Interventions avoided per 100 patients: (NB_model - NB_all) / (pt / (1 - pt)) * 100
                    diff_nb <- nb - nb_treat_all[ti]
                    avoided <- (diff_nb / weight) * 100
                    avoided_vec[ti] <- pmax(avoided, 0)
                }

                # Clinical Decision Window: range where NB > max(NB_all, 0)
                superior <- (nb_vec > pmax(nb_treat_all, 0) + 1e-4)
                if (any(superior)) {
                    sup_thresh <- thresholds[superior]
                    win_min <- min(sup_thresh) * 100
                    win_max <- max(sup_thresh) * 100
                    win_span <- win_max - win_min
                    status <- .("Superior")
                } else {
                    win_min <- NA
                    win_max <- NA
                    win_span <- 0
                    status <- .("No clinical gain")
                }

                # Interventions avoided at 20%
                idx_20 <- which.min(abs(thresholds - 0.20))
                avoid_20_val <- avoided_vec[idx_20]

                # Optimal threshold (max Net Benefit)
                max_nb_idx <- which.max(nb_vec)
                max_nb_val <- nb_vec[max_nb_idx]
                opt_thresh_val <- thresholds[max_nb_idx]

                dca_results$models[[m_name]] <- list(
                    p_vec = p_vec,
                    nb = nb_vec,
                    avoided = avoided_vec,
                    win_min = win_min,
                    win_max = win_max,
                    win_span = win_span,
                    avoid_20 = avoid_20_val,
                    status = status,
                    max_nb = max_nb_val,
                    opt_thresh = opt_thresh_val
                )
            }

            # Save state for plots
            image_dca <- self$results$dcaPlot
            image_dca$setState(dca_results)

            image_avoid <- self$results$avoidedPlot
            image_avoid$setState(dca_results)

            image_cal <- self$results$calPlot
            image_cal$setState(list(dca_results=dca_results, y=y))

            # Populate tables
            if (self$options$showDcaTable) {
                private$.populateDcaTable(dca_results)
            }

            if (self$options$showWindowTable) {
                private$.populateWindowTable(dca_results)
            }

            if (self$options$showCalTable) {
                private$.populateCalTable(dca_results, y)
            }
        },

        .initNotes = function() {
            # 1. dcaTable notes
            dcaTable <- self$results$dcaTable
            dcaTable$setNote("nb_def", .("<b>Net Benefit (NB)</b>: clinical utility calculated as NB = (TP/N) - (FP/N) * [pt / (1 - pt)] - harm. Represents the equivalent number of true positive diagnoses per patient without increasing unnecessary false positive interventions. The weighting factor [pt / (1 - pt)] reflects the relative harm of a false positive vs the benefit of a true positive."))
            dcaTable$setNote("treat_all", .("<b>Treat All</b>: strategy of offering intervention to all patients without diagnostic testing (NB = Prevalence - (1 - Prevalence) * [pt / (1 - pt)] - harm). Yields high net benefit at low thresholds, but drops rapidly with increasing pt and produces net clinical harm when unnecessary interventions outweigh true detections."))
            dcaTable$setNote("treat_none", .("<b>Treat None</b>: strategy of offering intervention to no patients. Net Benefit is identically zero (NB = 0) across all decision thresholds pt, as there are neither true positive benefits nor false positive harms."))
            dcaTable$setNote("clin_rule", .("<b>Clinical Selection Rule</b>: a model is clinically useful at threshold pt if its Net Benefit is strictly higher than both default strategies (NB_model > NB_all and NB_model > 0). Among competing models, the one with the highest Net Benefit at the clinically acceptable threshold should be chosen."))
            dcaTable$setNote("opt_thresh", .("<b>Optimal Threshold</b>: decision threshold pt at which the model achieves its maximum Net Benefit (Max Net Benefit). Balances detection of true events with avoidance of unnecessary interventions in the evaluated cohort."))

            # 2. windowTable notes
            windowTable <- self$results$windowTable
            windowTable$setNote("win_def", .("<b>Clinical Decision Window</b>: range of decision thresholds [Min %, Max %] where the model yields higher Net Benefit than both default strategies (NB_model > max(NB_all, 0)). Below Min %, Treat All is preferred; above Max %, Treat None is preferred."))
            windowTable$setNote("win_span", .("<b>Window Span</b>: width of the clinical decision window (Max % - Min %). A wider span (> 20-30%) demonstrates robustness of the model's clinical utility across diverse practitioner and patient risk tolerances."))
            windowTable$setNote("avoid_def", .("<b>Avoided at 20%</b>: reduction in unnecessary interventions per 100 patients compared to Treat All, without missing true positive cases: (NB_model - NB_all) / [pt / (1 - pt)] * 100 evaluated at pt = 0.20."))
            windowTable$setNote("status_def", .("<b>Clinical Superiority</b>: overall verdict on model utility. 'Superior' indicates that the model provides clinical gain over both default strategies across a meaningful threshold range; 'No clinical gain' indicates that testing offers no improvement over default strategies."))

            # 3. calTable notes
            calTable <- self$results$calTable
            calTable$setNote("brier_def", .("<b>Brier Score</b>: mean squared error between predicted probabilities and binary outcomes: BS = (1/N) * sum((p - y)^2). Overall accuracy metric combining discrimination and calibration (0 = perfect, 1 = worst). Values below the cohort incidence indicate informative predictions."))
            calTable$setNote("slope_def", .("<b>Calibration Slope</b>: regression slope of log-odds predicted risk. Ideal value is 1.000. Values < 1 indicate overfitting (risk overestimation in high-risk patients and underestimation in low-risk patients), while values > 1 indicate underfitting (predictions shrunk toward cohort incidence)."))
            calTable$setNote("intercept_def", .("<b>Calibration Intercept</b>: calibration-in-the-large reflecting systematic over- or underestimation with slope fixed to 1. Ideal value is 0.000. Negative values indicate systematic overestimation of cohort risk; positive values indicate underestimation."))
            calTable$setNote("e_avg_def", .("<b>E avg</b>: average absolute difference between non-parametric LOESS-calibrated probability and predicted risk across all patients. Reflects average individual risk miscalibration."))
            calTable$setNote("e_max_def", .("<b>E max</b>: maximum absolute difference between LOESS-calibrated probability and predicted risk across the risk spectrum. Reflects the worst-case local probability error."))
        },

        .populateDcaTable = function(res) {
            table <- self$results$dcaTable
            table$deleteRows()

            # Explanatory notes
            table$setNote("nb_def", .("<b>Net Benefit (NB)</b>: clinical utility calculated as NB = (TP/N) - (FP/N) * [pt / (1 - pt)] - harm. Represents the equivalent number of true positive diagnoses per patient without increasing unnecessary false positive interventions. The weighting factor [pt / (1 - pt)] reflects the relative harm of a false positive vs the benefit of a true positive."))
            table$setNote("treat_all", .("<b>Treat All</b>: strategy of offering intervention to all patients without diagnostic testing (NB = Prevalence - (1 - Prevalence) * [pt / (1 - pt)] - harm). Yields high net benefit at low thresholds, but drops rapidly with increasing pt and produces net clinical harm when unnecessary interventions outweigh true detections."))
            table$setNote("treat_none", .("<b>Treat None</b>: strategy of offering intervention to no patients. Net Benefit is identically zero (NB = 0) across all decision thresholds pt, as there are neither true positive benefits nor false positive harms."))
            table$setNote("clin_rule", .("<b>Clinical Selection Rule</b>: a model is clinically useful at threshold pt if its Net Benefit is strictly higher than both default strategies (NB_model > NB_all and NB_model > 0). Among competing models, the one with the highest Net Benefit at the clinically acceptable threshold should be chosen."))
            table$setNote("opt_thresh", .("<b>Optimal Threshold</b>: decision threshold pt at which the model achieves its maximum Net Benefit (Max Net Benefit). Balances detection of true events with avoidance of unnecessary interventions in the evaluated cohort."))

            if (self$options$harm > 0) {
                table$setNote("harm_note", jmvcore::format(.("<b>Test Harm</b>: a penalty of {harm} Net Benefit units is deducted per patient to account for financial costs, invasiveness, discomfort, or procedural complications of testing."), harm = self$options$harm))
            } else {
                table$setNote("harm_note", NULL)
            }

            thresholds <- res$thresholds

            get_nb_at <- function(nb_vec, t_target) {
                idx <- which.min(abs(thresholds - t_target))
                if (abs(thresholds[idx] - t_target) <= 0.03) {
                    return(nb_vec[idx])
                }
                return(NA)
            }

            # 1. Treat All row
            table$addRow(rowKey="Treat All", values=list(
                model = .("Treat All"),
                nb_10 = get_nb_at(res$nb_treat_all, 0.10),
                nb_20 = get_nb_at(res$nb_treat_all, 0.20),
                nb_30 = get_nb_at(res$nb_treat_all, 0.30),
                nb_40 = get_nb_at(res$nb_treat_all, 0.40),
                max_nb = max(res$nb_treat_all),
                opt_thresh = thresholds[which.max(res$nb_treat_all)]
            ))

            # 2. Treat None row
            table$addRow(rowKey="Treat None", values=list(
                model = .("Treat None"),
                nb_10 = 0,
                nb_20 = 0,
                nb_30 = 0,
                nb_40 = 0,
                max_nb = 0,
                opt_thresh = 0
            ))

            # 3. Model rows
            for (m_name in names(res$models)) {
                m_info <- res$models[[m_name]]
                table$addRow(rowKey=m_name, values=list(
                    model = m_name,
                    nb_10 = get_nb_at(m_info$nb, 0.10),
                    nb_20 = get_nb_at(m_info$nb, 0.20),
                    nb_30 = get_nb_at(m_info$nb, 0.30),
                    nb_40 = get_nb_at(m_info$nb, 0.40),
                    max_nb = m_info$max_nb,
                    opt_thresh = m_info$opt_thresh
                ))
            }
        },

        .populateWindowTable = function(res) {
            table <- self$results$windowTable
            table$deleteRows()

            # Explanatory notes
            table$setNote("win_def", .("<b>Clinical Decision Window</b>: range of decision thresholds [Min %, Max %] where the model yields higher Net Benefit than both default strategies (NB_model > max(NB_all, 0)). Below Min %, Treat All is preferred; above Max %, Treat None is preferred."))
            table$setNote("win_span", .("<b>Window Span</b>: width of the clinical decision window (Max % - Min %). A wider span (> 20-30%) demonstrates robustness of the model's clinical utility across diverse practitioner and patient risk tolerances."))
            table$setNote("avoid_def", .("<b>Avoided at 20%</b>: reduction in unnecessary interventions per 100 patients compared to Treat All, without missing true positive cases: (NB_model - NB_all) / [pt / (1 - pt)] * 100 evaluated at pt = 0.20."))
            table$setNote("status_def", .("<b>Clinical Superiority</b>: overall verdict on model utility. 'Superior' indicates that the model provides clinical gain over both default strategies across a meaningful threshold range; 'No clinical gain' indicates that testing offers no improvement over default strategies."))

            for (m_name in names(res$models)) {
                m_info <- res$models[[m_name]]
                table$addRow(rowKey=m_name, values=list(
                    model = m_name,
                    min_thresh = m_info$win_min,
                    max_thresh = m_info$win_max,
                    window_span = m_info$win_span,
                    avoid_20 = m_info$avoid_20,
                    status = m_info$status
                ))
            }
        },

        .populateCalTable = function(res, y) {
            table <- self$results$calTable
            table$deleteRows()

            # Explanatory notes
            table$setNote("brier_def", .("<b>Brier Score</b>: mean squared error between predicted probabilities and binary outcomes: BS = (1/N) * sum((p - y)^2). Overall accuracy metric combining discrimination and calibration (0 = perfect, 1 = worst). Values below the cohort incidence indicate informative predictions."))
            table$setNote("slope_def", .("<b>Calibration Slope</b>: regression slope of log-odds predicted risk. Ideal value is 1.000. Values < 1 indicate overfitting (risk overestimation in high-risk patients and underestimation in low-risk patients), while values > 1 indicate underfitting (predictions shrunk toward cohort incidence)."))
            table$setNote("intercept_def", .("<b>Calibration Intercept</b>: calibration-in-the-large reflecting systematic over- or underestimation with slope fixed to 1. Ideal value is 0.000. Negative values indicate systematic overestimation of cohort risk; positive values indicate underestimation."))
            table$setNote("e_avg_def", .("<b>E avg</b>: average absolute difference between non-parametric LOESS-calibrated probability and predicted risk across all patients. Reflects average individual risk miscalibration."))
            table$setNote("e_max_def", .("<b>E max</b>: maximum absolute difference between LOESS-calibrated probability and predicted risk across the risk spectrum. Reflects the worst-case local probability error."))

            if (self$options$recalibrate) {
                table$setNote("recal_note", .("<b>Recalibration</b>: Platt scaling (logistic calibration) applied to input probabilities to remove systematic bias, adjust slope toward 1.000, and align intercept to 0.000 while preserving ROC discrimination."))
            } else {
                table$setNote("recal_note", NULL)
            }

            n <- length(y)

            for (m_name in names(res$models)) {
                p <- res$models[[m_name]]$p_vec

                # Brier score
                brier <- mean((p - y)^2)

                # Calibration slope and intercept via logistic regression
                eps <- 1e-4
                p_clip <- pmax(pmin(p, 1 - eps), eps)
                logit_p <- stats::qlogis(p_clip)

                # Slope
                fit_slope <- tryCatch(
                    stats::glm(y ~ logit_p, family=stats::binomial()),
                    error=function(e) NULL
                )
                slope <- if (!is.null(fit_slope)) stats::coef(fit_slope)[2] else NA

                # Intercept with slope fixed to 1
                fit_int <- tryCatch(
                    stats::glm(y ~ 1 + offset(logit_p), family=stats::binomial()),
                    error=function(e) NULL
                )
                intercept <- if (!is.null(fit_int)) stats::coef(fit_int)[1] else NA

                # E_avg and E_max
                fit_loess <- tryCatch(
                    stats::loess(y ~ p, degree=1, span=0.75),
                    error=function(e) NULL
                )
                if (!is.null(fit_loess)) {
                    p_cal <- stats::predict(fit_loess, p)
                    e_diff <- abs(p_cal - p)
                    e_avg <- mean(e_diff, na.rm=TRUE)
                    e_max <- max(e_diff, na.rm=TRUE)
                } else {
                    e_avg <- NA
                    e_max <- NA
                }

                table$addRow(rowKey=m_name, values=list(
                    model = m_name,
                    brier = brier,
                    slope = as.numeric(slope),
                    intercept = as.numeric(intercept),
                    e_avg = as.numeric(e_avg),
                    e_max = as.numeric(e_max)
                ))
            }
        },

        .dcaPlot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res)) return(FALSE)

            t <- res$thresholds
            plot_data <- data.frame()

            # Treat All
            plot_data <- rbind(plot_data, data.frame(
                Threshold = t,
                NetBenefit = res$nb_treat_all,
                Strategy = .("Treat All"),
                Type = "Reference"
            ))

            # Treat None
            plot_data <- rbind(plot_data, data.frame(
                Threshold = t,
                NetBenefit = res$nb_treat_none,
                Strategy = .("Treat None"),
                Type = "Reference"
            ))

            # Models
            for (m_name in names(res$models)) {
                plot_data <- rbind(plot_data, data.frame(
                    Threshold = t,
                    NetBenefit = res$models[[m_name]]$nb,
                    Strategy = m_name,
                    Type = "Model"
                ))
            }

            # Filter out extreme negative values for clean visualization
            y_min <- max(-0.05, min(plot_data$NetBenefit, na.rm=TRUE))
            y_max <- max(plot_data$NetBenefit, na.rm=TRUE) * 1.15
            if (y_max <= 0) y_max <- 0.1

            pal <- self$options$palBrewer
            if (is.null(pal) || pal == "none") pal <- "Dark2"

            model_names <- names(res$models)
            n_models <- length(model_names)
            colors <- RColorBrewer::brewer.pal(max(3, n_models), pal)[seq_len(n_models)]
            names(colors) <- model_names

            all_colors <- c("Treat All"="#7F8C8D", "Treat None"="#2C3E50", colors)
            names(all_colors)[1] <- .("Treat All")
            names(all_colors)[2] <- .("Treat None")

            all_linetypes <- c("dashed", "dotted", rep("solid", n_models))
            names(all_linetypes) <- c(.("Treat All"), .("Treat None"), model_names)

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x=Threshold, y=NetBenefit, color=Strategy, linetype=Strategy)) +
                ggplot2::geom_line(size=1.0) +
                ggplot2::scale_color_manual(values=all_colors) +
                ggplot2::scale_linetype_manual(values=all_linetypes) +
                ggplot2::coord_cartesian(ylim=c(y_min, y_max)) +
                ggplot2::scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
                ggplot2::labs(
                    title=.("Decision Curve Analysis (Net Benefit)"),
                    x=.("Threshold Probability"),
                    y=.("Net Benefit"),
                    color=.("Strategy / Model"),
                    linetype=.("Strategy / Model")
                ) +
                ggplot2::theme_bw(base_size=12) +
                ggplot2::theme(
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.title=ggplot2::element_text(face="bold", color="#1B365D", hjust=0.5),
                    panel.grid.minor=ggplot2::element_blank()
                )

            print(p)
            return(TRUE)
        },

        .avoidedPlot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res)) return(FALSE)

            t <- res$thresholds
            plot_data <- data.frame()

            for (m_name in names(res$models)) {
                plot_data <- rbind(plot_data, data.frame(
                    Threshold = t,
                    Avoided = res$models[[m_name]]$avoided,
                    Model = m_name
                ))
            }

            if (nrow(plot_data) == 0) return(FALSE)

            pal <- self$options$palBrewer
            if (is.null(pal) || pal == "none") pal <- "Dark2"

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x=Threshold, y=Avoided, color=Model)) +
                ggplot2::geom_line(size=1.0) +
                ggplot2::scale_color_brewer(palette=pal) +
                ggplot2::scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
                ggplot2::labs(
                    title=.("Net Reduction in Interventions"),
                    x=.("Threshold Probability"),
                    y=.("Interventions Avoided (per 100 patients)"),
                    color=.("Model")
                ) +
                ggplot2::theme_bw(base_size=12) +
                ggplot2::theme(
                    legend.position="bottom",
                    plot.title=ggplot2::element_text(face="bold", color="#1B365D", hjust=0.5),
                    panel.grid.minor=ggplot2::element_blank()
                )

            print(p)
            return(TRUE)
        },

        .calPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            res <- state$dca_results
            y <- state$y
            bins <- self$options$calBins

            plot_points <- data.frame()

            for (m_name in names(res$models)) {
                p <- res$models[[m_name]]$p_vec

                # Quantile cut into bins
                cuts <- stats::quantile(p, probs=seq(0, 1, length.out=bins + 1))
                cuts[1] <- cuts[1] - 1e-5
                cuts[length(cuts)] <- cuts[length(cuts)] + 1e-5
                grp <- cut(p, breaks=unique(cuts), include.lowest=TRUE)

                bin_means_p <- tapply(p, grp, mean)
                bin_means_y <- tapply(y, grp, mean)
                bin_counts <- tapply(y, grp, length)

                for (b in seq_along(bin_means_p)) {
                    if (!is.na(bin_means_p[b]) && !is.na(bin_means_y[b])) {
                        k <- sum(y[grp == levels(grp)[b]])
                        n_b <- bin_counts[b]
                        ci <- tryCatch(
                            stats::binom.test(k, n_b)$conf.int,
                            error=function(e) c(bin_means_y[b], bin_means_y[b])
                        )
                        plot_points <- rbind(plot_points, data.frame(
                            Pred = bin_means_p[b],
                            Obs = bin_means_y[b],
                            ObsLow = ci[1],
                            ObsHigh = ci[2],
                            Model = m_name
                        ))
                    }
                }
            }

            if (nrow(plot_points) == 0) return(FALSE)

            pal <- self$options$palBrewer
            if (is.null(pal) || pal == "none") pal <- "Dark2"

            p <- ggplot2::ggplot(plot_points, ggplot2::aes(x=Pred, y=Obs, color=Model)) +
                ggplot2::geom_abline(intercept=0, slope=1, linetype="dashed", color="#7F8C8D", size=0.8) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin=ObsLow, ymax=ObsHigh), width=0.02, alpha=0.6) +
                ggplot2::geom_point(size=2.5) +
                ggplot2::geom_line(alpha=0.8) +
                ggplot2::scale_color_brewer(palette=pal) +
                ggplot2::coord_cartesian(xlim=c(0, 1), ylim=c(0, 1)) +
                ggplot2::scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
                ggplot2::scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
                ggplot2::labs(
                    title=.("Model Calibration Curves (Observed vs Predicted)"),
                    x=.("Mean Predicted Probability"),
                    y=.("Observed Proportion of Events"),
                    color=.("Model")
                ) +
                ggplot2::theme_bw(base_size=12) +
                ggplot2::theme(
                    legend.position="bottom",
                    plot.title=ggplot2::element_text(face="bold", color="#1B365D", hjust=0.5),
                    panel.grid.minor=ggplot2::element_blank()
                )

            print(p)
            return(TRUE)
        }
    )
)
