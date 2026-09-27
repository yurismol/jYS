# This file is a generated template, your changes will not be overwritten

mRFClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mRFClass",
    inherit = mRFBase,
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
            private$.initOutputs()
        },
        
        .initOutputs = function() {
            keys <- "predClass"
            titles <- .("Predicted Class")
            descriptions <- .("Random Forest Predicted Class")
            measureTypes <- "factor"
            self$results$predClass$set(keys, titles, descriptions, measureTypes)
            
            keys <- "predProb"
            titles <- .("Predicted Probability")
            descriptions <- .("Random Forest Predicted Probability")
            measureTypes <- "continuous"
            self$results$predProb$set(keys, titles, descriptions, measureTypes)
        },
        
        .run = function() {
            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors

            if (is.null(dep) || (length(covs) == 0 && length(factors) == 0)) {
                return()
            }

            if (!requireNamespace("ranger", quietly = TRUE)) {
                jmvcore::reject(.("The 'ranger' package is required. Please install it."))
            }

            # Prepare Data
            data_vars <- c(dep, covs, factors)
            clean_data <- na.omit(self$data[, data_vars, drop=FALSE])

            if (nrow(clean_data) < 10) {
                jmvcore::reject(.("The dataset has too few complete cases to train the model."))
            }

            y <- as.factor(clean_data[[dep]])
            y_levels <- levels(y)

            if (length(y_levels) < 2) {
                jmvcore::reject(.("The dependent variable must have at least 2 levels."))
            }

            C <- length(y_levels)

            # Build formula
            pred_vars <- c(covs, factors)
            formula_str <- jmvcore::composeFormula(dep, pred_vars)
            rf_formula <- as.formula(formula_str)

            # Model parameters
            ntree <- as.integer(self$options$ntree)
            nodesize <- as.integer(self$options$nodesize)
            maxdepth <- as.integer(self$options$maxdepth)
            sample_fraction <- as.numeric(self$options$sample_fraction)
            imp_type <- self$options$imp_type
            partition <- self$options$partition
            val_split <- as.numeric(self$options$val_split) / 100
            cv_folds <- as.integer(self$options$cv_folds)
            seed_val <- as.integer(self$options$seed)

            # Determine mtry
            mtry_val <- NULL
            if (self$options$mtry_method == "manual") {
                mtry_val <- as.integer(self$options$mtry_val)
            }

            # Importance type for ranger
            ranger_imp <- if (imp_type == "permutation") "permutation" else "impurity"

            # 1. Fit full model on all data
            set.seed(seed_val)
            fit_full <- try(ranger::ranger(
                formula = rf_formula,
                data = clean_data,
                num.trees = ntree,
                mtry = mtry_val,
                min.node.size = nodesize,
                max.depth = if (maxdepth == 0) NULL else maxdepth,
                sample.fraction = sample_fraction,
                importance = ranger_imp,
                probability = TRUE,
                seed = seed_val
            ), silent = TRUE)

            if (inherits(fit_full, "try-error")) {
                jmvcore::reject(jmvcore::format(.("Error during Random Forest fitting: {err}"), err = as.character(fit_full)))
            }

            # Full model training predictions
            train_prob_mat <- fit_full$predictions
            train_pred_idx <- apply(train_prob_mat, 1, which.max)
            train_pred_labels <- y_levels[train_pred_idx]
            train_acc <- mean(train_pred_labels == as.character(y)) * 100

            # OOB error
            oob_error <- fit_full$prediction.error * 100
            oob_acc <- 100 - oob_error

            # Effective mtry used
            mtry_used <- fit_full$mtry

            # 2. Validation
            val_acc <- NA
            val_prob <- NULL
            val_pred <- NULL
            test_idx <- NULL
            cv_acc <- NA
            cv_pred_probs <- NULL
            cv_preds <- NULL
            cv_errors <- 0

            set.seed(seed_val)

            if (partition == "holdout") {
                # Stratified hold-out split
                test_idx_list <- list()
                for (c_idx in seq_along(y_levels)) {
                    idx_c <- which(as.integer(y) == c_idx)
                    n_test_c <- max(1, round(length(idx_c) * val_split))
                    test_idx_list[[c_idx]] <- sample(idx_c, n_test_c)
                }
                test_idx <- unlist(test_idx_list)
                train_idx <- setdiff(1:nrow(clean_data), test_idx)

                fit_val <- try(ranger::ranger(
                    formula = rf_formula,
                    data = clean_data[train_idx, , drop = FALSE],
                    num.trees = ntree,
                    mtry = mtry_val,
                    min.node.size = nodesize,
                    max.depth = if (maxdepth == 0) NULL else maxdepth,
                    sample.fraction = sample_fraction,
                    probability = TRUE,
                    seed = seed_val
                ), silent = TRUE)

                if (!inherits(fit_val, "try-error")) {
                    val_pred_obj <- predict(fit_val, data = clean_data[test_idx, , drop = FALSE])
                    val_prob <- val_pred_obj$predictions
                    val_pred_idx <- apply(val_prob, 1, which.max)
                    val_pred <- y_levels[val_pred_idx]
                    val_acc <- mean(val_pred == as.character(y[test_idx])) * 100
                }
            } else if (partition == "kfold") {
                # Stratified K-Fold Cross-Validation
                create_stratified_folds <- function(y_vec, k) {
                    idx_by_class <- split(1:length(y_vec), y_vec)
                    folds <- vector("list", k)
                    for (class_idx in idx_by_class) {
                        shuffled <- sample(class_idx)
                        groups <- cut(seq_along(shuffled), breaks = k, labels = FALSE)
                        for (i in 1:k) {
                            folds[[i]] <- c(folds[[i]], shuffled[groups == i])
                        }
                    }
                    return(folds)
                }

                folds <- create_stratified_folds(y, cv_folds)
                cv_pred_probs <- matrix(0, nrow = nrow(clean_data), ncol = C)
                colnames(cv_pred_probs) <- y_levels
                cv_preds <- character(nrow(clean_data))
                cv_errors <- 0

                for (i in 1:cv_folds) {
                    private$.checkpoint()
                    t_idx <- folds[[i]]
                    tr_idx <- setdiff(1:nrow(clean_data), t_idx)

                    fit_cv <- try(ranger::ranger(
                        formula = rf_formula,
                        data = clean_data[tr_idx, , drop = FALSE],
                        num.trees = ntree,
                        mtry = mtry_val,
                        min.node.size = nodesize,
                        max.depth = if (maxdepth == 0) NULL else maxdepth,
                        sample.fraction = sample_fraction,
                        probability = TRUE,
                        seed = seed_val
                    ), silent = TRUE)

                    if (!inherits(fit_cv, "try-error")) {
                        p_obj <- predict(fit_cv, data = clean_data[t_idx, , drop = FALSE])
                        cv_pred_probs[t_idx, ] <- p_obj$predictions
                        pred_idx <- apply(p_obj$predictions, 1, which.max)
                        cv_preds[t_idx] <- y_levels[pred_idx]
                    } else {
                        cv_errors <- cv_errors + 1
                    }
                }

                if (cv_errors == 0) {
                    cv_acc <- mean(cv_preds == as.character(y)) * 100
                }
            }

            # Write Model Info Table
            info_table <- self$results$infoTable

            fmt_pct <- function(val) {
                if (is.null(val) || is.na(val)) return("-")
                return(sprintf("%.2f%%", val))
            }

            metrics <- c(
                .("Number of Trees"),
                .("Variables per Split (mtry)"),
                .("Minimum Node Size"),
                .("Training Accuracy (%)"),
                .("OOB Accuracy (%)"),
                .("Hold-out Accuracy (%)"),
                .("K-Fold CV Accuracy (%)")
            )

            values <- c(
                as.character(ntree),
                as.character(mtry_used),
                as.character(nodesize),
                fmt_pct(train_acc),
                fmt_pct(oob_acc),
                if (partition == "holdout") fmt_pct(val_acc) else "-",
                if (partition == "kfold") fmt_pct(cv_acc) else "-"
            )

            for (i in 1:7) {
                info_table$setRow(rowNo = i, values = list(
                    metric = metrics[i],
                    value = values[i]
                ))
            }

            info_table$setNote("n1", .("<b>Number of Trees</b>: total number of decision trees grown in the ensemble."))
            info_table$setNote("n2", .("<b>mtry</b>: number of candidate variables randomly sampled at each split."))
            info_table$setNote("n3", .("<b>OOB Accuracy</b>: out-of-bag accuracy estimated from bootstrap samples not used for training each tree."))
            if (partition == "holdout") {
                info_table$setNote("n4", .("<b>Hold-out Accuracy</b>: classification accuracy on the hold-out validation dataset."))
            } else {
                info_table$setNote("n4", NULL)
            }
            if (partition == "kfold") {
                info_table$setNote("n5", .("<b>K-Fold CV Accuracy</b>: average classification accuracy across cross-validation folds."))
            } else {
                info_table$setNote("n5", NULL)
            }

            # 3. Variable Importance
            if (self$options$show_imp) {
                imp_vals <- fit_full$variable.importance
                imp_df <- data.frame(
                    var = names(imp_vals),
                    imp = as.numeric(imp_vals),
                    stringsAsFactors = FALSE
                )
                
                # Convert importance values to percentage (%)
                if (imp_type == "permutation") {
                    # Permutation importance is error rate/Brier score difference (fraction).
                    # Multiply by 100 to get percentage points (%).
                    imp_df$imp <- imp_df$imp * 100
                } else {
                    # Impurity (Gini) importance is in arbitrary units.
                    # Normalize to sum to 100% (guard against sum == 0).
                    sum_imp <- sum(imp_df$imp)
                    if (sum_imp > 0) {
                        imp_df$imp <- (imp_df$imp / sum_imp) * 100
                    }
                }

                imp_df$rank <- rank(-imp_df$imp, ties.method = "first")
                imp_df <- imp_df[order(imp_df$rank), ]

                imp_table <- self$results$importanceTable
                
                # Dynamically set table column title to indicate percentage (%)
                if (imp_type == "permutation") {
                    imp_table$getColumn("imp")$setTitle(paste0(.("Permutation Importance"), " (%)"))
                } else {
                    imp_table$getColumn("imp")$setTitle(paste0(.("Impurity (Gini) Importance"), " (%)"))
                }

                for (row_idx in 1:nrow(imp_df)) {
                    imp_table$addRow(rowKey = row_idx, values = list(
                        var = imp_df$var[row_idx],
                        imp = imp_df$imp[row_idx],
                        rank = imp_df$rank[row_idx]
                    ))
                }

                imp_type_label <- if (imp_type == "permutation") .("Permutation Importance") else .("Impurity (Gini) Importance")
                imp_table$setNote("imp_note", paste0(.("Importance type:"), " ", imp_type_label))

                image_imp <- self$results$importancePlot
                image_imp$setState(list(imp_df = imp_df, imp_type = imp_type))
            }

            # 4. OOB Error Convergence Curve
            if (self$options$show_oob_curve) {
                # Build models with increasing number of trees
                tree_seq <- c()
                if (ntree >= 10) tree_seq <- c(tree_seq, seq(10, min(100, ntree), by = 10))
                if (ntree >= 100) tree_seq <- c(tree_seq, seq(100, min(500, ntree), by = 50))
                if (ntree >= 500) tree_seq <- c(tree_seq, seq(500, ntree, by = 100))
                tree_seq <- sort(unique(c(tree_seq, ntree)))
                tree_seq <- tree_seq[tree_seq >= 10 & tree_seq <= ntree]
                if (length(tree_seq) == 0) tree_seq <- c(ntree)
                if (length(tree_seq) > 30) {
                    tree_seq <- unique(c(round(seq(min(tree_seq), ntree, length.out = 30)), ntree))
                }

                oob_errors <- numeric(length(tree_seq))
                for (t_idx in seq_along(tree_seq)) {
                    fit_tmp <- try(ranger::ranger(
                        formula = rf_formula,
                        data = clean_data,
                        num.trees = tree_seq[t_idx],
                        mtry = mtry_val,
                        min.node.size = nodesize,
                        max.depth = if (maxdepth == 0) NULL else maxdepth,
                        sample.fraction = sample_fraction,
                        probability = TRUE,
                        seed = seed_val
                    ), silent = TRUE)
                    if (!inherits(fit_tmp, "try-error")) {
                        oob_errors[t_idx] <- fit_tmp$prediction.error * 100
                    } else {
                        oob_errors[t_idx] <- NA
                    }
                }

                curve_data <- data.frame(
                    Trees = tree_seq,
                    OOB_Error = oob_errors
                )
                image_curve <- self$results$oobCurvePlot
                image_curve$setState(curve_data)
            }

            # 5. Confusion Matrix & Performance Stats
            if (self$options$show_matrix) {
                can_run_matrix <- FALSE
                if (partition == "oob") {
                    actual_labels <- as.character(y)
                    pred_labels <- train_pred_labels
                    can_run_matrix <- TRUE
                } else if (partition == "kfold" && cv_errors == 0) {
                    actual_labels <- as.character(y)
                    pred_labels <- cv_preds
                    can_run_matrix <- TRUE
                } else if (partition == "holdout" && !is.null(val_pred)) {
                    actual_labels <- as.character(y[test_idx])
                    pred_labels <- val_pred
                    can_run_matrix <- TRUE
                }

                if (can_run_matrix) {
                    cm_table <- table(
                        Actual = factor(actual_labels, levels = y_levels),
                        Predicted = factor(pred_labels, levels = y_levels)
                    )

                    matrix_table <- self$results$matrixTable
                    row_idx <- 1
                    for (act_lev in y_levels) {
                        for (pred_lev in y_levels) {
                            cnt <- cm_table[act_lev, pred_lev]
                            matrix_table$addRow(rowKey = row_idx, values = list(
                                actual = act_lev,
                                predicted = pred_lev,
                                count = cnt
                            ))
                            row_idx <- row_idx + 1
                        }
                    }

                    # Performance statistics
                    calc_metrics_multiclass <- function(actual, predicted, class_levels) {
                        num_classes <- length(class_levels)
                        acc <- mean(actual == predicted)

                        se_list <- numeric(num_classes)
                        sp_list <- numeric(num_classes)
                        pr_list <- numeric(num_classes)
                        f1_list <- numeric(num_classes)
                        npv_list <- numeric(num_classes)

                        for (c in seq_along(class_levels)) {
                            lev <- class_levels[c]
                            tp <- sum(actual == lev & predicted == lev)
                            fp <- sum(actual != lev & predicted == lev)
                            fn <- sum(actual == lev & predicted != lev)
                            tn <- sum(actual != lev & predicted != lev)

                            se_list[c] <- if ((tp + fn) > 0) tp / (tp + fn) else 0
                            sp_list[c] <- if ((tn + fp) > 0) tn / (tn + fp) else 0
                            pr_list[c] <- if ((tp + fp) > 0) tp / (tp + fp) else 0
                            f1_list[c] <- if ((se_list[c] + pr_list[c]) > 0) 2 * (se_list[c] * pr_list[c]) / (se_list[c] + pr_list[c]) else 0
                            npv_list[c] <- if ((tn + fn) > 0) tn / (tn + fn) else 0
                        }

                        list(
                            acc = acc,
                            sen = mean(se_list),
                            spe = mean(sp_list),
                            prec = mean(pr_list),
                            f1 = mean(f1_list),
                            npv = mean(npv_list)
                        )
                    }

                    # Training metrics
                    tr_metrics <- calc_metrics_multiclass(as.character(y), train_pred_labels, y_levels)
                    # Validation metrics
                    val_metrics <- calc_metrics_multiclass(actual_labels, pred_labels, y_levels)

                    stats_table <- self$results$statsTable
                    if (partition == "kfold") {
                        stats_table$getColumn("cvVal")$setTitle(.("Cross-Validation"))
                    } else if (partition == "oob") {
                        stats_table$getColumn("cvVal")$setTitle(.("OOB"))
                    } else {
                        stats_table$getColumn("cvVal")$setTitle(.("Validation"))
                    }

                    is_pct <- self$options$roc_unit == "percent"
                    mult <- ifelse(is_pct, 100, 1)

                    metrics_names <- c(
                        .("Classification Accuracy"),
                        .("Sensitivity / Recall"),
                        .("Specificity"),
                        .("Precision"),
                        .("F1-Score"),
                        .("Negative Predictive Value (NPV)")
                    )
                    metrics_keys <- c("acc", "sen", "spe", "prec", "f1", "npv")

                    for (m_idx in seq_along(metrics_names)) {
                        k <- metrics_keys[m_idx]
                        stats_table$addRow(rowKey = m_idx, values = list(
                            metric = metrics_names[m_idx],
                            trainVal = tr_metrics[[k]] * mult,
                            cvVal = val_metrics[[k]] * mult
                        ))
                    }
                }
            }

            # 5.5 Misclassified Cases Table
            if (self$options$show_misclassified) {
                misc_table <- self$results$misclassifiedTable
                misc_table$deleteRows()
                
                orig_row_nums <- as.integer(rownames(clean_data))
                actual_chars <- as.character(y)
                
                misc_count <- 0
                max_misc_rows <- 100
                
                for (i in seq_along(actual_chars)) {
                    act_val <- actual_chars[i]
                    pred_val <- train_pred_labels[i]
                    
                    if (!is.na(act_val) && !is.na(pred_val) && act_val != pred_val) {
                        misc_count <- misc_count + 1
                        if (misc_count <= max_misc_rows) {
                            max_p <- train_prob_mat[i, train_pred_idx[i]]
                            
                            err_str <- if (C == 2) {
                                if (act_val == y_levels[1] && pred_val == y_levels[2]) {
                                    .("False Positive (FP)")
                                } else {
                                    .("False Negative (FN)")
                                }
                            } else {
                                .("Misclassified")
                            }
                            
                            misc_table$addRow(rowKey = i, values = list(
                                row_id = unname(as.integer(orig_row_nums[i]))[[1]],
                                actual = unname(as.character(act_val))[[1]],
                                predicted = unname(as.character(pred_val))[[1]],
                                prob = unname(as.numeric(max_p))[[1]],
                                error_type = unname(as.character(err_str))[[1]]
                            ))
                        }
                    }
                }
                
                if (misc_count > max_misc_rows) {
                    misc_table$setNote("n_misc", jmvcore::format(.("Showing first {max_rows} of {total} misclassified cases."), max_rows = max_misc_rows, total = misc_count))
                } else if (misc_count == 0) {
                    misc_table$setNote("n_misc", .("No misclassified cases found."))
                } else {
                    misc_table$setNote("n_misc", NULL)
                }
            }

            # 5b. Risk Stratification Table
            if (self$options$show_risk_strat) {
                risk_table <- self$results$riskTable
                risk_table$deleteRows()
                low_cut <- self$options$risk_cut_low
                high_cut <- self$options$risk_cut_high
                
                low <- max(0, min(100, low_cut)) / 100
                high <- max(0, min(100, high_cut)) / 100
                if (low > high) {
                    tmp <- low
                    low <- high
                    high <- tmp
                }
                
                if (length(y_levels) == 2) {
                    pos_level <- y_levels[2]
                    pos_probs <- train_prob_mat[, pos_level]
                    y_pos <- (y == pos_level)
                    N_clean <- length(y)
                    
                    idx_low <- pos_probs < low
                    idx_mod <- pos_probs >= low & pos_probs < high
                    idx_high <- pos_probs >= high
                    
                    cats <- list(
                        list(
                            name_key = .("Low Risk"),
                            range = paste0("< ", round(low * 100, 1), "%"),
                            idx = idx_low
                        ),
                        list(
                            name_key = .("Moderate Risk"),
                            range = paste0(round(low * 100, 1), "% – ", round(high * 100, 1), "%"),
                            idx = idx_mod
                        ),
                        list(
                            name_key = .("High Risk"),
                            range = paste0("≥ ", round(high * 100, 1), "%"),
                            idx = idx_high
                        )
                    )
                    
                    tot_events <- sum(y_pos, na.rm = TRUE)
                    
                    for (c_item in cats) {
                        cnt <- sum(c_item$idx, na.rm = TRUE)
                        ev <- sum(y_pos[c_item$idx], na.rm = TRUE)
                        pct <- if (N_clean > 0) (cnt / N_clean) * 100 else 0
                        rate <- if (cnt > 0) (ev / cnt) * 100 else 0
                        
                        risk_table$addRow(rowKey = c_item$name_key, values = list(
                            category = c_item$name_key,
                            range = c_item$range,
                            total = cnt,
                            total_pct = pct,
                            events = ev,
                            event_rate = rate
                        ))
                    }
                    
                    tot_pct <- if (N_clean > 0) 100 else 0
                    tot_rate <- if (N_clean > 0) (tot_events / N_clean) * 100 else 0
                    risk_table$addRow(rowKey = "Total", values = list(
                        category = .("Total"),
                        range = "0% – 100%",
                        total = N_clean,
                        total_pct = tot_pct,
                        events = tot_events,
                        event_rate = tot_rate
                    ))
                }
            }

            # 6. ROC Analysis
            if (self$options$show_roc) {
                can_run_roc <- FALSE
                if (partition == "oob") {
                    can_run_roc <- TRUE
                } else if (partition == "kfold" && cv_errors == 0) {
                    can_run_roc <- TRUE
                } else if (partition == "holdout" && !is.null(val_prob)) {
                    can_run_roc <- TRUE
                }

                image_roc <- self$results$rocPlot
                image_roc$clear()
                parent_state <- list()

                if (can_run_roc && requireNamespace("pROC", quietly = TRUE)) {
                    multiclass_roc_type <- self$options$multiclass_roc_type

                    if (C == 2) {
                        # Binary ROC
                        y_bin <- as.numeric(y == y_levels[2])
                        train_prob_bin <- train_prob_mat[, 2]

                        if (partition == "oob") {
                            roc_data <- list(
                                type = "binary",
                                C = C,
                                y_levels = y_levels,
                                y = y_bin,
                                train_prob = train_prob_bin,
                                val_prob = NULL,
                                val_y = NULL,
                                cv_prob = NULL,
                                partition = "oob",
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                        } else if (partition == "holdout" && !is.null(val_prob)) {
                            roc_data <- list(
                                type = "binary",
                                C = C,
                                y_levels = y_levels,
                                y = y_bin,
                                train_prob = train_prob_bin,
                                val_prob = val_prob[, 2],
                                val_y = as.numeric(y[test_idx] == y_levels[2]),
                                cv_prob = NULL,
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                        } else {
                            roc_data <- list(
                                type = "binary",
                                C = C,
                                y_levels = y_levels,
                                y = y_bin,
                                train_prob = train_prob_bin,
                                val_prob = NULL,
                                val_y = NULL,
                                cv_prob = cv_pred_probs[, 2],
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                        }
                        image_roc$addItem(key = "binary")
                        image_item <- image_roc$get(key = "binary")
                        image_item$setTitle(.("ROC Analysis Comparison"))
                        image_item$setState(roc_data)
                        parent_state[["binary"]] <- roc_data

                    } else if (multiclass_roc_type == "combined") {
                        # Multiclass Combined: 2 plots (Training + Validation)
                        roc_data_tr <- list(
                            type = "combined_training",
                            C = C,
                            y_levels = y_levels,
                            y = as.integer(y),
                            train_prob = train_prob_mat,
                            partition = partition,
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        )
                        image_roc$addItem(key = "combined_training")
                        image_item_tr <- image_roc$get(key = "combined_training")
                        image_item_tr$setTitle(.("Combined ROC - Training"))
                        image_item_tr$setState(roc_data_tr)
                        parent_state[["combined_training"]] <- roc_data_tr

                        if (partition == "holdout" && !is.null(val_prob)) {
                            roc_data_va <- list(
                                type = "combined_validation",
                                C = C,
                                y_levels = y_levels,
                                val_prob = val_prob,
                                val_y = as.integer(y)[test_idx],
                                cv_prob = NULL,
                                cv_y = NULL,
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                            image_roc$addItem(key = "combined_validation")
                            image_item_va <- image_roc$get(key = "combined_validation")
                            image_item_va$setTitle(.("Combined ROC - Validation"))
                            image_item_va$setState(roc_data_va)
                            parent_state[["combined_validation"]] <- roc_data_va
                        } else if (partition == "kfold" && !is.null(cv_pred_probs)) {
                            roc_data_va <- list(
                                type = "combined_validation",
                                C = C,
                                y_levels = y_levels,
                                val_prob = NULL,
                                val_y = NULL,
                                cv_prob = cv_pred_probs,
                                cv_y = as.integer(y),
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                            image_roc$addItem(key = "combined_validation")
                            image_item_va <- image_roc$get(key = "combined_validation")
                            image_item_va$setTitle(.("Combined ROC - Cross-Validation"))
                            image_item_va$setState(roc_data_va)
                            parent_state[["combined_validation"]] <- roc_data_va
                        }
                    } else {
                        # Multiclass Separate: one plot per class
                        for (c in 1:C) {
                            lev <- y_levels[c]
                            roc_data_c <- list(
                                type = "separate",
                                C = C,
                                class_name = lev,
                                train_y = as.numeric(as.integer(y) == c),
                                train_prob = train_prob_mat[, c],
                                val_y = if (!is.null(val_prob)) as.numeric(as.integer(y)[test_idx] == c) else NULL,
                                val_prob = if (!is.null(val_prob)) val_prob[, c] else NULL,
                                cv_y = if (!is.null(cv_pred_probs)) as.numeric(as.integer(y) == c) else NULL,
                                cv_prob = if (!is.null(cv_pred_probs)) cv_pred_probs[, c] else NULL,
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                            image_roc$addItem(key = lev)
                            image_item <- image_roc$get(key = lev)
                            image_item$setTitle(jmvcore::format(.("ROC Analysis for {class}"), class = lev))
                            image_item$setState(roc_data_c)
                            parent_state[[lev]] <- roc_data_c
                        }
                    }
                    image_roc$setState(parent_state)

                    # Populate ROC Table
                    if (self$options$show_roc_table) {
                        rocTable <- self$results$rocTable
                        rocTable$deleteRows()
                        private$.fillRocTable(rocTable, C, y, y_levels, train_prob_mat,
                                              val_prob, test_idx, cv_pred_probs,
                                              partition, cv_errors)
                    }
                } else {
                    self$results$rocTable$deleteRows()
                }
            } else {
                self$results$rocPlot$clear()
                self$results$rocTable$deleteRows()
            }

            # 7. Partial Dependence Plots
            if (self$options$show_pdp) {
                pdp_array <- self$results$pdpPlot
                pdp_array$clear()

                imp_vals <- fit_full$variable.importance
                if (!is.null(imp_vals) && length(imp_vals) > 0) {
                    imp_sorted <- sort(imp_vals, decreasing = TRUE)
                    n_pdp <- min(self$options$pdp_n_vars, length(imp_sorted))
                    top_vars <- names(imp_sorted)[1:n_pdp]

                    for (v_idx in seq_along(top_vars)) {
                        private$.checkpoint()
                        v_name <- top_vars[v_idx]
                        pdp_array$addItem(key = v_name)
                        pdp_item <- pdp_array$get(key = v_name)
                        pdp_item$setTitle(jmvcore::format(.("Partial Dependence: {var}"), var = v_name))

                        pdp_data <- private$.computePDP(fit_full, clean_data, v_name, y_levels)
                        pdp_item$setState(list(pdp_data = pdp_data, var_name = v_name, y_levels = y_levels, C = C))
                    }
                }
            }

            # 8. Save Predictions on Demand
            if ((self$options$predClass && self$results$predClass$isNotFilled()) ||
                (self$options$predProb && self$results$predProb$isNotFilled())) {

                pred_prob_col <- rep(NA, nrow(self$data))
                pred_class_col <- rep(NA, nrow(self$data))

                full_pred_idx <- apply(train_prob_mat, 1, which.max)
                full_classes <- y_levels[full_pred_idx]
                full_prob_val <- apply(train_prob_mat, 1, max)

                complete_case_indices <- as.integer(rownames(clean_data))
                pred_prob_col[complete_case_indices] <- full_prob_val
                pred_class_col[complete_case_indices] <- full_classes

                if (self$options$predClass && self$results$predClass$isNotFilled()) {
                    self$results$predClass$setRowNums(rownames(self$data))
                    self$results$predClass$setValues(index = 1, factor(pred_class_col, levels = y_levels))
                }

                if (self$options$predProb && self$results$predProb$isNotFilled()) {
                    self$results$predProb$setRowNums(rownames(self$data))
                    self$results$predProb$setValues(index = 1, pred_prob_col)
                }
            }
        },

        .importancePlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            imp_df <- state$imp_df
            imp_type <- state$imp_type

            y_label <- if (imp_type == "permutation") {
                paste0(.("Permutation Importance"), " (%)")
            } else {
                paste0(.("Impurity (Gini) Importance"), " (%)")
            }

            p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = reorder(var, imp), y = imp)) +
                ggplot2::geom_col(fill = "#2E8B57", width = 0.6) +
                ggplot2::geom_text(
                    ggplot2::aes(label = sprintf("%.2f%%", imp)),
                    hjust = -0.15,
                    size = 3.5,
                    fontface = "bold"
                ) +
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.20))) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = .("Variable Importance"),
                    x = .("Predictor"),
                    y = y_label
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .oobCurvePlot = function(image, ggtheme, theme, ...) {
            curve_data <- image$state
            if (is.null(curve_data)) return(FALSE)

            p <- ggplot2::ggplot(curve_data, ggplot2::aes(x = Trees, y = OOB_Error)) +
                ggplot2::geom_line(linewidth = 1.2, color = "#2E8B57") +
                ggplot2::geom_point(size = 2, color = "#2E8B57") +
                ggplot2::labs(
                    title = .("OOB Error Convergence"),
                    x = .("Number of Trees"),
                    y = .("OOB Error (%)")
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .rocPlot = function(image, ggtheme, theme, ...) {
            .renderClassifierRocPlot(
                image = image,
                ggtheme = ggtheme,
                theme = theme,
                options = self$options,
                defaultTrainColor = "#2E8B57",
                defaultCvColor = "#3366B2",
                mainTitle = .("ROC Analysis Comparison"),
                kfoldLabel = .("K-Fold Cross-Validation (AUC =")
            )
        },

        .pdpPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            pdp_data <- state$pdp_data
            var_name <- state$var_name
            y_levels <- state$y_levels
            C <- state$C

            if (is.null(pdp_data)) return(FALSE)

            if (is.numeric(pdp_data$x_val)) {
                if (C == 2) {
                    p <- ggplot2::ggplot(pdp_data, ggplot2::aes(x = x_val, y = prob)) +
                        ggplot2::geom_line(linewidth = 1.2, color = "#2E8B57") +
                        ggplot2::labs(
                            title = jmvcore::format(.("Partial Dependence: {var}"), var = var_name),
                            x = var_name,
                            y = paste0(.("Predicted Probability"), " (", y_levels[2], ")")
                        ) +
                        ggtheme
                } else {
                    p <- ggplot2::ggplot(pdp_data, ggplot2::aes(x = x_val, y = prob, color = class)) +
                        ggplot2::geom_line(linewidth = 1.2) +
                        ggplot2::labs(
                            title = jmvcore::format(.("Partial Dependence: {var}"), var = var_name),
                            x = var_name,
                            y = .("Predicted Probability")
                        ) +
                        ggtheme +
                        ggplot2::theme(legend.position = "bottom")
                }
            } else {
                if (C == 2) {
                    p <- ggplot2::ggplot(pdp_data, ggplot2::aes(x = x_val, y = prob)) +
                        ggplot2::geom_col(fill = "#2E8B57", width = 0.6) +
                        ggplot2::labs(
                            title = jmvcore::format(.("Partial Dependence: {var}"), var = var_name),
                            x = var_name,
                            y = paste0(.("Predicted Probability"), " (", y_levels[2], ")")
                        ) +
                        ggtheme
                } else {
                    p <- ggplot2::ggplot(pdp_data, ggplot2::aes(x = x_val, y = prob, fill = class)) +
                        ggplot2::geom_col(position = "dodge", width = 0.6) +
                        ggplot2::labs(
                            title = jmvcore::format(.("Partial Dependence: {var}"), var = var_name),
                            x = var_name,
                            y = .("Predicted Probability")
                        ) +
                        ggtheme +
                        ggplot2::theme(legend.position = "bottom")
                }
            }

            print(p)
            return(TRUE)
        },

        .computePDP = function(model, data, var_name, y_levels) {
            C <- length(y_levels)
            col_data <- data[[var_name]]

            if (is.numeric(col_data)) {
                grid_vals <- seq(min(col_data, na.rm=TRUE), max(col_data, na.rm=TRUE), length.out = 50)
            } else {
                grid_vals <- levels(as.factor(col_data))
            }

            results <- list()
            for (g_idx in seq_along(grid_vals)) {
                tmp_data <- data
                if (is.numeric(col_data)) {
                    tmp_data[[var_name]] <- grid_vals[g_idx]
                } else {
                    tmp_data[[var_name]] <- factor(grid_vals[g_idx], levels = levels(as.factor(col_data)))
                }
                pred_obj <- predict(model, data = tmp_data)
                mean_probs <- colMeans(pred_obj$predictions)

                if (C == 2) {
                    results[[g_idx]] <- data.frame(
                        x_val = grid_vals[g_idx],
                        prob = mean_probs[2],
                        stringsAsFactors = FALSE
                    )
                } else {
                    for (c in 1:C) {
                        results[[length(results) + 1]] <- data.frame(
                            x_val = grid_vals[g_idx],
                            prob = mean_probs[c],
                            class = y_levels[c],
                            stringsAsFactors = FALSE
                        )
                    }
                }
            }

            do.call(rbind, results)
        },

        .getColors = function(n) {
            base_colors <- c("#2E8B57", "#E54028", "#3366B2", "#8A2BE2", "#FF8C00", "#C71585", "#008080", "#8B4513")
            if (self$options$palBrewer != "none") {
                max_n <- switch(self$options$palBrewer,
                    "Accent" = 8, "Dark2" = 8, "Paired" = 12, "Pastel1" = 9,
                    "Set1" = 9, "Set2" = 8, "Set3" = 12, 8)
                req_n <- max(3, min(n, max_n))
                base_colors <- RColorBrewer::brewer.pal(n=req_n, name=self$options$palBrewer)
                if (n > max_n) {
                    base_colors <- grDevices::colorRampPalette(base_colors)(n)
                }
            }
            if (n > length(base_colors)) {
                return(grDevices::rainbow(n))
            }
            return(base_colors[1:n])
        },

        .fillRocTable = function(rocTable, C, y, y_levels, train_prob_mat,
                                  val_prob, test_idx, cv_pred_probs,
                                  partition, cv_errors) {
            is_pct <- self$options$roc_unit == "percent"
            mult <- if (is_pct) 100 else 1

            add_roc_row <- function(row_name, y_true, y_prob) {
                tryCatch({
                    if (is.null(y_true) || is.null(y_prob)) return()
                    y_true <- as.double(as.vector(drop(unlist(y_true))))
                    y_prob <- as.double(as.vector(drop(unlist(y_prob))))
                    if (length(y_true) != length(y_prob)) return()
                    complete_idx <- !is.na(y_true) & !is.na(y_prob)
                    if (sum(complete_idx) < 2) return()
                    y_t <- y_true[complete_idx]
                    y_p <- y_prob[complete_idx]
                    r <- try(pROC::roc(y_t, y_p, quiet = TRUE), silent = TRUE)
                    if (inherits(r, "try-error")) return()
                    auc_val <- as.numeric(pROC::auc(r)) * mult
                    coords <- try(pROC::coords(r, x="best", best.method="youden",
                        ret=c("threshold", "specificity", "sensitivity")), silent=TRUE)
                    if (inherits(coords, "try-error")) return()
                    if (is.data.frame(coords)) {
                        cutoff <- as.numeric(coords[1, "threshold"])
                        sp <- as.numeric(coords[1, "specificity"]) * mult
                        se <- as.numeric(coords[1, "sensitivity"]) * mult
                    } else {
                        if (is.matrix(coords)) coords <- coords[1, ]
                        cutoff <- as.numeric(coords["threshold"])
                        sp <- as.numeric(coords["specificity"]) * mult
                        se <- as.numeric(coords["sensitivity"]) * mult
                    }
                    direction <- as.character(r$direction)
                    rocTable$addRow(rowKey = row_name, values = list(
                        part = as.character(row_name),
                        auc = as.numeric(auc_val),
                        cutoff = cutoff,
                        se = se,
                        sp = sp,
                        direction = direction
                    ))
                }, error = function(e) {})
            }

            if (C == 2) {
                y_bin <- as.numeric(y == y_levels[2])
                add_roc_row(.("Training"), y_bin, train_prob_mat[, 2])
                if (partition == "holdout" && !is.null(val_prob)) {
                    add_roc_row(.("Hold-out Validation"), as.numeric(y[test_idx] == y_levels[2]), val_prob[, 2])
                } else if (partition == "kfold" && cv_errors == 0 && !is.null(cv_pred_probs)) {
                    add_roc_row(.("K-Fold Cross-Validation"), y_bin, cv_pred_probs[, 2])
                }
            } else {
                for (c in 1:C) {
                    lev <- y_levels[c]
                    add_roc_row(paste0(lev, " - ", .("Train")), as.numeric(as.integer(y) == c), train_prob_mat[, c])
                }
                if (partition == "holdout" && !is.null(val_prob)) {
                    for (c in 1:C) {
                        lev <- y_levels[c]
                        add_roc_row(paste0(lev, " - ", .("Val")), as.numeric(as.integer(y)[test_idx] == c), val_prob[, c])
                    }
                } else if (partition == "kfold" && cv_errors == 0 && !is.null(cv_pred_probs)) {
                    for (c in 1:C) {
                        lev <- y_levels[c]
                        add_roc_row(paste0(lev, " - ", .("CV")), as.numeric(as.integer(y) == c), cv_pred_probs[, c])
                    }
                }
            }
        }
    )
)
