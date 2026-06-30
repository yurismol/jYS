
# This file is a generated template, your changes will not be overwritten

mPCAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mPCAClass",
    inherit = mPCABase,
    private = list(
        .get_n_comp = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) < 2) return(1)
            return(min(length(vars), self$options$npc))
        },

        .init = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) == 0) return()
            
            loadingsTable <- self$results$loadingsTable
            cos2Table <- self$results$cos2Table
            
            for (i in 1:length(vars)) {
                loadingsTable$addColumn(name=paste0("pc", i), title=paste0(.("PC"), i), type="number")
                cos2Table$addColumn(name=paste0("pc", i), title=paste0(.("PC"), i), type="number")
            }
        },

        .run = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) < 2) {
                self$results$text$setContent(.("Please select at least 2 variables for PCA."))
                return()
            }
            
            dat <- self$data
            
            # 1. Restrict dataset by group as in mcor
            subs <- ""
            if (!is.null(self$options$group) && !is.null(self$options$selgroup) && self$options$selgroup != "") {
                subs <- paste(self$options$group, " == \"", self$options$selgroup, "\"", sep="")
                self$results$text$setContent(paste0("<h4>", jmvcore::.("Dataset restricted to group:"), " ", subs, "</h4>"))
                dat <- dat[dat[[self$options$group]] == self$options$selgroup, , drop=FALSE]
            } else {
                self$results$text$setContent(paste0("<h4>", jmvcore::.("Full dataset used in analysis"), "</h4>"))
            }
            
            # Extract variables and grouping class
            classVar <- self$options$classVar
            select_cols <- vars
            if (!is.null(classVar) && classVar != "") {
                select_cols <- c(select_cols, classVar)
            }
            
            # Omit rows with missing values
            dat_clean <- na.omit(dat[, select_cols, drop=FALSE])
            if (nrow(dat_clean) < 5) {
                self$results$text$setContent(.("Too few observations after removing missing values (minimum 5 required)."))
                return()
            }
            
            pca_data <- dat_clean[, vars, drop=FALSE]
            # Convert variables to numeric
            for (v in vars) {
                pca_data[[v]] <- jmvcore::toNumeric(pca_data[[v]])
            }
            
            # 2. Perform PCA
            pca_res <- try(prcomp(pca_data, scale. = TRUE, center = TRUE), silent = TRUE)
            if (inherits(pca_res, "try-error")) {
                self$results$text$setContent(.("Error occurred while calculating PCA. Please check your variables."))
                return()
            }
            
            eigenvalues <- pca_res$sdev^2
            total_variance <- sum(eigenvalues)
            explained_var <- (eigenvalues / total_variance) * 100
            cum_var <- cumsum(explained_var)
            
            # Parallel Analysis (run upfront to determine optimal_k)
            # Set local seed for parallel analysis simulation
            old_seed <- NULL
            if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
                old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
            }
            set.seed(self$options$seed)
            
            n <- nrow(pca_data)
            p <- ncol(pca_data)
            n_sim <- 100
            sim_eigenvalues <- matrix(0, nrow=n_sim, ncol=p)
            for (s in 1:n_sim) {
                sim_data <- matrix(rnorm(n * p), nrow=n, ncol=p)
                sim_pca <- prcomp(sim_data, scale. = TRUE, center = TRUE)
                sim_eigenvalues[s, ] <- sim_pca$sdev^2
            }
            mean_sim_eigen <- colMeans(sim_eigenvalues)
            
            # Restore seed
            if (!is.null(old_seed)) {
                assign(".Random.seed", old_seed, envir = .GlobalEnv)
            } else {
                rm(".Random.seed", envir = .GlobalEnv)
            }
            
            optimal_k <- sum(eigenvalues > mean_sim_eigen)
            if (optimal_k == 0) optimal_k <- 1
            
            n_comp <- min(length(vars), self$options$npc)
            
            # Show/hide columns in loadingsTable and cos2Table based on n_comp
            loadTable <- self$results$loadingsTable
            cosTable <- self$results$cos2Table
            for (i in 1:length(vars)) {
                col_name <- paste0("pc", i)
                visible <- (i <= n_comp)
                loadTable$getColumn(col_name)$setVisible(visible)
                cosTable$getColumn(col_name)$setVisible(visible)
            }
            
            # Populate Variance Table
            varTable <- self$results$varianceTable
            varTable$deleteRows()
            for (i in 1:length(eigenvalues)) {
                varTable$addRow(rowKey=i, values=list(
                    comp = paste0(.("PC"), i),
                    eigenvalue = eigenvalues[i],
                    variance = explained_var[i],
                    cumulative = cum_var[i]
                ))
            }
            
            # Loadings: rotation * sdev
            # Factor Loading = Correlation between variable and PC
            loadings_matrix <- pca_res$rotation %*% diag(pca_res$sdev)
            rownames(loadings_matrix) <- vars
            colnames(loadings_matrix) <- paste0(.("PC"), 1:length(eigenvalues))
            
            # Populate Loadings Table
            loadTable <- self$results$loadingsTable
            loadTable$deleteRows()
            existing_cols <- names(loadTable$columns)
            for (i in 1:length(vars)) {
                var_name <- vars[i]
                row_vals <- list(variable = var_name)
                for (j in 1:n_comp) {
                    col_name <- paste0("pc", j)
                    if (col_name %in% existing_cols) {
                        row_vals[[col_name]] <- loadings_matrix[i, j]
                    }
                }
                loadTable$addRow(rowKey=var_name, values=row_vals)
            }
            
            # 3. Quality of representation (Cos2) = loadings^2
            cos2_matrix <- loadings_matrix^2
            cosTable <- self$results$cos2Table
            cosTable$deleteRows()
            existing_cos_cols <- names(cosTable$columns)
            for (i in 1:length(vars)) {
                var_name <- vars[i]
                row_vals <- list(variable = var_name)
                for (j in 1:n_comp) {
                    col_name <- paste0("pc", j)
                    if (col_name %in% existing_cos_cols) {
                        row_vals[[col_name]] <- cos2_matrix[i, j]
                    }
                }
                cosTable$addRow(rowKey=var_name, values=row_vals)
            }
            
            # 4. Scree Plot / Parallel Analysis (already calculated upfront)
            
            scree_state <- list(
                eigenvalues = eigenvalues,
                sim_eigenvalues = mean_sim_eigen,
                optimal = optimal_k
            )
            self$results$screePlot$setState(scree_state)
            
            # Heatmaps states
            self$results$loadingsHeatmap$setState(list(loadings = loadings_matrix[, 1:n_comp, drop=FALSE]))
            self$results$cos2Heatmap$setState(list(cos2 = cos2_matrix[, 1:n_comp, drop=FALSE]))
            
            # Biplot state
            self$results$biplot$setState(list(
                pca = pca_res,
                pcX = self$options$pcX,
                pcY = self$options$pcY,
                groups = if (!is.null(classVar) && classVar != "") as.factor(dat_clean[[classVar]]) else NULL
            ))
            
            # Score plot state & Permutation Test
            pcX <- min(self$options$pcX, length(eigenvalues))
            pcY <- min(self$options$pcY, length(eigenvalues))
            scores_2d <- pca_res$x[, c(pcX, pcY), drop=FALSE]
            
            p_val <- NULL
            group_vec <- NULL
            if (!is.null(classVar) && classVar != "") {
                group_vec <- as.factor(dat_clean[[classVar]])
                # Permutation test of separation
                obs_ratio <- 0
                p_val <- 1.0
                if (length(levels(group_vec)) > 1 && nrow(scores_2d) > 5) {
                    overall_mean <- colMeans(scores_2d)
                    ss_total <- sum((scores_2d[,1] - overall_mean[1])^2 + (scores_2d[,2] - overall_mean[2])^2)
                    
                    calc_ratio <- function(g_vec) {
                        g_sums <- rowsum(scores_2d, g_vec)
                        n_g <- as.numeric(table(g_vec))
                        n_g_safe <- ifelse(n_g == 0, 1, n_g)
                        g_means <- g_sums / n_g_safe
                        ssb <- sum(n_g * ((g_means[,1] - overall_mean[1])^2 + (g_means[,2] - overall_mean[2])^2))
                        ssw <- ss_total - ssb
                        if (ssw <= 1e-10) return(0)
                        ssb / ssw
                    }
                    
                    obs_ratio <- tryCatch(calc_ratio(group_vec), error = function(e) 0)
                    if (obs_ratio > 0) {
                        perm_ratios <- replicate(999, {
                            calc_ratio(sample(group_vec))
                        })
                        p_val <- (sum(perm_ratios >= obs_ratio) + 1) / (length(perm_ratios) + 1)
                    }
                }
            }
            
            score_state <- list(
                pca = pca_res,
                pcX = pcX,
                pcY = pcY,
                groups = group_vec,
                groupName = if (is.null(group_vec)) "" else classVar,
                p_val = p_val
            )
            self$results$scorePlot$setState(score_state)
            
            # 5. K-Means clustering comparison
            try({
                if (self$options$kmeansTable && !is.null(classVar) && classVar != "") {
                    group_vec <- as.factor(dat_clean[[classVar]])
                    k_val <- self$options$kMeansK
                    km_res <- kmeans(pca_res$x[, 1:n_comp, drop=FALSE], centers=k_val, nstart=25)
                    
                    tbl <- table(Cluster = km_res$cluster, RealGroup = group_vec)
                    kmTable <- self$results$kmeansTable
                    kmTable$deleteRows()
                    
                    # Show contingency table in results
                    kmTable$addRow(rowKey="header", values=list(metric=.("Contingency Matrix (Clusters vs Groups):"), value=""))
                    for (cl in 1:k_val) {
                        matches <- paste(sapply(colnames(tbl), function(cn) paste0(cn, ": ", tbl[cl, cn])), collapse=" | ")
                        kmTable$addRow(rowKey=paste0("cl_", cl), values=list(
                            metric = paste0(.("K-Means Cluster"), " ", cl),
                            value = matches
                        ))
                    }
                    
                    # Adjusted Rand Index (ARI) calculation
                    calc_ari <- function(t) {
                        sum_n_ij_c2 <- sum(choose(t, 2))
                        sum_a_i_c2 <- sum(choose(rowSums(t), 2))
                        sum_b_j_c2 <- sum(choose(colSums(t), 2))
                        n_c2 <- choose(sum(t), 2)
                        if (n_c2 == 0) return(0)
                        expected <- (sum_a_i_c2 * sum_b_j_c2) / n_c2
                        if (( (sum_a_i_c2 + sum_b_j_c2)/2 - expected ) == 0) return(1)
                        (sum_n_ij_c2 - expected) / ( (sum_a_i_c2 + sum_b_j_c2)/2 - expected )
                    }
                    ari_val <- calc_ari(tbl)
                    kmTable$addRow(rowKey="blank", values=list(metric="", value=""))
                    kmTable$addRow(rowKey="ari", values=list(
                        metric = .("Adjusted Rand Index (ARI)"),
                        value = sprintf("%.4f", ari_val)
                    ))
                }
            }, silent = TRUE)
            
            # 6. Regression Model & ROC & K-fold Validation
            try({
                if (!is.null(classVar) && classVar != "") {
                    group_vec <- as.factor(dat_clean[[classVar]])
                    levels_present <- levels(group_vec)
                    
                    if (length(levels_present) >= 2) {
                        lvl1 <- levels_present[1]
                        lvl2 <- levels_present[2]
                        
                        # Filter dat_clean to only these two levels for binary classification
                        dat_reg <- dat_clean[dat_clean[[classVar]] %in% c(lvl1, lvl2), , drop=FALSE]
                        group_vec_reg <- as.factor(as.character(dat_reg[[classVar]]))
                        target_numeric <- as.numeric(group_vec_reg) - 1
                        
                        scores_df <- as.data.frame(pca_res$x[rownames(dat_reg), 1:n_comp, drop=FALSE])
                        scores_df$target <- target_numeric
                        
                        # K-fold Cross-Validation
                        k_folds <- self$options$regKFold
                        set.seed(self$options$seed)
                        folds <- sample(cut(seq(1, nrow(scores_df)), breaks=k_folds, labels=FALSE))
                        
                        cv_acc <- numeric(k_folds)
                        cv_sens <- numeric(k_folds)
                        cv_spec <- numeric(k_folds)
                        cv_auc <- numeric(k_folds)
                        
                        # Collect out-of-fold cross-validation predictions
                        cv_prob <- rep(NA, nrow(scores_df))
                        
                        regTable <- self$results$regTable
                        regTable$deleteRows()
                        
                        for (fold_idx in 1:k_folds) {
                            test_indices <- which(folds == fold_idx)
                            train_data <- scores_df[-test_indices, , drop=FALSE]
                            test_data <- scores_df[test_indices, , drop=FALSE]
                            
                            if (length(unique(train_data$target)) < 2 || length(unique(test_data$target)) < 2) {
                                cv_acc[fold_idx] <- NA
                                cv_sens[fold_idx] <- NA
                                cv_spec[fold_idx] <- NA
                                cv_auc[fold_idx] <- NA
                                next
                            }
                            
                            model <- glm(target ~ ., data=train_data, family=binomial)
                            preds <- predict(model, newdata=test_data, type="response")
                            
                            # Store predictions in cv_prob
                            cv_prob[test_indices] <- preds
                            
                            class_preds <- ifelse(preds >= 0.5, 1, 0)
                            actuals <- test_data$target
                            
                            cv_acc[fold_idx] <- sum(class_preds == actuals) / length(actuals)
                            
                            tp <- sum(class_preds == 1 & actuals == 1)
                            tn <- sum(class_preds == 0 & actuals == 0)
                            fp <- sum(class_preds == 1 & actuals == 0)
                            fn <- sum(class_preds == 0 & actuals == 1)
                            
                            cv_sens[fold_idx] <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                            cv_spec[fold_idx] <- if ((tn + fp) > 0) tn / (tn + fp) else NA
                            
                            roc_fold <- try(pROC::roc(actuals, preds, quiet=TRUE), silent=TRUE)
                            cv_auc[fold_idx] <- if (inherits(roc_fold, "try-error")) NA else as.numeric(roc_fold$auc)
                            
                            regTable$addRow(rowKey=fold_idx, values=list(
                                fold = paste0(.("Fold"), " ", fold_idx),
                                accuracy = cv_acc[fold_idx],
                                sensitivity = cv_sens[fold_idx],
                                specificity = cv_spec[fold_idx],
                                auc = cv_auc[fold_idx]
                            ))
                        }
                        
                        # Add Mean row
                        regTable$addRow(rowKey="mean", values=list(
                            fold = .("Average (Mean)"),
                            accuracy = mean(cv_acc, na.rm=TRUE),
                            sensitivity = mean(cv_sens, na.rm=TRUE),
                            specificity = mean(cv_spec, na.rm=TRUE),
                            auc = mean(cv_auc, na.rm=TRUE)
                        ))
                        
                        # Full model ROC curve state
                        full_model <- glm(target ~ ., data=scores_df, family=binomial)
                        full_preds <- predict(full_model, type="response")
                        
                        self$results$rocPlot$setState(list(
                            y_num = target_numeric,
                            train_prob = full_preds,
                            val_y = NULL,
                            val_prob = NULL,
                            cv_y = target_numeric,
                            cv_prob = cv_prob,
                            partition = "kfold",
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        ))
                        
                        # Populate ROC Table
                        if (self$options$show_roc_table && requireNamespace("pROC", quietly = TRUE)) {
                            rocTable <- self$results$rocTable
                            rocTable$deleteRows()
                            
                            is_pct <- self$options$roc_unit == "percent"
                            mult <- if (is_pct) 100 else 1
                            
                            add_roc_row <- function(row_name, y_true, y_prob) {
                                tryCatch({
                                    if (is.null(y_true) || is.null(y_prob)) return()
                                    
                                    # Aggressively coerce to plain numeric vector
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
                                    
                                    coords <- try(pROC::coords(r, x="best", best.method="youden", ret=c("threshold", "specificity", "sensitivity")), silent=TRUE)
                                    if (inherits(coords, "try-error")) return()
                                    
                                    # pROC::coords returns data.frame in newer versions, matrix in older
                                    if (is.data.frame(coords)) {
                                        cutoff <- as.numeric(coords[1, "threshold"])
                                        sp     <- as.numeric(coords[1, "specificity"]) * mult
                                        se     <- as.numeric(coords[1, "sensitivity"]) * mult
                                    } else {
                                        if (is.matrix(coords)) {
                                            coords <- coords[1, ]
                                        }
                                        cutoff <- as.numeric(coords["threshold"])
                                        sp     <- as.numeric(coords["specificity"]) * mult
                                        se     <- as.numeric(coords["sensitivity"]) * mult
                                    }
                                    
                                    direction <- as.character(r$direction)
                                    auc_val <- as.numeric(auc_val)
                                    
                                    rocTable$addRow(rowKey = row_name, values = list(
                                        part = as.character(row_name),
                                        auc = auc_val,
                                        cutoff = cutoff,
                                        se = se,
                                        sp = sp,
                                        direction = direction
                                    ))
                                }, error = function(e) {
                                    # Silently skip if ROC table row fails
                                })
                            }
                            
                            add_roc_row(.("Training"), target_numeric, full_preds)
                            add_roc_row(.("K-Fold CV"), target_numeric, cv_prob)
                        } else {
                            self$results$rocTable$deleteRows()
                        }
                    } else {
                        regTable <- self$results$regTable
                        regTable$deleteRows()
                        regTable$addRow(rowKey="warn", values=list(
                            fold = .("Warning: Grouping variable must have at least 2 levels."),
                            accuracy = NaN, sensitivity = NaN, specificity = NaN, auc = NaN
                        ))
                    }
                }
            }, silent = TRUE)
        },

        .screePlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            
            eigenvalues <- state$eigenvalues
            sim_eigenvalues <- state$sim_eigenvalues
            optimal <- state$optimal
            
            df <- data.frame(
                Component = 1:length(eigenvalues),
                Eigenvalue = eigenvalues,
                Noise = sim_eigenvalues
            )
            
            lbl_obs <- .("Observed Data")
            lbl_noise <- .("Noise (95th percentile)")
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Component)) +
                ggplot2::geom_col(ggplot2::aes(y = Eigenvalue), fill = "dodgerblue", alpha = 0.5) +
                ggplot2::geom_line(ggplot2::aes(y = Eigenvalue, color = lbl_obs), size = 1.2) +
                ggplot2::geom_point(ggplot2::aes(y = Eigenvalue, color = lbl_obs), size = 3) +
                ggplot2::geom_line(ggplot2::aes(y = Noise, color = lbl_noise), linetype = "dashed", size = 1) +
                ggplot2::geom_point(ggplot2::aes(y = Noise, color = lbl_noise), size = 2) +
                ggplot2::geom_vline(xintercept = optimal, color = "red", linetype = "dotted", size = 1.2) +
                ggplot2::annotate("text", x = optimal + 0.15, y = max(df$Eigenvalue) * 0.9, 
                         label = paste0(.("Threshold (K ="), " ", optimal, " ", .("comp.)")), color = "red", fontface = "bold", hjust=0) +
                ggplot2::scale_color_manual(values = stats::setNames(c("blue", "red"), c(lbl_obs, lbl_noise))) +
                ggplot2::labs(
                    title = .("Eigenvalues and Parallel Analysis"),
                    x = .("Component Number"),
                    y = .("Eigenvalue"),
                    color = .("Source")
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                    panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                    panel.grid.minor = ggplot2::element_blank(),
                    axis.title = ggplot2::element_text(size = 9),
                    axis.text = ggplot2::element_text(size = 8),
                    legend.position = "bottom"
                )
            
            p <- p + ggtheme
            p <- p + ggplot2::theme(legend.position = "bottom")
            print(p)
            TRUE
        },

        .loadingsHeatmap = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            
            loadings <- state$loadings
            df <- as.data.frame(loadings)
            df$Variable <- rownames(df)
            
            df_melt <- reshape2::melt(df, id.vars = "Variable")
            colnames(df_melt) <- c("Variable", "Component", "Loading")
            
            p <- ggplot2::ggplot(df_melt, ggplot2::aes(x = Component, y = Variable, fill = Loading)) +
                ggplot2::geom_tile(color = "white", size = 0.5) +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Loading)), color = "black", fontface = "bold", size = 5) +
                ggplot2::labs(
                    title = .("Factor Loadings Matrix"),
                    x = .("Principal Component"),
                    y = .("Variable"),
                    fill = .("Loading")
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    axis.title = ggplot2::element_text(size = 9),
                    axis.text = ggplot2::element_text(size = 8),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank()
                )
            
            p <- p + ggtheme
            p <- p + ggplot2::scale_fill_gradient2(low = "#4b7bec", mid = "white", high = "#eb3b5a", midpoint = 0, limit = c(-1, 1))
            print(p)
            TRUE
        },

        .cos2Heatmap = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            
            cos2 <- state$cos2
            df <- as.data.frame(cos2)
            df$Variable <- rownames(df)
            
            df_melt <- reshape2::melt(df, id.vars = "Variable")
            colnames(df_melt) <- c("Variable", "Component", "Cos2")
            
            p <- ggplot2::ggplot(df_melt, ggplot2::aes(x = Component, y = Variable, fill = Cos2)) +
                ggplot2::geom_tile(color = "white", size = 0.5) +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Cos2)), color = "black", fontface = "bold", size = 5) +
                ggplot2::labs(
                    title = .("Representation Quality (Cos\u00B2)"),
                    x = .("Principal Component"),
                    y = .("Variable"),
                    fill = .("Cos\u00B2")
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    axis.title = ggplot2::element_text(size = 9),
                    axis.text = ggplot2::element_text(size = 8),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank()
                )
            
            p <- p + ggtheme
            p <- p + ggplot2::scale_fill_gradient(low = "white", high = "#1f77b4", limit = c(0, 1))
            print(p)
            TRUE
        },        .scorePlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            
            pca <- state$pca
            pcX <- state$pcX
            pcY <- state$pcY
            groups <- state$groups
            groupName <- state$groupName
            p_val <- state$p_val
            
            df <- data.frame(
                X = pca$x[, pcX],
                Y = pca$x[, pcY]
            )
            
            if (!is.null(groups)) {
                df$Group <- groups
                p <- ggplot2::ggplot(df, ggplot2::aes(x = X, y = Y, color = Group)) +
                    ggplot2::geom_point(ggplot2::aes(fill = Group), size = 3, alpha = 0.8, color = "white", shape = 21, stroke = 0.5)
                
                # Dynamic selective drawing of ellipses
                groups_with_enough_points <- names(which(table(df$Group) >= 3))
                if (length(groups_with_enough_points) > 0) {
                    p <- p + ggplot2::stat_ellipse(data = droplevels(subset(df, Group %in% groups_with_enough_points)), 
                                                 ggplot2::aes(x = X, y = Y, color = Group), 
                                                 type = "t", level = 0.95, linetype = "dashed", size = 0.8, alpha = 0.8)
                }
            } else {
                p <- ggplot2::ggplot(df, ggplot2::aes(x = X, y = Y)) +
                    ggplot2::geom_point(fill = "#3498db", color = "white", size = 3, alpha = 0.8, shape = 21, stroke = 0.5)
            }
            
            p <- p +
                ggplot2::labs(
                    title = .("Group Projection in PCA Space"),
                    subtitle = if (!is.null(p_val)) {
                        p_text <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", sprintf("%.3f", p_val))
                        paste0(.("Group:"), " ", groupName, " (", .("Permutation"), " ", p_text, ")")
                    } else "",
                    x = paste0(.("Principal Component"), " ", pcX, " (PC", pcX, ") - ", sprintf("%.1f", (pca$sdev[pcX]^2/sum(pca$sdev^2))*100), .("% of variance")),
                    y = paste0(.("Principal Component"), " ", pcY, " (PC", pcY, ") - ", sprintf("%.1f", (pca$sdev[pcY]^2/sum(pca$sdev^2))*100), .("% of variance")),
                    color = NULL,
                    fill = NULL
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(face = "bold", size = 10, hjust = 0.5, color = "black"),
                    legend.position = if (!is.null(groups)) c(0.02, 0.98) else "none",
                    legend.justification = c(0, 1),
                    legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.8), color = "lightgrey", size = 0.3),
                    legend.title = ggplot2::element_blank(),
                    legend.text = ggplot2::element_text(size = 8),
                    panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                    panel.grid.minor = ggplot2::element_blank(),
                    axis.title = ggplot2::element_text(size = 9),
                    axis.text = ggplot2::element_text(size = 8)
                )
            
            p <- p + ggtheme
            
            # Re-apply manual scales after theme & ggtheme override to ensure style is enforced
            if (!is.null(groups)) {
                num_levels <- length(levels(df$Group))
                default_colors <- c("#2ecc71", "#3498db", "#e67e22", "#e74c3c", "#9b59b6", "#1abc9c", "#f1c40f", "#34495e", "#7f8c8d", "#d35400")
                if (num_levels > length(default_colors)) {
                    colors_to_use <- grDevices::rainbow(num_levels)
                } else {
                    colors_to_use <- default_colors[1:num_levels]
                }
                p <- p +
                    ggplot2::scale_color_manual(values = colors_to_use) +
                    ggplot2::scale_fill_manual(values = colors_to_use) +
                    ggplot2::theme(
                        legend.position = c(0.02, 0.98),
                        legend.justification = c(0, 1),
                        legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.8), color = "lightgrey", size = 0.3),
                        legend.title = ggplot2::element_blank()
                    )
            }
            
            print(p)
            TRUE
        },

        .biplot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            
            pca <- state$pca
            pcX <- state$pcX
            pcY <- state$pcY
            groups <- state$groups
            
            scores <- as.data.frame(pca$x[, c(pcX, pcY)])
            colnames(scores) <- c("X", "Y")
            if (!is.null(groups)) {
                scores$Group <- groups
            }
            
            rotation <- as.data.frame(pca$rotation[, c(pcX, pcY)])
            colnames(rotation) <- c("X", "Y")
            rotation$Variable <- rownames(rotation)
            
            # Auto-scaling vectors to scores
            mult <- min(
                (max(scores$X) - min(scores$X)) / (max(rotation$X) - min(rotation$X)),
                (max(scores$Y) - min(scores$Y)) / (max(rotation$Y) - min(rotation$Y))
            ) * 0.75
            
            rotation$X_scaled <- rotation$X * mult
            rotation$Y_scaled <- rotation$Y * mult
            
            p <- ggplot2::ggplot() +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5, alpha = 0.7) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5, alpha = 0.7)
            if (!is.null(groups)) {
                p <- p + ggplot2::geom_point(data = scores, ggplot2::aes(x = X, y = Y, fill = Group), color = "white", shape = 21, stroke = 0.5, alpha = 0.4, size = 2)
            } else {
                p <- p + ggplot2::geom_point(data = scores, ggplot2::aes(x = X, y = Y), color = "darkgrey", alpha = 0.4, size = 2)
            }
            
            p <- p + ggplot2::geom_segment(data = rotation, ggplot2::aes(x = 0, y = 0, xend = X_scaled, yend = Y_scaled), 
                             arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "darkgreen", size = 1.2, alpha = 0.9) +
                ggrepel::geom_text_repel(data = rotation, ggplot2::aes(x = X_scaled, y = Y_scaled, label = Variable), 
                                         color = "black", fontface = "bold", box.padding = 0.3,
                                         bg.color = "white", bg.r = 0.15) +
                ggplot2::labs(
                    title = .("Two-dimensional Variable Projection (Biplot)"),
                    x = paste0(.("Principal Component"), " ", pcX, " (PC", pcX, ") - ", sprintf("%.1f", (pca$sdev[pcX]^2/sum(pca$sdev^2))*100), .("% of variance")),
                    y = paste0(.("Principal Component"), " ", pcY, " (PC", pcY, ") - ", sprintf("%.1f", (pca$sdev[pcY]^2/sum(pca$sdev^2))*100), .("% of variance"))
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                    panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                    panel.grid.minor = ggplot2::element_blank(),
                    axis.title = ggplot2::element_text(size = 9),
                    axis.text = ggplot2::element_text(size = 8)
                )
            
            p <- p + ggtheme
            p <- p + ggplot2::theme(legend.position = "bottom")
            
            if (!is.null(groups)) {
                num_levels <- length(levels(scores$Group))
                default_colors <- c("#2ecc71", "#3498db", "#e67e22", "#e74c3c", "#9b59b6", "#1abc9c", "#f1c40f", "#34495e", "#7f8c8d", "#d35400")
                if (num_levels > length(default_colors)) {
                    colors_to_use <- grDevices::rainbow(num_levels)
                } else {
                    colors_to_use <- default_colors[1:num_levels]
                }
                p <- p + ggplot2::scale_fill_manual(values = colors_to_use)
            }
            
            print(p)
            TRUE
        },

        .rocPlot = function(image, ggtheme, theme, ...) {
            roc_data <- image$state
            if (is.null(roc_data)) return(FALSE)
            
            result <- tryCatch({
                roc_x <- roc_data$roc_x
                roc_unit <- roc_data$roc_unit
                partition <- roc_data$partition
                is_pct <- roc_unit == "percent"
                legacy_axes <- (roc_x == "1spec")
                show_roc_cut <- isTRUE(roc_data$show_roc_cut)
                
                # Fetch raw prediction data from state
                y_num <- roc_data$y_num
                train_prob <- roc_data$train_prob
                val_y <- roc_data$val_y
                val_prob <- roc_data$val_prob
                cv_y <- roc_data$cv_y
                cv_prob <- roc_data$cv_prob
                
                # We will plot sequentially:
                # 1. Training curve
                r_tr <- pROC::roc(y_num, train_prob, percent = is_pct, quiet = TRUE)
                
                # Determine colors and legend names
                cols <- c("#3366B2") # Training is always Blue
                if (self$options$palBrewer != "none") {
                    cols <- RColorBrewer::brewer.pal(n=3, name=self$options$palBrewer)
                }
                active_cols <- c(cols[1])
                ltys <- c(1)         # Training is solid
                auc_tr_val <- as.numeric(pROC::auc(r_tr))
                auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
                leg_labels <- c(paste0(.("Training (AUC ="), " ", auc_tr_str, ")"))
                
                # Format threshold pattern
                thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                
                # Plot Training Curve
                p <- pROC::plot.roc(r_tr, col=cols[1],
                    main=.("ROC curves"), cex.main=1.3,
                    percent=is_pct,
                    cex.lab=1.5, cex.axis=1.3, lwd=3, lty=1,
                    legacy.axes=legacy_axes,
                    xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                    ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                    print.thres=show_roc_cut,
                    print.thres.col=cols[1],
                    print.thres.pch=19, print.thres.cex=1.3,
                    print.thres.best.method="youden",
                    print.thres.pattern=thres_pattern,
                    grid=TRUE, add=FALSE
                )
                
                # Plot Validation/CV Curve if present
                if (partition == "holdout" && !is.null(val_prob) && !is.null(val_y)) {
                    r_va <- pROC::roc(val_y, val_prob, percent = is_pct, quiet = TRUE)
                    if (self$options$palBrewer == "none") {
                        cols <- c(cols, "#E54028") # Holdout is Red
                    }
                    active_cols <- c(active_cols, cols[2])
                    ltys <- c(ltys, 1)         # Holdout is solid
                    auc_va_val <- as.numeric(pROC::auc(r_va))
                    auc_va_str <- if (is_pct) paste0(round(auc_va_val, 1), "%") else round(auc_va_val, 3)
                    leg_labels <- c(leg_labels, paste0(.("Hold-out Validation (AUC ="), " ", auc_va_str, ")"))
                    
                    pROC::plot.roc(r_va, col=cols[2],
                        percent=is_pct,
                        lwd=3, lty=1,
                        legacy.axes=legacy_axes,
                        print.thres=show_roc_cut,
                        print.thres.col=cols[2],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        add=TRUE
                    )
                } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob) && !is.null(cv_y)) {
                    r_cv <- pROC::roc(cv_y, cv_prob, percent = is_pct, quiet = TRUE)
                    if (self$options$palBrewer == "none") {
                        cols <- c(cols, "#46B233") # CV is Green
                    }
                    active_cols <- c(active_cols, cols[2])
                    ltys <- c(ltys, 2)         # CV is dashed
                    auc_cv_val <- as.numeric(pROC::auc(r_cv))
                    auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                    lbl <- if (partition == "kfold") .("K-Fold CV (AUC =") else .("Repeated Stratified CV (AUC =")
                    leg_labels <- c(leg_labels, paste0(lbl, " ", auc_cv_str, ")"))
                    
                    pROC::plot.roc(r_cv, col=cols[2],
                        percent=is_pct,
                        lwd=3, lty=2,
                        legacy.axes=legacy_axes,
                        print.thres=show_roc_cut,
                        print.thres.col=cols[2],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        add=TRUE
                    )
                }
                
                # Draw legend
                legend("bottomright",
                    cex=1.1, lwd=3, col=active_cols,
                    lty=ltys,
                    bg="white", box.lwd=1,
                    legend=leg_labels
                )
                
                print(p)
                return(TRUE)
            }, error = function(e) {
                return(FALSE)
            })
            return(result)
        }
    )
)
