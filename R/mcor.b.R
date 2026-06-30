# This file is a generated template, your changes will not be overwritten

mCORClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "mCORClass",
  inherit = mCORBase,
  private = list(

     .init=function() {
        #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
        #Sys.setlocale('LC_ALL', 'russian')
        matrix <- self$results$matrix
        vars   <- self$options$vars
        nVars  <- length(vars)
        ciw    <- self$options$ciWidth
        mtord <- FALSE

	#grp    <- self$options$group
	#if (!is.null(grp)) {
        #  grps  <- jmvcore::select(self$data, grp)
        #  groupLevels <- base::levels(grps[!is.na(grps[[grp]]), grp])
	#} else {
	#  groupLevels <- c("")
	#}

        #for (g in groupLevels) {
          for (i in seq_along(vars)) {
            var <- vars[[i]]
            matrix$addColumn(name=paste0(var, '[r]'), title=var,
                type='number', format='zto')
            matrix$addColumn(name=paste0(var, '[cil]'), title=var,
                type='number', format='zto', visible='(method=="pearson" && ci)')
            matrix$addColumn(name=paste0(var, '[ciu]'), title=var,
                type='number', format='zto', visible='(method=="pearson" && ci)')
            matrix$addColumn(name=paste0(var, '[p]'), title=var,
                type='number', format='zto,pvalue', visible='(pval)')
            matrix$addColumn(name=paste0(var, '[n]'), title=var,
                type='integer', visible='(n)')
          }
        #}

        for (i in seq_along(vars)) {
            var <- vars[[i]]
            values <- list()

            for (j in seq(i, nVars)) {
                v <- vars[[j]]
                values[[paste0(v, '[r]')]]   <- ''
                values[[paste0(v, '[cil]')]] <- ''
                values[[paste0(v, '[ciu]')]] <- ''
                values[[paste0(v, '[p]')]]   <- ''
                values[[paste0(v, '[n]')]]   <- ''
            }

            values[[paste0(var, '[r]')]]   <- '\u2013'
            values[[paste0(var, '[cil]')]] <- '\u2013'
            values[[paste0(var, '[ciu]')]] <- '\u2013'
            values[[paste0(var, '[p]')]]   <- '\u2013'
            values[[paste0(var, '[n]')]]   <- '\u2013'

            values[['.stat[ciu]']] <- jmvcore::format(.('{ciWidth}% CI Upper'), ciWidth=ciw)
            values[['.stat[cil]']] <- jmvcore::format(.('{ciWidth}% CI Lower'), ciWidth=ciw)
            rw <- matrix$setRow(rowKey=var, values)
  
        }
        hyp  <- self$options$get('hyp')
        flag <- self$options$get('flag')
        sign <- '* p < .05, ** p < .01, *** p < .001'
        if (self$options$decSymbol==",") sign <- '* p < ,05; ** p < ,01; *** p < ,001'
        if (hyp=='pos') {
            matrix$setNote('hyp', .('<b>H\u2090</b> is positive correlation'))
            hyp <- 'greater'
            if (flag)
                matrix$setNote('flag', paste0(sign, ', ', .('one-tailed')))
        } else if (hyp=='neg') {
            matrix$setNote('hyp', .('<b>H\u2090</b> is negative correlation'))
            hyp <- 'less'
            if (flag)
                matrix$setNote('flag', paste0(sign, ', ', .('one-tailed')))
        } else {
            matrix$setNote('hyp', NULL)
            hyp <- 'two.sided'
            if (flag)
                matrix$setNote('flag', sign)
        }
        if (self$options$adjust!='none') {
            matrix$setNote("adjust", jmvcore::format(.("Simultaneous multiple correlation comparisons using {n} method"), n=self$options$adjust))
        }

        method_title <- switch(self$options$method,
                               pearson = .("Pearson's r"),
                               spearman = .("Spearman's ρ"),
                               kendall = .("Kendall Tau"))
        matrix$setNote("method", jmvcore::format(.("<b>Correlation method</b>: {method}"), method=method_title))

         images <- self$results$get('rplots')
         nc <- 0
         if (self$options$hclust && nVars>2) {
           nc  <- self$options$numClust
         } else if (self$options$clustMan>"" && nVars>2) {
           nc  <- length(strsplit(self$options$clustMan,",")[[1]])+1
         }
         #self$results$pre$setContent(nc)
         if (nc>0) {
           cmb <- combn(1:nc, 2)
           for (i in 1:ncol(cmb)) {
             cm    <- cmb[,i]
             key   <- paste(cm[1], cm[2], sep="_")
             image <- images$addItem(key)
           }
         }

          # GLASSO table initialization
          glassoTable <- self$results$glassoGroup$glassoTable
          for (i in seq_along(vars)) {
              var <- vars[[i]]
              glassoTable$addColumn(name=paste0(var, '[r]'), title=var, type='number', format='zto')
              glassoTable$addColumn(name=paste0(var, '[p]'), title=var, type='number', format='zto,pvalue', visible='(pval)')
              glassoTable$addColumn(name=paste0(var, '[n]'), title=var, type='integer', visible='(n)')
          }
          for (i in seq_along(vars)) {
              var <- vars[[i]]
              values <- list()
              for (j in seq_along(vars)) {
                  values[[paste0(vars[[j]], '[r]')]] <- ''
                  values[[paste0(vars[[j]], '[p]')]] <- ''
                  values[[paste0(vars[[j]], '[n]')]] <- ''
              }
              values[[paste0(var, '[r]')]] <- '\u2013'
              values[[paste0(var, '[p]')]] <- '\u2013'
              values[[paste0(var, '[n]')]] <- '\u2013'
              glassoTable$setRow(rowKey=var, values)
          }
     },

     .run = function() {
        #options(OutDec=",")
        matrix <- self$results$matrix
        vars   <- self$options$vars
        nVars  <- length(vars)
        hyp    <- self$options$hyp

        corr   <- matrix(, nrow=nVars, ncol=nVars); corr[1, 1] <- 1
        colnames(corr) <- vars; rownames(corr) <- vars
        corp   <- matrix(, nrow=nVars, ncol=nVars); corp[1, 1] <- 0
        colnames(corp) <- vars; rownames(corp) <- vars
        if (hyp=='pos') hyp <- 'greater'
        else if (hyp=='neg') hyp <- 'less'
        else hyp <- 'two.sided'
        dat <- self$data
        self$results$text$setContent(" ")

	# data subset
	subs <- ""
	if (!is.null(self$options$group)) {
	   subs <- paste(self$options$group, " == \"", self$options$selgroup,"\"", sep="")
           self$results$text$setContent(paste("<h2>", subs, "</h2>", sep=""))
           dat <- dat[dat[[self$options$group]] == self$options$selgroup, , drop=FALSE]
        }

        if (self$options$adjust!='none' && nVars>1) {
            for (i in 1:nVars) {
              rowVarName <- vars[[i]]
              rowVar <- jmvcore::toNumeric(dat[[rowVarName]])
              for (j in 1:nVars) {
                colVarName <- vars[[j]]
                colVar <- jmvcore::toNumeric(dat[[colVarName]])
                if (is.factor(rowVar) || is.factor(colVar)) mtord <- TRUE
                else mtord <- FALSE
                result <- private$.test(rowVar, colVar, hyp, mtord)
                corr[i, j] <- corr[j, i] <- result$r
                corp[i, j] <- corp[j, i] <- result$p
              }
            }
            #if (self$options$hclust) {
            #  hc  <- hclust(dist(corr, method="euclidean"),
            #                method=self$options$clustMet)
            #  vars <- vars[hc$order]
            #}
            pp <- corp[lower.tri(corp, diag=FALSE)]
            pp <- p.adjust(pp, self$options$adjust)
            corp[lower.tri(corp, diag=FALSE)] <- pp

            pp <- corp[upper.tri(corp, diag=FALSE)]
            pp <- p.adjust(pp, self$options$adjust)
            corp[upper.tri(corp, diag=FALSE)] <- pp
            #self$results$pre$setContent(corp)
        }

        if (nVars>1) {
            for (i in 2:nVars) {
                rowVarName <- vars[[i]]
                rowVar <- jmvcore::toNumeric(dat[[rowVarName]])
                ciw <- self$options$ciWidth / 100
                for (j in seq_len(i-1)) {
                    values <- list()
                    colVarName <- vars[[j]]
                    colVar <- jmvcore::toNumeric(dat[[colVarName]])

                    if (is.factor(rowVar) || is.factor(colVar)) mtord <- TRUE
                    else mtord <- FALSE

                    result <- private$.test(rowVar, colVar, hyp, mtord)

                    corr[i, j] <- result$r; corr[j, i] <- result$r; corr[i, i] <- 1
                    if (self$options$adjust=='none') {
                      #self$results$pre$setContent(paste(result))
                      corp[i, j] <- result$p;
                      corp[j, i] <- result$p;
                      corp[i, i] <- 0
                    }

                    values[[paste0(colVarName, '[r]')]]   <- result$r
                    values[[paste0(colVarName, '[cil]')]] <- result$cil
                    values[[paste0(colVarName, '[ciu]')]] <- result$ciu
                    #values[[paste0(colVarName, '[p]')]]   <- result$p
                    values[[paste0(colVarName, '[p]')]]   <- corp[i, j]
                    n <- sum(!(is.na(colVar) | is.na(rowVar)))
                    values[[paste0(colVarName, '[n]')]]   <- n

                    matrix$setRow(rowNo=i, values)

                    if (mtord)
                        matrix$addFootnote(rowNo=i, paste0(colVarName, '[r]'), .('Pearson correlation cannot be calculated for non-numeric values'))
                    flag <- self$options$get('flag')
                    if (flag) {
                        p_val <- corp[i, j]
                        if (mtord)
                            {}  # do nothing
                        else if (!is.na(p_val)) {
                            if (p_val < .001)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '***')
                            else if (p_val < .01)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '**')
                            else if (p_val < .05)
                                matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '*')
                        }
                    }
               }
            }
            cor   <- NULL
            cor$r <- corr
            cor$p <- corp
            cor$s <- subs
            image <- self$results$treeplot
            if (self$options$hclust && nVars>2) {
              if (any(is.na(cor$r))) {
                  jmvcore::reject(.("Hierarchical clustering cannot be computed because the correlation matrix contains missing (NA/NaN) values."), code='')
              }
              hc <- hclust(dist(cor$r, method="euclidean"), method=self$options$clustMet)
              cor$hc <- hc
              cor$r <- cor$r[hc$order, hc$order]
              cor$p <- cor$p[hc$order, hc$order]
              image$setState(cor)
            } else {
              image$setState(NULL)
            }
            image <- self$results$plot
            image$setState(cor)

            if (self$options$glasso) {
                netMethod <- self$options$netMethod
                
                if (netMethod == "glasso") {
                    if ( ! (requireNamespace("glasso", quietly = TRUE) && requireNamespace("qgraph", quietly = TRUE))) {
                        jmvcore::reject(.("Packages 'glasso' and 'qgraph' are required for this analysis. Please install them."), code='')
                    }
                    
                    if (any(is.na(corr))) {
                        jmvcore::reject(.("Correlation matrix contains missing values. Cannot compute Graphical Lasso."), code='')
                    }
                    
                    n <- sum(complete.cases(dat[, vars, drop=FALSE]))
                    if (n < 4) {
                        jmvcore::reject(.("Sample size is too small to compute Graphical Lasso."), code='')
                    }
                    
                    # Check if corr is positive definite. If not, try to make it positive definite.
                    pd_adjusted <- FALSE
                    ev <- eigen(corr, symmetric = TRUE, only.values = TRUE)$values
                    if (any(ev <= 1e-8)) {
                        if (requireNamespace("Matrix", quietly = TRUE)) {
                            c_names <- colnames(corr)
                            r_names <- rownames(corr)
                            npd <- Matrix::nearPD(corr, corr = TRUE)
                            corr <- as.matrix(npd$mat)
                            colnames(corr) <- c_names
                            rownames(corr) <- r_names
                            pd_adjusted <- TRUE
                        }
                    }
                    
                    pcor <- tryCatch({
                        if (self$options$glassoType == "ebic") {
                            gamma <- self$options$glassoGamma
                            res <- qgraph::EBICglasso(corr, n = n, gamma = gamma, penalize.diagonal = FALSE)
                            res
                        } else {
                            rho <- self$options$glassoRho
                            gl <- glasso::glasso(corr, rho = rho, penalize.diagonal = FALSE)
                            wi <- gl$wi
                            d <- diag(wi)
                            if (any(d <= 0)) {
                                stop(.("Precision matrix diagonal elements are not all positive."))
                            }
                            pcor_mat <- -wi / (sqrt(d) %*% t(sqrt(d)))
                            diag(pcor_mat) <- 0
                            pcor_mat <- (pcor_mat + t(pcor_mat)) / 2
                            pcor_mat
                        }
                    }, error = function(e) {
                        jmvcore::reject(jmvcore::format(.("Error in Graphical Lasso calculation: {}"), e$message), code='')
                    })
                    
                    colnames(pcor) <- vars
                    rownames(pcor) <- vars
                    
                    # Calculate p-values for partial correlations
                    df_glasso <- n - length(vars)
                    corp_glasso <- matrix(NA, nrow=length(vars), ncol=length(vars))
                    if (df_glasso > 0) {
                        t_val <- ifelse(abs(pcor) < 1, pcor * sqrt(df_glasso / (1 - pcor^2)), Inf)
                        corp_glasso <- 2 * pt(abs(t_val), df = df_glasso, lower.tail = FALSE)
                        diag(corp_glasso) <- 0
                        
                        if (self$options$adjust != 'none') {
                            pp_glasso <- corp_glasso[lower.tri(corp_glasso, diag = FALSE)]
                            pp_glasso <- p.adjust(pp_glasso, self$options$adjust)
                            corp_glasso[lower.tri(corp_glasso, diag = FALSE)] <- pp_glasso
                            
                            pp_glasso <- corp_glasso[upper.tri(corp_glasso, diag = FALSE)]
                            pp_glasso <- p.adjust(pp_glasso, self$options$adjust)
                            corp_glasso[upper.tri(corp_glasso, diag = FALSE)] <- pp_glasso
                        }
                    }
                } else if (netMethod == "classic") {
                    # Classic partial correlation using ppcor
                    if ( ! (requireNamespace("ppcor", quietly = TRUE) && requireNamespace("qgraph", quietly = TRUE))) {
                        jmvcore::reject(.("Packages 'ppcor' and 'qgraph' are required for this analysis. Please install them."), code='')
                    }
                    
                    # Prepare data: convert variables to numeric and omit NA
                    c_dat <- dat[, vars, drop=FALSE]
                    for (v in vars) {
                        c_dat[[v]] <- jmvcore::toNumeric(c_dat[[v]])
                    }
                    c_dat <- c_dat[complete.cases(c_dat), , drop=FALSE]
                    n <- nrow(c_dat)
                    
                    if (n < 4) {
                        jmvcore::reject(.("Sample size is too small to compute Classical Partial Correlation."), code='')
                    }
                    
                    pd_adjusted <- FALSE
                    res_pcor <- tryCatch({
                        ppcor::pcor(c_dat, method = self$options$method)
                    }, error = function(e) {
                        jmvcore::reject(jmvcore::format(.("Error in Classical Partial Correlation calculation: {}"), e$message), code='')
                    })
                    
                    pcor <- res_pcor$estimate
                    corp_glasso <- res_pcor$p.value
                    df_glasso <- n - length(vars)
                    
                    # Apply multiple comparison adjustment to classic partial correlation p-values
                    if (self$options$adjust != 'none') {
                        pp_glasso <- corp_glasso[lower.tri(corp_glasso, diag = FALSE)]
                        pp_glasso <- p.adjust(pp_glasso, self$options$adjust)
                        corp_glasso[lower.tri(corp_glasso, diag = FALSE)] <- pp_glasso
                        
                        pp_glasso <- corp_glasso[upper.tri(corp_glasso, diag = FALSE)]
                        pp_glasso <- p.adjust(pp_glasso, self$options$adjust)
                        corp_glasso[upper.tri(corp_glasso, diag = FALSE)] <- pp_glasso
                    }
                } else {
                    # Classic semi-partial correlation using ppcor
                    if ( ! (requireNamespace("ppcor", quietly = TRUE) && requireNamespace("qgraph", quietly = TRUE))) {
                        jmvcore::reject(.("Packages 'ppcor' and 'qgraph' are required for this analysis. Please install them."), code='')
                    }
                    
                    # Prepare data: convert variables to numeric and omit NA
                    c_dat <- dat[, vars, drop=FALSE]
                    for (v in vars) {
                        c_dat[[v]] <- jmvcore::toNumeric(c_dat[[v]])
                    }
                    c_dat <- c_dat[complete.cases(c_dat), , drop=FALSE]
                    n <- nrow(c_dat)
                    
                    if (n < 4) {
                        jmvcore::reject(.("Sample size is too small to compute Classical Semi-Partial Correlation."), code='')
                    }
                    
                    pd_adjusted <- FALSE
                    res_spcor <- tryCatch({
                        ppcor::spcor(c_dat, method = self$options$method)
                    }, error = function(e) {
                        jmvcore::reject(jmvcore::format(.("Error in Classical Semi-Partial Correlation calculation: {}"), e$message), code='')
                    })
                    
                    pcor <- res_spcor$estimate
                    corp_glasso <- res_spcor$p.value
                    df_glasso <- n - length(vars)
                    
                    # Apply multiple comparison adjustment to classic semi-partial correlation p-values
                    if (self$options$adjust != 'none') {
                        diag_mask <- diag(nrow(corp_glasso)) == 1
                        p_vals <- corp_glasso[!diag_mask]
                        p_vals <- p.adjust(p_vals, self$options$adjust)
                        corp_glasso[!diag_mask] <- p_vals
                    }
                }
                
                # Fill table
                glassoTable <- self$results$glassoGroup$glassoTable
                for (i in seq_along(vars)) {
                    rowVarName <- vars[[i]]
                    values <- list()
                    for (j in seq_along(vars)) {
                        colVarName <- vars[[j]]
                        if (i == j) {
                             values[[paste0(colVarName, '[r]')]] <- '\u2013'
                             values[[paste0(colVarName, '[p]')]] <- '\u2013'
                             values[[paste0(colVarName, '[n]')]] <- '\u2013'
                        } else if (j > i && netMethod != "semiclassic") {
                            values[[paste0(colVarName, '[r]')]] <- ''
                            values[[paste0(colVarName, '[p]')]] <- ''
                            values[[paste0(colVarName, '[n]')]] <- ''
                        } else {
                            values[[paste0(colVarName, '[r]')]] <- pcor[i, j]
                            values[[paste0(colVarName, '[p]')]] <- corp_glasso[i, j]
                            values[[paste0(colVarName, '[n]')]] <- n
                        }
                    }
                    glassoTable$setRow(rowKey=rowVarName, values)
                    
                    if (self$options$flag && df_glasso > 0) {
                        for (j in seq_along(vars)) {
                            colVarName <- vars[[j]]
                            if (j != i && (j < i || netMethod == "semiclassic")) {
                                p_val <- corp_glasso[i, j]
                                if (!is.na(p_val)) {
                                    if (p_val < .001) {
                                        glassoTable$addSymbol(rowKey=rowVarName, paste0(colVarName, '[r]'), '***')
                                    } else if (p_val < .01) {
                                        glassoTable$addSymbol(rowKey=rowVarName, paste0(colVarName, '[r]'), '**')
                                    } else if (p_val < .05) {
                                        glassoTable$addSymbol(rowKey=rowVarName, paste0(colVarName, '[r]'), '*')
                                    }
                                }
                            }
                        }
                    }
                }
                
                 # Set notes for glassoTable
                 if (pd_adjusted) {
                     glassoTable$setNote('pd_warning', .("The correlation matrix was not positive definite and was adjusted to the nearest positive definite matrix to calculate Graphical Lasso."))
                 } else {
                     glassoTable$setNote('pd_warning', NULL)
                 }

                 if (self$options$flag) {
                     sign <- '* p < .05, ** p < .01, *** p < .001'
                     if (self$options$decSymbol == ",") sign <- '* p < ,05; ** p < ,01; *** p < ,001'
                     glassoTable$setNote('flag', sign)
                 } else {
                     glassoTable$setNote('flag', NULL)
                 }
                
                if (self$options$adjust != 'none') {
                    glassoTable$setNote('adjust', jmvcore::format(.("Simultaneous multiple correlation comparisons using {n} method"), n=self$options$adjust))
                } else {
                    glassoTable$setNote('adjust', NULL)
                }
                
                if (netMethod == "glasso") {
                    if (self$options$glassoType == "ebic") {
                        glassoTable$setNote('glasso_tuning', jmvcore::format(.("<b>Method</b>: Graphical Lasso (GLASSO) with Extended Bayesian Information Criterion (EBIC) tuning parameter selection (gamma = {gamma}). GLASSO uses L1 regularization to shrink weak partial correlations to exactly zero, producing a sparse network."), gamma=self$options$glassoGamma))
                    } else {
                        glassoTable$setNote('glasso_tuning', jmvcore::format(.("<b>Method</b>: Graphical Lasso (GLASSO) with a manual penalty parameter (rho = {rho}). GLASSO uses L1 regularization to shrink weak partial correlations to exactly zero, producing a sparse network."), rho=self$options$glassoRho))
                    }
                } else if (netMethod == "classic") {
                    glassoTable$setNote('glasso_tuning', .("<b>Method</b>: Classical Partial Correlation (no regularization/L1 penalty). Computes the linear association between each pair of variables while controlling for all other variables in the dataset, producing a fully connected network."))
                } else {
                    glassoTable$setNote('glasso_tuning', .("<b>Method</b>: Classical Semi-Partial Correlation (no regularization/L1 penalty). Computes the linear association between each pair of variables while controlling for all other variables in the dataset from only the second variable, yielding an asymmetric network."))
                }

                # Calculate Centrality
                node_cent <- NULL
                if (self$options$glassoHub || self$options$glassoPlotScale) {
                    cent <- qgraph::centrality_auto(pcor)
                    node_cent <- cent$node.centrality
                }

                # Fill Hub markers table
                if (self$options$glassoHub && !is.null(node_cent)) {
                    hubTable <- self$results$glassoGroup$glassoHubTable
                    hubTable$deleteRows()
                    hubTable$setNote('cent_intro', .("<b>Centrality metrics</b> (different measures of a node's importance in the network)."))
                    hubTable$setNote('cent_strength', .("<b>STRENGTH</b>: sum of absolute weights of connected edges; shows how strongly a node is directly connected to its neighbors."))
                    hubTable$setNote('cent_closeness', .("<b>CLOSENESS</b>: inverse of the sum of shortest path lengths to all other nodes; shows how central a node is in terms of distance."))
                    hubTable$setNote('cent_betweenness', .("<b>BETWEENNESS</b>: number of shortest paths between all pairs of nodes that pass through this node; shows its importance as a \"bridge\" or transit point."))
                    hubTable$setNote('cent_influence', .("<b>EXPECTED INFLUENCE</b>: sum of signed weights of connected edges; shows the node's net impact on the network, considering positive and negative relations."))
                    
                    hub_data <- list()
                    for (i in seq_along(vars)) {
                        varName <- vars[[i]]
                        
                        # Robustly extract metrics based on available columns in node_cent
                        strength <- 0
                        if ("Strength" %in% colnames(node_cent)) {
                            strength <- node_cent[i, "Strength"]
                        } else if ("strength" %in% colnames(node_cent)) {
                            strength <- node_cent[i, "strength"]
                        } else if ("Degree" %in% colnames(node_cent)) {
                            strength <- node_cent[i, "Degree"]
                        } else if ("degree" %in% colnames(node_cent)) {
                            strength <- node_cent[i, "degree"]
                        }
                        
                        closeness <- 0
                        if ("Closeness" %in% colnames(node_cent)) {
                            closeness <- node_cent[i, "Closeness"]
                        } else if ("closeness" %in% colnames(node_cent)) {
                            closeness <- node_cent[i, "closeness"]
                        }
                        
                        betweenness <- 0
                        if ("Betweenness" %in% colnames(node_cent)) {
                            betweenness <- node_cent[i, "Betweenness"]
                        } else if ("betweenness" %in% colnames(node_cent)) {
                            betweenness <- node_cent[i, "betweenness"]
                        }
                        
                        influence <- 0
                        if ("ExpectedInfluence" %in% colnames(node_cent)) {
                            influence <- node_cent[i, "ExpectedInfluence"]
                        } else if ("expectedinfluence" %in% colnames(node_cent)) {
                            influence <- node_cent[i, "expectedinfluence"]
                        } else {
                            influence <- sum(pcor[varName, ])
                        }
                        
                        hub_data[[i]] <- list(
                            var = varName,
                            strength = strength,
                            closeness = closeness,
                            betweenness = betweenness,
                            influence = influence
                        )
                    }
                    
                    # Sort the collected rows by strength in descending order
                    strengths <- sapply(hub_data, function(x) x$strength)
                    sorted_indices <- order(strengths, decreasing = TRUE)
                    sorted_hub_data <- hub_data[sorted_indices]
                    
                    for (row in sorted_hub_data) {
                        hubTable$addRow(rowKey = row$var, values = list(
                            var = row$var,
                            strength = row$strength,
                            closeness = row$closeness,
                            betweenness = row$betweenness,
                            influence = row$influence
                        ))
                    }
                }
                
                # Determine clusters if any
                clusters <- NULL
                if (self$options$hclust && nVars > 2) {
                    nClust <- ifelse(self$options$numClust > nVars, nVars, self$options$numClust)
                    hc <- hclust(dist(corr, method="euclidean"), method=self$options$clustMet)
                    ct <- cutree(hc, k=nClust)
                    clusters <- split(names(ct), ct)
                } else if (self$options$clustMan > "" && nVars > 2) {
                    vec <- as.numeric(strsplit(self$options$clustMan, ",")[[1]])
                    posClust <- c(1, vec, nVars)
                    splAt  <- function(x, pos) unname(split(x, findInterval(seq_along(x), pos)))
                    clust_nodes <- splAt(vars, posClust)
                    clusters <- clust_nodes
                }

                if (!is.null(clusters)) {
                    if (is.null(names(clusters))) {
                        names(clusters) <- seq_along(clusters)
                    }
                    names(clusters) <- sapply(names(clusters), function(name) jmvcore::format(.("Cluster {i}"), i=name))
                }
                
                # Set plot state
                gplot <- self$results$glassoGroup$glassoPlot
                gplot$setState(list(pcor = pcor, vars = vars, subs = subs, clusters = clusters, centrality = node_cent))
            }
         }

       },

      .test=function(var1, var2, hyp, mtord) {
        #options(OutDec=",")
        results <- list()
        suppressWarnings({
            if (mtord) {
                results$r   <- NaN
                results$p   <- NaN
                results$ciu <- NaN
                results$cil <- NaN
            } else {
                ciw <- self$options$ciWidth / 100
                res <- try(cor.test(as.numeric(var1), as.numeric(var2),
                           alternative=hyp, method=self$options$method,
                           conf.level=ciw, na.action=na.omit))
                if (!base::inherits(res, 'try-error')) {
                    results$r   <- res$estimate
                    results$p   <- res$p.value
                    results$cil <- res$conf.int[1]
                    results$ciu <- res$conf.int[2]
                }
                else {
                    results$r   <- NaN
                    results$p   <- NaN
                    results$cil <- NaN
                    results$ciu <- NaN
                }
            }

        }) # suppressWarnings

        return(results)
      },

      .plot=function(image, ggtheme, theme, ...) {
	  vars  <- self$options$vars
          nVars <- length(self$options$vars)
          key   <- image$key
          if (is.null(image$state) || nVars<2) return(FALSE)
          c <- image$state

          if (!self$options$hclust && self$options$plotOrder!="original") {
            order <- corrplot::corrMatOrder(c$r, order=self$options$plotOrder)
            c$r   <- c$r[order, order]
            c$p   <- c$p[order, order]
          }

	  vars  <- colnames(c$r)

	  col <- c("#053061", "#2869AC", "#5098C3",
		"#83C0DE", "#B8DDF0", "#FAC5A8", "#F4A07B",
		"#D65C48", "#B21E30", "#67001F")
	  cols <- hcl.colors(10, palette="Blue-Red 3")
	  if (self$options$signif=="all") {
		insig <- "pch"
		sigl  <- 1.2
	  } else {
		insig <- "blank"
		sigl  <- as.double(self$options$signif)
	  }
	  plotMetU <- self$options$plotMetU
	  plotMetL <- self$options$plotMetL
	  pm <- plotMetU
	  if (pm=="empty") pm = "circle"
	  sl <- sigl
	  ULe <- plotMetU==plotMetL && plotMetU=="empty"
	  if (plotMetU!=plotMetL || ULe) sl=as.double(0)

	  ncex <- 1.8-0.055*nVars
          p <- corrplot::corrplot(c$r, p.mat=c$p,
		tl.cex=1, tl.col="black", diag=FALSE,
		mar=c(0, 0, 1, 0), main=c$s,
		col=col, number.cex=ncex,
		cl.pos=self$options$clPos, cl.cex=1.1,
		sig.level=sl,
		insig=ifelse(plotMetU!=plotMetL || ULe, "blank", insig), pch="",
		method=pm,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="full")

	  if ((plotMetU!=plotMetL && plotMetU!="empty") && !ULe) {
            corrplot::corrplot(c$r, p.mat=c$p, add=TRUE, cl.pos="n", diag=FALSE,
		col=col, number.cex=ncex, tl.pos="n",
		sig.level=sigl, insig=insig, pch="",
		method=plotMetU,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="upper")
          }

	  if ((plotMetU!=plotMetL && plotMetL!="empty") && !ULe) {
            corrplot::corrplot(c$r, p.mat=c$p, add=TRUE, cl.pos="n", diag=FALSE,
		col=col, number.cex=ncex, tl.pos="n",
		sig.level=sigl, insig=insig, pch="",
		method=plotMetL,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="lower")
          }

          par(mar=c(0,0,1,0))
	  lines(c(nVars+0.5, 0.5), rev(c(nVars+0.5, 0.5)), lwd=3, col="lightgrey")
	  posClust <- list()
          if (self$options$hclust && nVars>2) {
            hc <- c$hc
            ct <- cutree(hc, k=ifelse(self$options$numClust>nVars,nVars,self$options$numClust))
            ct <- ct[hc$order]
            ct <- c(ct[!duplicated(ct)], ct[length(ct)])
            posClust <- pmatch(names(ct), vars, nomatch=nVars)
            corrplot::corrRect(p, name=names(ct), lwd=2, add=TRUE)
          } else if (self$options$clustMan>"" && nVars>2) {
            vec <- as.numeric(strsplit(self$options$clustMan, ",")[[1]])
            posClust <- c(1, vec, nVars)
            corrplot::corrRect(p, posClust, lwd=2, add=TRUE)
	  }

          # clusters numbers
          if (length(posClust)>0) {
            coor <- vector()
            io   <- 0
            nCl  <- length(posClust)
            posClust[nCl] <- posClust[nCl]+1
            for (i in posClust[-1]) {
              cw   <- i-io
              coor <- append(coor, cw/2+io)
              io   <- i-1
            }
            Num<- diag(1:(nCl-1))
            dn <- lower.tri(Num, diag=FALSE)
            r  <- (nCl):(nCl^2)
            r  <- r[1:(length(r)/2)]
            Num[dn] <- r; Num <- t(Num); Num[dn] <- r; Num <- t(Num)
            k <- 1
            for (i in coor) {
              for (j in coor) {
                text(j, nVars-i+1, Num[k], cex=1.4, font=4, col="black")
                k <- k + 1
                #lines(c(nVars+0.5, 0.5), rev(c(nVars+0.5, 0.5)), lwd=2, col="lightgreen")
              }
            }

            splAt  <- function(x, pos) unname(split(x, findInterval(x, pos)))
            vec    <- splAt(1:nVars, posClust)
            cmb    <- combn(1:(length(vec)), 2)
            #self$results$pre$setContent(cmb)

            images <- self$results$get('rplots')
            #images <- groups$get(key=i)$assump$resPlots
            #images <- self$results$rplots

            for (i in 1:ncol(cmb)) {
              cm  <- cmb[,i]
              crr <- list()
	      if (length(vec[[cm[1]]])>length(vec[[cm[2]]])) {
                m1 <- vec[[cm[2]]]; m2 <- vec[[cm[1]]]
              } else {
                m1 <- vec[[cm[1]]]; m2 <- vec[[cm[2]]]
              }
              
              crr$r <- c$r[m1, m2, drop=FALSE]
              crr$p <- c$p[m1, m2, drop=FALSE]
	      crr$m <- c$s

	      key <- paste(cm[1], cm[2], sep="_")
              #image <- images$addItem(key)

              image <- images$get(key=key)  # or whatever the $get() thing is
              nm <- jmvcore::format(.("Matrix #{i}"), i=i+nCl-1)
              image$setTitle(nm)
              image$setStatus('complete')
              image$setVisible(visible=TRUE)
              image$setState(crr)
            }
          }

	  print(p)
	  return(TRUE)
      },

      .rplot=function(image, ggtheme, theme, ...) {
          if (is.null(image$state)) return(FALSE)
          cor   <- image$state
	  col <- c("#053061", "#2869AC", "#5098C3",
		"#83C0DE", "#B8DDF0", "#FAC5A8", "#F4A07B",
		"#D65C48", "#B21E30", "#67001F")
	  plotMetU <- self$options$plotMetU
	  ULe  <- plotMetU=="empty"
	  if (ULe) pm = "circle"
	  else pm <- plotMetU
	  if (self$options$signif=="all") {
		insig <- "pch"
		sigl  <- 1.2
	  } else {
		insig <- "blank"
		sigl  <- as.double(self$options$signif)
	  }
          nrow <- nrow(cor$r)
          ncol <- ncol(cor$r)
	  ncex <- 1.8-0.055*max(ncol, nrow)
	  p <- corrplot::corrplot(cor$r, p.mat=cor$p,
		tl.cex=1.5, tl.col="black",
		mar=c(0, 0, ifelse(cor$m>"",1,0), 0), main=cor$m,
		col=col, number.cex=ncex,
		cl.pos="n",	#ifelse(nrow<3,"n",self$options$clPos),
		cl.cex=1.1, cl.ratio=0,
		sig.level=sigl,
		insig=ifelse(ULe, "blank", insig), pch="",
		method=pm,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="full")

	  if (self$options$clPos=="r") {
            vert = TRUE
            xlim = c(ncol+0.5, ncol+1.1)
            ylim = c(0.5, nrow+0.5)
	  } else {
            vert = FALSE
            xlim = c(0.5, ncol+0.5)
            ylim = c(0, 0.6)
          }
#	  if (self$options$clPos!="n" && nrow<3)
#	    corrplot::colorlegend(col, labels=c(seq(-1,1,.2)), align="c",
#		cex=1.1, vertical=vert, addlabels=TRUE,
#		xlim=xlim, ylim=ylim, ratio.colbar=0.15)
	  print(p)
	  return(TRUE)
      },

      .treeplot=function(image, ggtheme, theme, ...) {
          nVars  <- length(self$options$vars)
          if (is.null(image$state) || nVars<3) return(FALSE)
          nClust <- ifelse(self$options$numClust>nVars,nVars,self$options$numClust)
          hc <- image$state$hc
          main <- image$state$s
          if (self$options$clustCol) {
            hd <- as.dendrogram(hc)
            cols <- jmvcore::colorPalette(n=nClust, theme$palette, type="fill")
            hd <- dendextend::color_branches(hd, k=nClust, col=cols)
            hd <- dendextend::color_labels(hd, k=nClust, col=cols)
            hd <- dendextend::set(hd, "branches_lwd", 3)
            hd <- dendextend::set(hd, "labels_cex", 1.5)
            if (main>"") opar <- par(mar=c(10,0,1,0))
            else opar <- par(mar=c(10,0,0,0))
            p <- plot(hd, axes=FALSE, main=main)
          } else {
            if (main>"") opar <- par(mar=c(0,0,1,0))
            else opar <- par(mar=c(0,0,0,0))
            p <- plot(hc, col=1:10, axes=FALSE, ann=TRUE, cex=1.5,
                 main=main, lwd=2, hang=-1)
          }

          cutree_k_to_h <- function(tree, k) {
            n1 <- nrow(tree$merge)
            n  <- n1 + 1
            mean(tree$height[c(n-k, n-k+1L)])
          }
          abline(h=cutree_k_to_h(hc, nClust), col="red", lwd=2, lty=2)

	  print(p)
	  return(TRUE)
      },

      .glassoPlot=function(image, ggtheme, theme, ...) {
          if (is.null(image$state)) return(FALSE)
          
          pcor <- image$state$pcor
          vars <- as.character(image$state$vars)
          subs <- image$state$subs
          
          if (!requireNamespace("qgraph", quietly = TRUE)) {
              return(FALSE)
          }
          
          posCol <- "#B2182B"
          negCol <- "#2166AC"
          
          groups <- NULL
          nodeCols <- NULL
          
          if (self$options$clustCol && !is.null(image$state$clusters)) {
              groups <- lapply(image$state$clusters, function(cluster_vars) {
                  match(as.character(cluster_vars), vars)
              })
              nClust <- length(groups)
              nodeCols <- jmvcore::colorPalette(n = nClust, theme$palette, type = "fill")
          }
          
          layout <- self$options$glassoLayout
          if (layout == "groups" && is.null(groups)) {
              layout <- "spring"
          }
          
          vsize <- 12
          if (self$options$glassoPlotScale) {
              vsize <- c(8, 18)
          }

          hasGroups <- !is.null(groups) && length(groups) > 0
          
          if (hasGroups) {
              par(mar = c(6, 2, 3, 2))
              qmar <- c(7, 3, 3, 3)
          } else {
              par(mar = c(2, 2, 3, 2))
              qmar <- c(3, 3, 3, 3)
          }
          
          edge.labels <- FALSE

          # Determine title based on method
          plot_title <- if (subs > "") {
              paste0(.("Network ("), subs, ")")
          } else if (self$options$netMethod == "glasso") {
              .("Partial Correlation Network (GLASSO)")
          } else if (self$options$netMethod == "classic") {
              .("Classical Partial Correlation Network")
          } else {
              .("Classical Semi-Partial Correlation Network")
          }

          q_obj <- qgraph::qgraph(
              pcor,
              layout = layout,
              labels = vars,
              theme = "classic",
              posCol = posCol,
              negCol = negCol,
              groups = groups,
              color = nodeCols,
              legend = FALSE,
              title = plot_title,
              title.cex = 1.2,
              vsize = vsize,
              label.cex = 1.0,
              label.color = "black",
              legend.cex = 1.0,
              edge.width = 2,
              edge.labels = FALSE,
              fade = FALSE,
              doPlot = TRUE,
              mar = qmar
          )

          if (self$options$glassoLabels) {
              layout <- q_obj$layout
              n <- nrow(pcor)
              for (i in 1:(n - 1)) {
                  for (j in (i + 1):n) {
                      val <- pcor[i, j]
                      if (abs(val) > 1e-8) {
                          x1 <- layout[i, 1]
                          y1 <- layout[i, 2]
                          x2 <- layout[j, 1]
                          y2 <- layout[j, 2]
                          
                          x_mid <- (x1 + x2) / 2
                          y_mid <- (y1 + y2) / 2
                          
                          lbl <- sprintf("%.2f", val)
                          
                          sw <- strwidth(lbl, cex = 0.8)
                          sh <- strheight(lbl, cex = 0.8)
                          
                          rect(x_mid - sw/2 - 0.015, y_mid - sh/2 - 0.015,
                               x_mid + sw/2 + 0.015, y_mid + sh/2 + 0.015,
                               col = "white", border = "lightgray", lwd = 0.5)
                          
                          lbl_col <- ifelse(val > 0, posCol, negCol)
                          text(x_mid, y_mid, lbl, cex = 0.8, col = lbl_col)
                      }
                  }
              }
          }
          
          if (hasGroups) {
              legend(
                  x = 0,
                  y = -1.4,
                  xjust = 0.5,
                  yjust = 0.5,
                  legend = names(groups),
                  fill = nodeCols,
                  horiz = TRUE,
                  bty = "n",
                  cex = 1.0,
                  xpd = TRUE
              )
          }
          
          return(TRUE)
      }
))
