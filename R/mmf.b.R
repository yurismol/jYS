# This file is a generated template, your changes will not be overwritten

mMFClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMFClass",
    inherit = mMFBase,
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

        .init=function() {
            #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
            private$.initOutputs()

            #if (length(self$options$learnvar)+length(self$options$imputevar)==0) {
            #  msg <- .("<h2>Help</h2><div>Please select complete and incomplete variables</div>")
            #  self$results$help$setVisible(TRUE)
            #  self$results$help$setContent(msg)
            #} else {
            #  self$results$help$setVisible(FALSE)
            #}

            mctable<- self$results$estim$mcar
            mtable <- self$results$estim$mars
            etable <- self$results$imput$errors
            mctable$setNote('mcar', paste(
                          .('<b>MCAR</b> - missing completely at random (if the p<sub>value</sub> is not significant, there is evidence the data is MCAR).')
            ))
            mtable$setNote('mar', paste(
                          .('<b>N</b> - number of missing values;'),
                          .('<b>MAR</b> - missing at random (if each p<sub>value</sub> is significant, there is evidence the data is MAR);'),
                          .('<b>Explanatory</b> - variable corresponding to MAR with minimal p<sub>value</sub>.')
            ))
            mtable$setNote('mcar_mar', paste(
                          .('If at least one p<sub>value</sub> MAR is not significant, and the p<sub>value</sub> in MCAR is significant then the data is MNAR (Missing Not At Random).')
            ))
 
            if (self$options$alg=="mF") {
              etable$addColumn(name="err", title=.("MSE"), type='number')
              contErr = .('<b>MSE</b> - mean squared error (for Continuous variables);')
            } else {
              etable$addColumn(name="err", title=.("PVU"), type='number')
              contErr = .('<b>PVU</b> - proportion of variance unexplained 1-R\u00B2 (for Continuous variables);')
            }
            etable$setNote('obe', paste(
                          .('<b>N</b> - number of missing values;'),
                          .('<b>PFC</b> - proportion of falsely classified (for Nominal and Ordinal variables);'),
                          contErr
            ))

            if (self$options$fullmars) {
                tables <- self$results$estim$fMARtab
                keys   <- self$options$imputevar
                all_vars <- c(self$options$learnvar, keys)
                for (tab in keys) {
                    table <- tables$get(key=tab)
                    other_vars <- all_vars
                    for (v in other_vars) {
                        rk <- gsub(" ", ".", v)
                        table$addRow(rowKey=rk, list(exp=v))
                    }
                }
            }
        },

        .run = function() {
            private$.populateOutputs()
        },

        .initOutputs=function() {
            description = function(part1, part2=NULL) {
                return(
                    jmvcore::format(
                        .("{varType} with imputed values"),
                        varType=part1,
                        modelNo=ifelse(is.null(part2), "", paste0(" ", part2))
                    )
                )
            }
            title = function(part1=NULL, part2=NULL) {
                return(jmvcore::format("{} ({})", part2, part1))
            }

            keys <- self$options$imputevar
            measureTypes <- sapply(keys, function(x) private$.columnType(self$data[[x]]))

            titles <- vapply(keys, function(key) title(.("imp"), key), '')
            descriptions <- vapply(keys, function(key) description(key), '')
            self$results$imputeOV$set(keys, titles, descriptions, measureTypes)
        },

        .columnType = function(column) {
            if (inherits(column, "ordered")) {
                return("ordinal")
            } else if (inherits(column, "factor")) {
                return("nominal")
            } else {
                return("continuous")
            }
        },

        .plot=function(image, ggtheme, theme, ...) {
          if (length(self$options$imputevar)<2) {
             jmvcore::reject(jmvcore::format(
		.("Minimum 2 impute variables are required")), code='')
             return(FALSE)
          }

          private$.ensureData()
          dat <- data.frame(self$data, check.names=FALSE)
          if (self$options$compinres) dat <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))
          else dat <- jmvcore::select(dat, self$options$imputevar)

          #fill <- jmvcore::colorPalette(n=2, theme$palette, type="fill")
          fill <- jmvcore::colorPalette(n=2, "Set1", type="fill")
          if (self$options$npat>0) {
            p <- ggmice::plot_pattern(data=dat, square=TRUE, rotate=TRUE,
		npat=self$options$npat)
          } else {
            p <- ggmice::plot_pattern(data=dat, square=TRUE, rotate=TRUE)
          }
          #self$results$text$setContent(sum(is.na(dat)))
          p$labels$caption <- jmvcore::format(
			.("Total number of missing entries {}"),
			sum(is.na(dat))
			)
          p$labels$x <- .("Number of missing entries per variable")
          p$labels$y <- .("Pattern frequency")
          p <- p +
		ggplot2::scale_fill_manual(values=fill, labels=c(.("missing"), .("Observed"))) +
		ggplot2::theme(text=ggplot2::element_text(size=ggtheme[[1]]$text$size))
	  if (self$options$anghead) {
		p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=0, hjust=0))
		p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=45, hjust=0))
          } else {
		p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=0, hjust=0))
		p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=90, hjust=0))
          }

          pb <- ggplot2::ggplot_build(p)
          xscale <- pb$layout$panel_scales_x[[1]]
          xscale$secondary.axis$name <- .("Variable name")
          yscale <- pb$layout$panel_scales_y[[1]]
          yscale$secondary.axis$name <- paste0(.("Number of missing entries"),
					"\n", .("per pattern"))

	  print(p)
	  return(TRUE)
        },

        .fplot=function(image, ggtheme, theme, ...) {
          if (length(self$options$learnvar)+length(self$options$imputevar)<2) {
             jmvcore::reject(jmvcore::format(
		.("Minimum 2 impute variables are required")), code='')
             return(FALSE)
          }

          private$.ensureData()
          dat <- data.frame(self$data, check.names=FALSE)
          if (self$options$compinres) dat <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))
          else dat <- jmvcore::select(dat, self$options$imputevar)

          p <- ggmice::plot_flux(data=dat, label=TRUE)
          p <- p + 
		   ggplot2::theme(text=ggplot2::element_text(size=ggtheme[[1]]$text$size))

          p$labels$x <- paste0(.("Influx"), "*")
          p$labels$y <- paste0(.("OutFlux"), "**")
          p$labels$caption <- paste0(
              "* ", .("Connection of a variable's missingness indicator with observed data on other variables"), "\n",
              "** ", .("Connection of a variable's observed data with missing data on other variables")
          )

	  print(p)
	  return(TRUE)
        },

        .cplot=function(image, ggtheme, theme, ...) {
          if (length(self$options$learnvar)+length(self$options$imputevar)<2) {
             jmvcore::reject(jmvcore::format(
		.("Minimum 2 impute variables are required")), code='')
             return(FALSE)
          }

          private$.ensureData()
          dat <- data.frame(self$data, check.names=FALSE)
          if (self$options$compinres) dat <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))
          else dat <- jmvcore::select(dat, self$options$imputevar)

          p <- ggmice::plot_corr(data=dat, square=TRUE, rotate=TRUE, label=TRUE)
          #self$results$text$setContent(c(p))
          p <- p + 
		   ggplot2::theme(text=ggplot2::element_text(size=ggtheme[[1]]$text$size))

	  if (self$options$anghead) {
		p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=0, hjust=0))
		p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=45, hjust=0))
          } else {
		p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=0, hjust=0))
		p <- p + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=90, hjust=0))
          }


          p$labels$x <- .("Imputation model predictor")
          p$labels$y <- .("Variable to impute")
          p$labels$caption <- .("*pairwise complete observations")
          p$labels$fill <- paste0(.("Correlation"), "*\n      ")

	  print(p)
	  return(TRUE)
        },

        .populateOutputs=function() {
            decimalplaces <- function(col) {
              maxd = 0
              for (x in col) {
                if (!is.na(x) && (x%%1)!= 0) {
                  maxd = max(maxd, nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][2]))
                }
              }
              return(maxd)
            }
            dat   <- data.frame(self$data, check.names=FALSE)
            dat   <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))

            minVar <- 3
            #if (ncol(dat)<minVar && (self$options$isMAR || self$options$imputeOV)) {
            if (ncol(dat)<minVar) {
		jmvcore::reject(jmvcore::format(
			.("Minimum {minVar} variables (Complete + Incomplete) are required"),
                        minVar=minVar), code='')
            }
            mar   <- missr::mar(dat)
            mcar  <- missr::mcar(dat)
            marp  <- mar$p_value;     names(marp) <- mar$missing
            mare  <- mar$explanatory; names(mare) <- mar$missing
            mctable<- self$results$estim$mcar
            mctable$setRow(rowNo=1,
		list(pval=mcar$p_val,
		     df=mcar$degrees_freedom,
		     d2=round(mcar$statistic, 2),
		     mpat=mcar$missing_patterns)
            )

            if (self$options$fullmars && self$options$isMAR) {
                tables <- self$results$estim$fMARtab
		keys   <- self$options$imputevar
		marc   <- mar$combined
		for (tab in keys) {
		  d  <- dat[[tab]]
		  nr <- length(d[is.na(d)])
		  if (nr>0) {
		    table <- tables$get(key=tab)
		    tt <- gsub(" ", ".", tab)
		    m  <- marc[[tt]]
		    nm <- names(m)
		    for (i in seq_along(m)) {
		      table$setRow(rowKey=nm[i], list(pval=m[i]))
		    }
		  }
		}
            }

            mtable<- self$results$estim$mars
            keys  <- mtable$rowKeys
            for (i in seq_along(keys)) {
                key <- keys[[i]]
                d   <- dat[[key]]
                nr  <- length(d[is.na(d)])
                if (nr==0) {
                   tableRow <- list(ninp=nr, exp='\u2013', mar='\u2013')
                } else {
                  tableRow <- list(ninp=nr, exp=mare[key], mar=marp[key])
                }
                mtable$setRow(rowKey=key, tableRow)
            }
            if (self$options$imputeOV && self$results$imputeOV$isNotFilled()) {
                etable <- self$results$imput$errors
                if (nrow(dat)<3) {
		  jmvcore::reject(.("Empty data table"), code='')
                }
                # normalized root mean squared error (NRMSE)
                # proportion of falsely classified (PFC)
                if (self$options$setseed) {
                   if (self$options$seed>0) set.seed(self$options$seed)
                }
                private$.checkpoint()
                if (self$options$alg=="mF") {
                  rf <- tryCatch(
                      missForest::missForest(dat, maxiter=self$options$maxiter,
			ntree=self$options$ntree, replace=TRUE,
			variablewise=TRUE),
                      error = function(e) {
                          jmvcore::reject(jmvcore::format(.("Imputation failed: {}"), e$message), code='')
                      }
                  )
                  oob <- rf$OOBerror
                  names(oob) <- colnames(dat)
                  out <- rf$ximp
                } else {
                  rf <- tryCatch(
                      missRanger::missRanger(dat, data_only=FALSE, returnOOB=FALSE,
			maxiter=self$options$maxiter, num.trees=self$options$ntree,
			pmm.k=self$options$pmmk),	#, seed=self$options$seed
                      error = function(e) {
                          jmvcore::reject(jmvcore::format(.("Imputation failed: {}"), e$message), code='')
                      }
                  )
                  oob <- rf$pred_errors[rf$best_iter,]
                  out <- rf$data
                }
                self$results$imputeOV$setRowNums(rownames(self$data))
                #self$results$text$setContent(marp)

                keys <- etable$rowKeys
                for (i in seq_along(keys)) {
                    key <- keys[[i]]
                    d   <- dat[[key]]
                    nr  <- length(out[[key]]) - length(d[!is.na(d)])
                    oo  <- oob[key]
                     if (is.na(oo) || nr==0) oo <- '\u2013'
                     if (private$.columnType(d)=="continuous") {
                       tableRow <- list(ninp=nr, err=oo, pfc='\u2013')
                       dec <- decimalplaces(d)
                       self$results$imputeOV$setValues(index=i, round(out[[key]], dec))
                     } else {
                       tableRow <- list(ninp=nr, err='\u2013', pfc=oo)
                      self$results$imputeOV$setValues(index=i, out[[key]])
                    }
                    etable$setRow(rowKey=key, tableRow)
                }
            } else {
            }
        }
  )
)
