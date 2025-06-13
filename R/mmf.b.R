# This file is a generated template, your changes will not be overwritten

mMFClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMFClass",
    inherit = mMFBase,
    private = list(
        .init=function() {
            #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
            private$.initOutputs()
            mctable<- self$results$estim$mcar
            mtable <- self$results$estim$mars
            etable <- self$results$imput$errors
            mctable$setNote('mcar', paste("<br>",
                          .('MCAR - missing completely at random (if the p<sub>value</sub> is not significant, there is evidence the data is MCAR).')
            ))
            mtable$setNote('mar', paste("<br>",
                          .('N - number of missing values;'), "<br>",
                          .('MAR - missing at random (if each p<sub>value</sub> is significant, there is evidence the data is MAR);'), "<br>",
                          .('Explanatory - variable corresponding to MAR with minimal p<sub>value</sub>.')
            ))
            mtable$setNote('mcar_mar', paste("<br>",
                          .('If at least one p<sub>value</sub> MAR is not significant, and the p<sub>value</sub> in MCAR is significant then the data is MNAR (Missing Not At Random).')
            ))
 
            if (self$options$alg=="mF") {
              etable$addColumn(name="err", title="MSE", type='number')
              contErr = .('MSE - mean squared error (for Continuous variables);')
            } else {
              etable$addColumn(name="err", title="PVU", type='number')
              contErr = .('PVU - proportion of variance unexplained 1-R\u00B2 (for Continuous variables);')
            }
            etable$setNote('obe', paste("<br>",
                          .('N - number of imputted values;'), "<br>",
                          .('PFC - proportion of falsely classified (for Nominal and Ordinal variables);'), "<br>",
                          contErr
            ))
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
          dat <- data.frame(self$data, check.names=FALSE)
          dat <- jmvcore::select(dat, self$options$imputevar)
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
          p$labels$x <- .("Number of missing entries by variable")
          p$labels$y <- .("Pattern frequency")
          p <- p +
		ggplot2::scale_fill_manual(values=fill, labels=c(.("missing"), .("observed"))) +
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
          xscale$secondary.axis$name <- .("Column name")
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
          dat <- data.frame(self$data, check.names=FALSE)
          dat <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))
          p <- ggmice::plot_flux(data=dat, label=TRUE)
          p <- p + 
		   ggplot2::theme(text=ggplot2::element_text(size=ggtheme[[1]]$text$size))

          p$labels$x <- .("Influx*")
          p$labels$y <- .("OutFlux**")
          p$labels$caption <- paste0(.("*connection of a variable's missingness indicator"), "\n", .("with observed data on other variables"),
		"\n",
		.("**connection of a variable's observed data"), "\n", .("with missing data on other variables")
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

          dat <- data.frame(self$data, check.names=FALSE)
          dat <- jmvcore::select(dat, c(self$options$learnvar, self$options$imputevar))

          p <- ggmice::plot_corr(data=dat, square=TRUE, rotate=TRUE, label=TRUE)
          #self$results$text$setContent(c(p))
          p <- p + 
		   ggplot2::theme(text=ggplot2::element_text(size=ggtheme[[1]]$text$size))

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
            if (ncol(dat)<minVar && (self$options$isMAR || self$options$imputeOV)) {
		jmvcore::reject(jmvcore::format(
			.("Minimum {minVar} variables (Training + Imputing) are required"),
                        minVar=minVar), code='')
            }
            mar   <- missr::mar(dat)
            mcar  <- missr::mcar(dat)
            marp  <- mar$p_value;     names(marp) <- mar$missing
            mare  <- mar$explanatory; names(mare) <- mar$missing
            mctable<- self$results$estim$mcar
            mctable$addRow(rowKey='MCAR',
		list(pval=signif(mcar$p_val, 3),
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
    		    m  <- m[order(unlist(m))]
		    nm <- names(m)
		    for (i in seq_along(m)) {
		      table$addRow(rowKey=nm[i], list(exp=nm[i], pval=m[i]))
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
                  tableRow <- list(ninp=nr, exp='\u2014', mar='\u2014')
                } else {
                  tableRow <- list(ninp=nr, exp=mare[key], mar=marp[key])
                }
                mtable$setRow(rowKey=key, tableRow)
            }
            if (self$options$imputeOV && self$results$imputeOV$isNotFilled()) {
                etable<- self$results$imput$errors
                if (nrow(dat)<3) {
		  jmvcore::reject(.("Empty data table"), code='')
                }
                # normalized root mean squared error (NRMSE)
                # proportion of falsely classified (PFC)
                if (self$options$setseed) {
                   if (self$options$seed>0) set.seed(self$options$seed)
                }
                if (self$options$alg=="mF") {
                  rf <- missForest::missForest(dat, maxiter=self$options$maxiter,
			ntree=self$options$ntree, replace=TRUE,
			variablewise=TRUE)
                  oob <- rf$OOBerror
                  names(oob) <- colnames(dat)
                  out <- rf$ximp
                } else {
                  rf <- missRanger::missRanger(dat, data_only=FALSE, returnOOB=FALSE,
			maxiter=self$options$maxiter, num.trees=self$options$ntree,
			pmm.k=self$options$pmmk)	#, seed=self$options$seed
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
                    if (is.na(oo) || nr==0) oo <- '\u2014'
                    if (private$.columnType(d)=="continuous") {
                      tableRow <- list(ninp=nr, err=oo, pfc='\u2014')
                      dec <- decimalplaces(d)
                      self$results$imputeOV$setValues(index=i, round(out[[key]], dec))
                    } else {
                      tableRow <- list(ninp=nr, err='\u2014', pfc=oo)
                      self$results$imputeOV$setValues(index=i, out[[key]])
                    }
                    etable$setRow(rowKey=key, tableRow)
                }
            } else {
            }
        }
  )
)
