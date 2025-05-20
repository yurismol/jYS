# This file is a generated template, your changes will not be overwritten

mMFClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMFClass",
    inherit = mMFBase,
    private = list(
        .init=function() {
            #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
            private$.initOutputs()
            table <- self$results$errors
            if (self$options$alg=="mF") {
              table$addColumn(name="err", title="MSE", type='number')
              contErr = .('MSE - mean squared error (for Continuous variables);')
            } else {
              table$addColumn(name="err", title="PVU", type='number')
              contErr = .('PVU - proportion of variance unexplained 1-R\u00B2 (for Continuous variables);')
            }
            table$setNote('obe', paste(
                          .('N - number of imputted values;'),
                          .('PFC - proportion of falsely classified (for Nominal and Ordinal variables)'),
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

            if (self$options$imputeOV && self$results$imputeOV$isNotFilled()) {
                #self$results$text$setContent(.("Wait for calculation..."))
                table <- self$results$errors
                learn <- self$options$learnvar
                keys  <- self$options$imputevar
                dat   <- data.frame(self$data, check.names=FALSE)
                dat   <- jmvcore::select(dat, c(learn, keys))
                if (ncol(dat)<3) {
		  jmvcore::reject(.("Minimum 3 variables (Training + Imputing) are required"), code='')
                }
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
                #self$results$text$setContent(rf$pred_errors)

                keys <- table$rowKeys
                for (i in seq_along(keys)) {
                    key <- keys[[i]]
                    d   <- dat[[key]]
                    oo  <- oob[key]
                    nr  <- length(out[[key]]) - length(d[!is.na(d)])
                    if (is.na(oo) || nr==0) oo <- '\u2014'
                    if (private$.columnType(d)=="continuous") {
                      tableRow <- list(ninp=nr, err=oo, pfc='\u2014')
                    } else {
                      tableRow <- list(ninp=nr, err='\u2014', pfc=oo)
                    }
                    table$setRow(rowKey=key, tableRow)
                    if (private$.columnType(d)=="continuous") {
                      dec <- decimalplaces(d)
                      self$results$imputeOV$setValues(index=i, round(out[[key]], dec))
                    } else {
                      self$results$imputeOV$setValues(index=i, out[[key]])
                    }
                }
            } else {
                #self$results$text$setContent(.('<h2>For computation select variables and check the "Model the missing values"</h2>'))
            }
        }
  )
)
