
# This file is a generated template, your changes will not be overwritten

mMFClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMFClass",
    inherit = mMFBase,
    private = list(
        .init=function() {
            private$.initOutputs()
            table <- self$results$errors
            table$setNote('obe', paste(
                          .('N - number of imputted values;'),
                          .('MSE - mean squared error (for Continuous variables);'),
                          .('PFC - proportion of falsely classified (for Nominal and Ordinal variables)')
            ))
            #keys  <- self$options$imputevar
            #for (i in seq_along(keys)) 
            #  table$addRow(rowKey=keys[i], list(mse='', pfc=''))
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
                mf <- missForest::missForest(dat, maxiter=self$options$maxiter,
			 ntree=self$options$ntree, replace=TRUE,
			 variablewise=TRUE)

                # normalized root mean squared error (NRMSE)
                # proportion of falsely classified (PFC)
                self$results$imputeOV$setRowNums(rownames(self$data))

                keys <- table$rowKeys
                for (i in seq_along(keys)) {
                    key <- keys[[i]]
                    d   <- dat[[key]]
                    nr  <- length(mf$ximp[[key]]) - length(d[!is.na(d)])
                    err <- mf$OOBerror[length(learn)+i]
                    if (names(err) == "MSE") {
                      tableRow <- list(ninp=nr, mse=err, pfc='\u2014')
                    } else {
                      tableRow <- list(ninp=nr, mse='\u2014', pfc=err)
                    }
                    table$setRow(rowKey=key, tableRow)
                    self$results$imputeOV$setValues(index=i, mf$ximp[[key]])
                    #self$results$text$setContent(keys)
                }
            } else {
                #self$results$text$setContent(.('<h2>For computation select variables and check the "Model the missing values"</h2>'))
            }
        }
  )
)
