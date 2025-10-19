
# This file is a generated template, your changes will not be overwritten

mUIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mUIClass",
    inherit = mUIBase,
    private = list(
        shared_data = NULL,
        lowercut = NULL,
        uppercut = NULL,

        .init=function() {
            #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
            private$.initOutputs()
            uitable  <- self$results$stat$uistat
            mcitable <- self$results$stat$mcistat
            mcitable$setNote('mci', paste(
                          .('CCR - Correct Classification Rate or accuracy of the positive and negative classifications (TP+TN)/(TN+FP+FN+TP)'), "<br>",
                          .('Balance - balance between correct and incorrect classified (TP+TN)/(FP+FN)'), "<br>",
                          .('Sp - specificity of the positive and negative classifications TN/(TN+FN)'), "<br>",
                          .('Se - sensitivity of the positive and negative classifications TP/(TP+FN)'), "<br>",
                          .('NPV - Negative Predictive Value of the negative class TN/(TN+FN)'), "<br>",
                          .('PPV - Positive Predictive Value of the positive class TP/(TN+FN)'), "<br>",
                          .('SNPV - standardized negative predictive value of the negative class'), "<br>",
                          .('SPPV - standardized positive predictive value of the positive class'), "<br>",
                          .('Concordance - C-Statistic or AUC. The probability that a random chosen patient with the condition is correctly ranked higher than a randomly chosen patient without the condition. Equal to AUC, with for the more certain interval a higher outcome than the overall concordance')
            ))
        },

        .initOutputs=function() {
            description = function(part1, part2=NULL) {
                return(
                    jmvcore::format(
                        .("{varType} proposed decision"),
                        varType=part1,
                        modelNo=ifelse(is.null(part2), "", paste0(" ", part2))
                    )
                )
            }
            title = function(part1=NULL, part2=NULL) {
                return(jmvcore::format("{} ({})", part2, part1))
            }

            keys <- self$options$test
            measureTypes <- sapply(keys, function(x) "factor")
            titles <- vapply(keys, function(key) title(.("decision"), key), '')
            descriptions <- vapply(keys, function(key) description(key), '')
            self$results$decision$set(keys, titles, descriptions, measureTypes)
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

        .run = function() {
            ref  <- self$options$ref
            refv <- self$options$refval
            test <- self$options$test
            dat  <- data.frame(self$data, check.names=FALSE)
            dat  <- jmvcore::select(dat, c(ref, test))
            fct  <- dat[,1]
            lvl  <- levels(fct)
            fct  <- ifelse(fct==refv, 0, 1)
            dat[,1] <- factor(fct)
            #self$results$text$setContent(dat)
            private$shared_data <- dat

            ref  <- self$options$ref
            test <- self$options$test
            uitable  <- self$results$stat$uistat
            mcitable <- self$results$stat$mcistat
            mcitable$addColumn(name="MCI.CCR", title="CCR", type="number")
            mcitable$addColumn(name="MCI.bal", title=.("Balance"), type="number")
            mcitable$addColumn(name="MCI.Se",  title="Se",  type="number")
            mcitable$addColumn(name="MCI.Sp",  title="Sp",  type="number")
            mcitable$addColumn(name="MCI.NPV", title="NPV", type="number")
            mcitable$addColumn(name="MCI.PPV", title="PPV", type="number")
            mcitable$addColumn(name="MCI.SNPV", title="SNPV", type="number")
            mcitable$addColumn(name="MCI.SPPV", title="SPPV", type="number")
            mcitable$addColumn(name="Prevalence", title=.("Prevalence"), type="number")
            mcitable$addColumn(name="MCI.C", title=.("Concordance"), type="number")
            if (self$options$UImethod!="noUI") {
              uitable$addColumn(name="lth", title=.("Lower threshold"), type="number")
              uitable$addColumn(name="uth", title=.("Upper threshold"), type="number")
            }
            for (i in seq_along(test)) {
              var <- test[[i]]
              model=ifelse(self$options$model=="none","kernel","binormal")
              if (self$options$UImethod=="TGR") {
                p   <- UncertainInterval::TG.ROC(dat[[ref]], dat[[var]], plot=FALSE,
                       model=self$options$model,
                       Se.criterion=self$options$minSe, Sp.criterion=self$options$minSp)
                lth <- p["L"]; uth = p["U"]
              } else if (self$options$UImethod=="UI") {
                if (private$.columnType(dat[[var]])=="ordinal2") {
                  ui  <- UncertainInterval::ui.ordinal(dat[[ref]], dat[[var]])
                  lth <- ui[1]; uth = ui[2]
                } else {
                  if (self$options$model=="binormal") {
                    ui  <- UncertainInterval::ui.binormal(dat[[ref]], dat[[var]],
                           UI.Se=self$options$uiSe, UI.Sp=self$options$uiSp)
                    lth <- ui$solution["L"]; uth = ui$solution["U"]
                  } else {
                    ui  <- UncertainInterval::ui.nonpar(dat[[ref]], dat[[var]],
                           UI.Se=self$options$uiSe, UI.Sp=self$options$uiSp)
                    lth <- ui[1]; uth = ui[2]
                  }
                }
              } else {
                if (self$options$youden) {
                  roc  <- pROC::roc(dat[[ref]], dat[[var]])
                  best <- pROC::coords(roc, "best", ret=c("threshold", "specificity", "1-npv")[1])
                  lth <- best[1,]; uth = best[1,]
                } else {
                  ui <- UncertainInterval::get.intersection(dat[[ref]], dat[[var]], model=model)
                  lth <- ui[length(ui)]; uth = ui[length(ui)]
                }
                int <- lth
              }
              statMCI <- UncertainInterval::quality.threshold(dat[[ref]], dat[[var]], threshold=lth, threshold.upper=uth, model=model)
              #self$results$text$setContent(statMCI)

              if (lth!=uth) {
                statUI  <- UncertainInterval::quality.threshold.uncertain(dat[[ref]], dat[[var]], lth, uth, model=model)
                int     <- statUI$intersection
              }

              uitable$setRow(rowKey=var, list(dir=statMCI$direction, int=int, lth=lth, uth=uth))

              mcitable$setRow(rowKey=var, list(
                Prevalence=statMCI$indices["Prevalence"],
                MCI.CCR=statMCI$indices["MCI.CCR"],
                MCI.bal=statMCI$indices["MCI.balance"],
                MCI.Se =statMCI$indices["MCI.Se"],
                MCI.Sp =statMCI$indices["MCI.Sp"],
                MCI.NPV=statMCI$indices["MCI.NPV"],
                MCI.PPV=statMCI$indices["MCI.PPV"],
                MCI.SNPV=statMCI$indices["MCI.SNPV"],
                MCI.SPPV=statMCI$indices["MCI.SPPV"],
                MCI.C   =statMCI$indices["MCI.C"]
                ))

              image <- self$results$plotsMD$get(key=var)
              image$setState(list(lth=lth, uth=uth))

              if (self$options$decision && self$results$decision$isNotFilled()) {
                uc  <- UncertainInterval::quality.threshold.uncertain(dat[[ref]], dat[[var]], lth, uth)
                dt  <- rep(NA, times=length(dat[[var]]))
                lvl <- lvl[lvl != refv]
                if (uc$direction=="<") {
                  dt[dat[[var]]<=lth] <- refv
                  dt[dat[[var]]>=uth] <- toString(lvl)
                } else {
                  dt[dat[[var]]<=lth] <- toString(lvl)
                  dt[dat[[var]]>=uth] <- refv
                }
                self$results$decision$setValues(index=i, dt)
              }
            }
        },

        .plotTGR=function(image, ggtheme, theme, ...) {
            ref  <- self$options$ref
            test <- self$options$test
            key  <- image$key
            dat  <- private$shared_data
            fct  <- dat[,1]
            lvl  <- levels(fct)
            
            #self$results$text$setContent(dat[[ref]])
            ua   <- UncertainInterval::ui.nonpar(dat[[ref]], dat[[key]])
            par  <- UncertainInterval::quality.threshold.uncertain(dat[[ref]], dat[[key]], ua[1], ua[2])
            if (par$direction=="<") cols <- c("red", "blue") else cols <- c("blue", "red")
	    p    <- UncertainInterval::TG.ROC(dat[[ref]], dat[[key]], plot=TRUE,
                    model=self$options$model,
                    Se.criterion=self$options$minSe, Sp.criterion=self$options$minSp)
	    lth  <- p["L"]; uth = p["U"]
            #title(xlab=key, ylab="Sens-Spec")
            #is <- get.intersection(dat[[ref]], dat[[key]], model=c("kernel", "binormal", "ordinal")[3])
            #text(par$intersection, 0, signif(par$intersection, 4))
            text(lth*0.95, 1, signif(lth, 4), col=cols[1])
            text(uth*1.05, 1, signif(uth, 4), col=cols[2])
            if (self$options$polygon) {
              x <- c(lth, uth, uth, lth)
              y <- c(-0.1, -0.1, 1.1, 1.1)
              polygon(x, y, angle=45, density=5, border=NA, col="red")
            }

            #text(lth*0.9, 1.02, ref)
            #text(uth*1.1, 1.02, toString(lvl))

            print(p)
            return(TRUE)
        },

        .plotROC=function(image, ggtheme, theme, ...) {
            ref  <- self$options$ref
            test <- self$options$test
            key  <- image$key
            dat  <- private$shared_data

            cols <- RColorBrewer::brewer.pal(n=8, name="Dark2")
            col  <- cols[which(test==key)]
            #self$results$text$setContent(dat[[ref]])
	    p <- pROC::plot.roc(dat[[ref]], dat[[key]], col=col,
                 legacy.axes=FALSE, thresholds="best",
                 print.thres=TRUE, print.thres.col=col, print.auc=TRUE, print.thres.pch=19,
                 ci=TRUE, ci.col=col, ci.type=c("bars", "shape", "no")[2],
                 xlab=.("Specificity"), ylab=.("Sensitivity"),
                 print.thres.best.method="youden")
            print(p)
            return(TRUE)
        },

        .plotMD=function(image, ggtheme, theme, ...) {
            state<- image$state
            ref  <- self$options$ref
            test <- self$options$test
            key  <- image$key
            dat  <- private$shared_data
            #self$results$text$setContent(dat[[ref]])
	    p <- UncertainInterval::plotMD(dat[[ref]], dat[[key]], plot=TRUE,
                intersection='Youden',
		model=ifelse(self$options$model=="none","kernel","binormal"))
            if (self$options$UImethod!="noUI") {
              abline(v=state$lth, col="red", lty=2, lwd=2)
              abline(v=state$uth, col="red", lty=2, lwd=2)
              if (self$options$polygon) {
                x <- c(state$lth, state$uth, state$uth, state$lth)
                y <- c(-0.1, -0.1, 1, 1)
                polygon(x, y, angle=45, density=5, border=NA, col="red")
              }
            } else if (self$options$youden) {
              roc  <- pROC::roc(dat[[ref]], dat[[key]])
              best <- pROC::coords(roc, "best", ret=c("threshold", "specificity", "1-npv")[1])
              abline(v=best, col="green", lty=3, lwd=3)
            }
            print(p)
            return(TRUE)
        }
  )
)
