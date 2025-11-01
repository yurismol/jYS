
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
            uitable$setNote('ui', paste(
                          .('Uncertain Interval (UI) is placed around the point of intersection between the two distributions with and without the targeted state.'), "<br>",
                          .('Uncertain Interval is considered to be a range of test scores that is inconclusive and does not warrant a right decision.')))
            mcitable <- self$results$stat$mcistat
            mcitable$setNote('mci', paste(
                          .('More Certain Interval (MCI) is interval outside the Uncertain Interval and divide into low and high parts.'), "<br>",
                          .('Concordance - C-Statistic or AUC. The probability that a random chosen patient with the condition is correctly ranked higher than a randomly chosen patient without the condition'), "<br>",
                          .('Se - sensitivity of the positive and negative classifications TP/(TP+FN)'), "<br>",
                          .('Sp - specificity of the positive and negative classifications TN/(TN+FN)'), "<br>",
                          .('CCR - Correct Classification Rate or accuracy of the positive and negative classifications (TP+TN)/(TN+FP+FN+TP)'), "<br>",
                          .('Balance - balance between correct and incorrect classified (TP+TN)/(FP+FN)'), "<br>",
                          .('NPV - Negative Predictive Value of the negative class TN/(TN+FN)'), "<br>",
                          .('PPV - Positive Predictive Value of the positive class TP/(TN+FN)'), "<br>",
                          .('SNPV - standardized negative predictive value of the negative class'), "<br>",
                          .('SPPV - standardized positive predictive value of the positive class')
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
            ref0 <- self$options$refval
            test <- self$options$test
            dat  <- data.frame(self$data, check.names=FALSE)
            dat  <- jmvcore::select(dat, c(ref, test))
            fct  <- dat[,1]
            lvl  <- levels(fct)
            ref1 <- toString(lvl[lvl!=ref0])
            fct  <- ifelse(fct==ref0, 0, 1)
            dat[,1] <- factor(fct)
            private$shared_data <- dat

            intable <- self$results$stat$intstat
            intable$addColumn(name="mciL", title=.("MCI=0"), type="integer")
            intable$addColumn(name="ui",   title=.("UI"), type="integer")
            intable$addColumn(name="mciU", title=.("MCI=1"), type="integer")
            intable$addColumn(name="sum",  title=.("Sum"), type="integer")

            uitable <- self$results$stat$uistat
            if (self$options$youden) {
              uitable$addColumn(name="int", title=.("Threshold on Youden"), type="number")
            } else {
              uitable$addColumn(name="int", title=.("Intersection"), type="number")
            }

            mcitable <- self$results$stat$mcistat
            mcitable$addColumn(name="C",    title=.("Concordance"), type="number")
            mcitable$addColumn(name="Se",   title="Se",  type="number")
            mcitable$addColumn(name="Sp",   title="Sp",  type="number")
            mcitable$addColumn(name="CCR",  title="CCR", type="number")
            mcitable$addColumn(name="bal",  title=.("Balance"), type="number")
            mcitable$addColumn(name="NPV",  title="NPV", type="number")
            mcitable$addColumn(name="PPV",  title="PPV", type="number")
            mcitable$addColumn(name="SNPV", title="SNPV", type="number")
            mcitable$addColumn(name="SPPV", title="SPPV", type="number")
            mcitable$addColumn(name="Prevalence", title=.("Prevalence"), type="number")
            if (self$options$UImethod!="noUI") {
              uitable$addColumn(name="lth", title=.("Lower threshold"), type="number")
              uitable$addColumn(name="uth", title=.("Upper threshold"), type="number")
            }

            for (i in seq_along(test)) {
              var   <- test[[i]]
              Vref  <- dat[[ref]]
              Vtest <- dat[[var]]
              notna <- !(is.na(Vtest) | is.na(Vref))
              Vref  <- Vref[notna]
              Vtest <- Vtest[notna]
              
              model=ifelse(self$options$model=="none","kernel","binormal")
              if (self$options$UImethod=="TGR") {
                p   <- UncertainInterval::TG.ROC(Vref, Vtest, plot=FALSE,
                       model=self$options$model,
                       Se.criterion=self$options$minSe, Sp.criterion=self$options$minSp)
                lth <- p["L"]; uth = p["U"]
              } else if (self$options$UImethod=="UI") {
                if (private$.columnType(dat[[var]])=="ordinal2") {
                  ui  <- UncertainInterval::ui.ordinal(Vref, Vtest)
                  lth <- ui[1]; uth = ui[2]
                } else {
                  if (self$options$model=="binormal") {
                    ui  <- UncertainInterval::ui.binormal(Vref, Vtest,
                           UI.Se=self$options$uiSe, UI.Sp=self$options$uiSp)
                    lth <- ui$solution["L"]; uth = ui$solution["U"]
                  } else {
                    ui  <- UncertainInterval::ui.nonpar(Vref, Vtest,
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
                  ui <- UncertainInterval::get.intersection(Vref, Vtest, model=model)
                  lth <- ui[length(ui)]; uth = ui[length(ui)]
                }
                int <- lth
              }
              #if (lth==uth) {
              #  statMCI <- UncertainInterval::quality.threshold(Vref, Vtest, threshold=lth, model=model)
              #} else {
                statMCI <- UncertainInterval::quality.threshold(Vref, Vtest, threshold=lth, threshold.upper=uth, model=model)
              #}
              #self$results$text$setContent(statMCI)
              t <- t(statMCI$table)
              intable$addRow(rowKey=var, list(lev=ref0,     mciL=t[1,1], ui=t[1,2], mciU=t[1,3], sum=t[1,1]+t[1,2]+t[1,3]))
              intable$addRow(rowKey="" , list(lev=ref1,     mciL=t[2,1], ui=t[2,2], mciU=t[2,3], sum=t[2,1]+t[2,2]+t[2,3]))
              intable$addRow(rowKey="" , list(lev=.("Sum"), mciL=t[3,1], ui=t[3,2], mciU=t[3,3], sum=t[3,1]+t[3,2]+t[3,3]))

              if (lth!=uth) {
                statUI  <- UncertainInterval::quality.threshold.uncertain(Vref, Vtest, lth, uth, model=model)
                int     <- statUI$intersection
              }

              uitable$setRow(rowKey=var, list(dir=paste(ref0, statMCI$direction, ref1),
                             int=int, lth=lth, uth=uth))

              ind <- statMCI$indices
              names(ind) <- gsub("^MCI\\.", "", names(ind))
              mcitable$setRow(rowKey=var, list(
                Prevalence=ind["Prevalence"],
                CCR  =ind["CCR"],
                bal  =ind["balance"],
                Se   =ind["Se"],
                Sp   =ind["Sp"],
                NPV  =ind["NPV"],
                PPV  =ind["PPV"],
                SNPV =ind["SNPV"],
                SPPV =ind["SPPV"],
                C    =ind["C"]
                ))

              image <- self$results$plotsMD$get(key=var)
              image$setState(list(lth=lth, uth=uth))

              if (self$options$decision && self$results$decision$isNotFilled()) {
                dt  <- rep(NA, times=length(dat[[var]]))
                if (statMCI$direction=="<") {
                  dt[dat[[var]]<=lth] <- ref0
                  dt[dat[[var]]>=uth] <- ref1
                } else {
                  dt[dat[[var]]<=lth] <- ref1
                  dt[dat[[var]]>=uth] <- ref0
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
            Vref  <- dat[[ref]]
            Vtest <- dat[[key]]
            notna <- !(is.na(Vtest) | is.na(Vref))
            Vref  <- Vref[notna]
            Vtest <- Vtest[notna]
            
            #self$results$text$setContent(dat[[ref]])
            ua   <- UncertainInterval::ui.nonpar(Vref, Vtest)
            par  <- UncertainInterval::quality.threshold.uncertain(Vref, Vtest, ua[1], ua[2])
            if (par$direction=="<") cols <- c("red", "blue") else cols <- c("blue", "red")
	    p    <- UncertainInterval::TG.ROC(Vref, Vtest, plot=TRUE,
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
	    p <- UncertainInterval::plotMD(dat[[ref]], dat[[key]], plot=TRUE,
                intersection='Youden',
		model=ifelse(self$options$model=="none","kernel","binormal"))
            if (self$options$UImethod!="noUI") {
              abline(v=state$lth, col="red", lty=2, lwd=2)
              abline(v=state$uth, col="red", lty=2, lwd=2)
              if (self$options$polygon) {
                x <- c(state$lth, state$uth, state$uth, state$lth)
                y <- c(-0.1, -0.1, 100, 100)
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
