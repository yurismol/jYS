
# This file is a generated template, your changes will not be overwritten

mUIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mUIClass",
    inherit = mUIBase,
    private = list(
        lowercut = NULL,
        uppercut = NULL,

        .init=function() {
            #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
            private$.initOutputs()
            uitable  <- self$results$stat$uistat
            uitable$setNote('ui', paste(
                          .('<b>Uncertain Interval (UI)</b> is placed around the point of intersection between the two distributions with and without the targeted state.'),
                          .('<b>Uncertain Interval</b> is considered to be a range of test scores that is inconclusive and does not warrant a right decision.')))
            mcitable <- self$results$stat$mcistat
            mcitable$setNote('mci1', .('<b>More Certain Interval (MCI)</b> is interval outside the Uncertain Interval and divide into low and high parts.'))
            mcitable$setNote('mci2', .('<b>Concordance</b> - C-Statistic or AUC. The probability that a randomly chosen subject with the condition is correctly ranked higher than a randomly chosen subject without the condition'))
            mcitable$setNote('mci3', .('<b>Se</b> - sensitivity of the positive and negative classifications TP/(TP+FN)'))
            mcitable$setNote('mci4', .('<b>Sp</b> - specificity of the positive and negative classifications TN/(TN+FN)'))
            mcitable$setNote('mci5', .('<b>CCR</b> - Correct Classification Rate or accuracy of the positive and negative classifications (TP+TN)/(TN+FP+FN+TP)'))
            mcitable$setNote('mci6', .('<b>Balance</b> - balance between correct and incorrect classified (TP+TN)/(FP+FN)'))
            mcitable$setNote('mci7', .('<b>NPV</b> - Negative Predictive Value of the negative class TN/(TN+FN)'))
            mcitable$setNote('mci8', .('<b>PPV</b> - Positive Predictive Value of the positive class TP/(TN+FN)'))
            mcitable$setNote('mci9', .('<b>SNPV</b> - standardized negative predictive value of the negative class'))
            mcitable$setNote('mciA', .('<b>SPPV</b> - standardized positive predictive value of the positive class'))
            mcitable$setNote('mciA', .('<b>LR-</b> - Negative Likelihood Ratio P(-|D+))/(P(-|D-)). The probability of a person with the condition receiving a negative classification / probability of a person without the condition receiving a negative classification'))
            mcitable$setNote('mciA', .('<b>LR+</b> - Positive Likelihood Ratio (P(+|D+))/(P(+|D-)) The probability of a person with the condition receiving a positive classification / probability of a person without the condition receiving a positive classification'))

            if (self$options$youden) {
              uitable$getColumn('int')$setTitle(.("Threshold on Youden"))
            } else {
              uitable$getColumn('int')$setTitle(.("Intersection"))
            }

            if (self$options$isProp) {
              ref0 <- self$options$refval
              ref  <- self$options$ref
              ref1 <- ""
              if (length(self$data) > 0 && ref %in% names(self$data)) {
                fct  <- self$data[[ref]]
                lvl  <- levels(fct)
                if (!is.null(lvl) && length(lvl) > 0) {
                  ref1 <- toString(lvl[lvl!=ref0])
                }
              }

              intable <- self$results$stat$intstat
              intable$addColumn(name="norm", title=.("Normality p-value"), type="number", format='pvalue')
              intable$addColumn(name="mciL", title=jmvcore::format(.("MCI=0 ({})"), ref0), type="integer")
              intable$addColumn(name="ui",   title=.("UI"),    type="integer")
              intable$addColumn(name="mciU", title=jmvcore::format(.("MCI=1 ({})"), ref1), type="integer")
              intable$addColumn(name="sum",  title=.("Sum"),   type="integer")
              intable$setNote('flag', .('<b>*p<0.05</b> - The hypothesis of normal distribution was rejected by the Shapiro-Wilk test'))
            }
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

        .replaceLevels = function() {
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
            return(dat)
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

            if (self$options$isProp) {
              intable <- self$results$stat$intstat
            }

            uitable <- self$results$stat$uistat
            mcitable <- self$results$stat$mcistat

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
                       Se.criterion=self$options$minSe/100, Sp.criterion=self$options$minSp/100)
                lth <- p["L"]; uth = p["U"]
              } else if (self$options$UImethod=="UI") {
                if (private$.columnType(dat[[var]])=="ordinal2") {
                  ui  <- UncertainInterval::ui.ordinal(Vref, Vtest)
                  lth <- ui[1]; uth = ui[2]
                } else {
                  if (self$options$model=="binormal") {
                    ui  <- UncertainInterval::ui.binormal(Vref, Vtest,
                           UI.Se=self$options$uiSe/100, UI.Sp=self$options$uiSp/100)
                    lth <- ui$solution["L"]; uth = ui$solution["U"]
                  } else {
                    ui  <- UncertainInterval::ui.nonpar(Vref, Vtest,
                           UI.Se=self$options$uiSe/100, UI.Sp=self$options$uiSp/100)
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

              if (self$options$isProp) {
                t  <- t(statMCI$table)
                Vr <- dat[[ref]]
                Vt <- dat[[var]]
                sw <- try(shapiro.test(Vt))
                if (jmvcore::isError(sw)) norm0 <- ""
                else norm <- sw$p.value
                df <- subset(Vt, Vr==0)
                sw <- try(shapiro.test(df))
                if (jmvcore::isError(sw)) norm0 <- ""
                else norm0 <- sw$p.value
                df <- subset(Vt, Vr==1)
                sw <- try(shapiro.test(df))
                if (jmvcore::isError(sw)) norm1 <- ""
                else norm1 <- sw$p.value
                intable$addRow(rowKey=norm0, list(var=var, norm=norm0, lev=ref0,     mciL=t[1,1], ui=t[1,2], mciU=t[1,3], sum=t[1,1]+t[1,2]+t[1,3]))
                if (norm0<0.05) intable$addSymbol(rowKey=norm0, "norm", '*')
                intable$addRow(rowKey=norm1, list(var="", norm=norm1, lev=ref1,     mciL=t[2,1], ui=t[2,2], mciU=t[2,3], sum=t[2,1]+t[2,2]+t[2,3]))
                if (norm1<0.05) intable$addSymbol(rowKey=norm1, "norm", '*')
                intable$addRow(rowKey=norm,  list(var="", norm=norm,  lev=.("Sum"), mciL=t[3,1], ui=t[3,2], mciU=t[3,3], sum=t[3,1]+t[3,2]+t[3,3]))
                if (norm<0.05) intable$addSymbol(rowKey=norm, "norm", '*')
              }

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
                LRm  =ind["LR-"],
                LRp  =ind["LR+"],
                C    =ind["C"]
                ))

              image <- self$results$plotsMD$get(key=var)
              image$setState(list(lth=lth, uth=uth))

              if (self$options$show_roc_table) {
                r <- try(pROC::roc(Vref, Vtest, quiet=TRUE), silent=TRUE)
                if (!inherits(r, "try-error")) {
                  auc_val <- as.numeric(pROC::auc(r))
                  coords <- try(pROC::coords(r, x="best", best.method="youden", ret=c("threshold", "specificity", "sensitivity")), silent=TRUE)
                  if (!inherits(coords, "try-error")) {
                    # pROC::coords returns data.frame in newer versions, matrix in older
                    if (is.data.frame(coords)) {
                      cutoff <- as.numeric(coords[1, "threshold"])
                      sp     <- as.numeric(coords[1, "specificity"])
                      se     <- as.numeric(coords[1, "sensitivity"])
                    } else {
                      if (is.matrix(coords)) {
                        coords <- coords[1, ]
                      }
                      cutoff <- as.numeric(coords["threshold"])
                      sp <- as.numeric(coords["specificity"])
                      se <- as.numeric(coords["sensitivity"])
                    }
                    direction <- as.character(r$direction)
                    
                    self$results$stat$rocTable$setRow(rowKey=var, list(
                      auc = auc_val,
                      cutoff = cutoff,
                      se = se,
                      sp = sp,
                      direction = direction
                    ))
                  }
                }
              }

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
            dat  <- private$.replaceLevels()
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
            decopt <- getOption("OutDec")
            options(OutDec=".")
	    p    <- UncertainInterval::TG.ROC(Vref, Vtest, plot=TRUE,
                    model=self$options$model,
                    Se.criterion=self$options$minSe/100, Sp.criterion=self$options$minSp/100)
            options(OutDec=decopt)
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
            dat  <- private$.replaceLevels()

            cols <- RColorBrewer::brewer.pal(n=8, name="Dark2")
            col  <- cols[which(test==key)]
            #self$results$text$setContent(dat[[ref]])
            show_roc_cut <- self$options$show_roc_cut
	    p <- pROC::plot.roc(dat[[ref]], dat[[key]], col=col,
                 legacy.axes=FALSE, thresholds="best",
                 print.thres=show_roc_cut, print.thres.col=col, print.auc=TRUE, print.thres.pch=19,
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
            dat  <- private$.replaceLevels()

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
