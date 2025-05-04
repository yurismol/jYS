
# This file is a generated template, your changes will not be overwritten

mXLSClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mXLSClass",
    inherit = mXLSBase,
    private = list(
        .run = function() {
            data <- data.frame(self$data, check.names=FALSE)
            data <- jmvcore::select(data, self$options$vars)
            self$results$text$setContent("")
            if (self$options$xlsexport) {
		file <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile="TEST.xlsx",
			title=.("Save file as"), defaultextension=".xlsx",
			filetypes = .('{"Excel files" {.xls}}')))
		file <- "c:/!!/test.xlsx"
                if (file>"") {
                  colnames(data) <- iconv(colnames(data), "UTF-8", "")
                  openxlsx::write.xlsx(data, file=file,
		    firstActiveRow=2, firstActiveCol=1, colWidths="auto",
		    col.names=TRUE, row.names=FALSE, sheetName="RES",
		    showNA=TRUE, overwrite=TRUE)
                  self$results$text$setContent("SAVED...")
                }
            }

        })
)
