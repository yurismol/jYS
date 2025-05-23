
# This file is automatically generated, you probably don't want to edit this

mMFOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mMFOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            learnvar = NULL,
            imputevar = NULL,
            alg = "mF",
            maxiter = 10,
            ntree = 500,
            setseed = FALSE,
            seed = 123,
            pmmk = 0, ...) {

            super$initialize(
                package="jYS",
                name="mMF",
                requiresData=TRUE,
                ...)

            private$..learnvar <- jmvcore::OptionVariables$new(
                "learnvar",
                learnvar,
                required=FALSE)
            private$..imputevar <- jmvcore::OptionVariables$new(
                "imputevar",
                imputevar,
                takeFromDataIfMissing=TRUE,
                required=TRUE)
            private$..imputeOV <- jmvcore::OptionOutput$new(
                "imputeOV")
            private$..alg <- jmvcore::OptionList$new(
                "alg",
                alg,
                options=list(
                    "mF",
                    "mR"),
                default="mF")
            private$..maxiter <- jmvcore::OptionNumber$new(
                "maxiter",
                maxiter,
                default=10)
            private$..ntree <- jmvcore::OptionNumber$new(
                "ntree",
                ntree,
                default=500)
            private$..setseed <- jmvcore::OptionBool$new(
                "setseed",
                setseed,
                default=FALSE)
            private$..seed <- jmvcore::OptionNumber$new(
                "seed",
                seed,
                default=123)
            private$..pmmk <- jmvcore::OptionNumber$new(
                "pmmk",
                pmmk,
                default=0)

            self$.addOption(private$..learnvar)
            self$.addOption(private$..imputevar)
            self$.addOption(private$..imputeOV)
            self$.addOption(private$..alg)
            self$.addOption(private$..maxiter)
            self$.addOption(private$..ntree)
            self$.addOption(private$..setseed)
            self$.addOption(private$..seed)
            self$.addOption(private$..pmmk)
        }),
    active = list(
        learnvar = function() private$..learnvar$value,
        imputevar = function() private$..imputevar$value,
        imputeOV = function() private$..imputeOV$value,
        alg = function() private$..alg$value,
        maxiter = function() private$..maxiter$value,
        ntree = function() private$..ntree$value,
        setseed = function() private$..setseed$value,
        seed = function() private$..seed$value,
        pmmk = function() private$..pmmk$value),
    private = list(
        ..learnvar = NA,
        ..imputevar = NA,
        ..imputeOV = NA,
        ..alg = NA,
        ..maxiter = NA,
        ..ntree = NA,
        ..setseed = NA,
        ..seed = NA,
        ..pmmk = NA)
)

mMFResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mMFResults",
    inherit = jmvcore::Group,
    active = list(
        errors = function() private$.items[["errors"]],
        imputeOV = function() private$.items[["imputeOV"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Missing Values Imputation",
                refs=list(
                    "mf",
                    "rg",
                    "mr"))
            self$add(jmvcore::Table$new(
                options=options,
                name="errors",
                visible="(imputeOV)",
                title="Estimated out-of-bag (OOB) imputation error",
                rows="(imputevar)",
                columns=list(
                    list(
                        `name`="name", 
                        `title`="Variable", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="ninp", 
                        `title`="N", 
                        `type`="integer"),
                    list(
                        `name`="pfc", 
                        `title`="PFC", 
                        `type`="number"))))
            self$add(jmvcore::Output$new(
                options=options,
                name="imputeOV",
                title="Imputed misses"))}))

mMFBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mMFBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jYS",
                name = "mMF",
                version = c(1,0,7),
                options = options,
                results = mMFResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'none')
        }))

#' Missing Values Imputation
#'
#' 
#' @param data .
#' @param learnvar .
#' @param imputevar .
#' @param alg .
#' @param maxiter .
#' @param ntree .
#' @param setseed .
#' @param seed .
#' @param pmmk Number of candidate non-missing values to sample from in the
#'   predictive mean matching steps. 0 to avoid this step
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$errors} \tab \tab \tab \tab \tab OOB errors table \cr
#'   \code{results$imputeOV} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$errors$asDF}
#'
#' \code{as.data.frame(results$errors)}
#'
#' @export
mMF <- function(
    data,
    learnvar,
    imputevar,
    alg = "mF",
    maxiter = 10,
    ntree = 500,
    setseed = FALSE,
    seed = 123,
    pmmk = 0) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("mMF requires jmvcore to be installed (restart may be required)")

    if ( ! missing(learnvar)) learnvar <- jmvcore::resolveQuo(jmvcore::enquo(learnvar))
    if ( ! missing(imputevar)) imputevar <- jmvcore::resolveQuo(jmvcore::enquo(imputevar))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(learnvar), learnvar, NULL),
            `if`( ! missing(imputevar), imputevar, NULL))

    imputevar <- `if`( ! missing(imputevar), imputevar, colnames(data))

    options <- mMFOptions$new(
        learnvar = learnvar,
        imputevar = imputevar,
        alg = alg,
        maxiter = maxiter,
        ntree = ntree,
        setseed = setseed,
        seed = seed,
        pmmk = pmmk)

    analysis <- mMFClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

