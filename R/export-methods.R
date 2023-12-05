#' @name export
#' @inherit AcidGenerics::export
#' @note Updated 2023-09-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param bindRowData `logical(1)`.
#' Whether to column bind row data (e.g. gene annotations), slotted in
#' `rowData`, to each exported assay matrix, defined in `assays`.
#'
#' @param compress `logical(1)`.
#' Apply gzip compression to all files.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' con <- AcidBase::tempdir2()
#' x <- export(object = object, con = con)
#' print(x)
#' AcidBase::unlink2(con)
NULL



#' Export MultiAssayExperiment experiments
#'
#' @note Updated 2022-10-24.
#' @noRd
.exportExperiments <-
    function(object,
             con,
             bindRowData,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "MultiAssayExperiment"),
            isADirectory(con),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        exp <- experiments(object)
        assert(
            is(exp, "ExperimentList"),
            hasNames(exp)
        )
        ## Drop complex objects, such as RaggedExperiment.
        keep <- bapply(X = exp, FUN = is, class2 = "SummarizedExperiment")
        exp <- exp[keep]
        if (!hasLength(exp)) {
            return(NULL)
        }
        assert(hasNoDuplicates(names(exp)))
        ## Ensure nested objects do not contain duplicated dimnames. This has
        ## been observed with cBioPortalData MAE objects.
        exp <- lapply(
            X = exp,
            FUN = function(object) {
                assert(hasNoDuplicates(colnames(object)))
                if (hasDuplicates(rownames(object))) {
                    alertWarning("Duplicate rownames detected.")
                    rownames(object) <- NULL
                }
                object
            }
        )
        Map(
            f = export,
            object = exp,
            con = file.path(con, names(exp)),
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' Export MultiAssayExperiment
#'
#' @note Updated 2023-10-05.
#' @noRd
`export,MAE` <- # nolint
    function(object,
             con,
             bindRowData = FALSE,
             compress = FALSE,
             overwrite = TRUE,
             quiet = FALSE) {
        assert(
            validObject(object),
            ## Some cBioPortalData objects currently fail these checks.
            ## > hasNoDuplicates(rownames(object)),
            ## > hasNoDuplicates(colnames(object)),
            isString(con),
            isFlag(bindRowData),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        con <- initDir(con)
        if (!isTRUE(quiet)) {
            alert(sprintf(
                fmt = "Exporting {.cls %s} to {.path %s}.",
                "MultiAssayExperiment", con
            ))
        }
        files <- list()
        files[["experiments"]] <-
            .exportExperiments(
                object = object,
                con = initDir(file.path(con, "experiments")),
                bindRowData = bindRowData,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ext <- "csv"
        if (isTRUE(compress)) {
            ext <- paste0(ext, ".gz")
        }
        ext <- paste0(".", ext)
        colData <- colData(object)
        if (hasDuplicates(rownames(colData))) {
            alertWarning(sprintf(
                "Duplicate column identifiers detected in {.var %s}.",
                "colData"
            ))
            rownames(colData) <- NULL
        }
        files[["colData"]] <-
            .exportDF(
                object = colData,
                con = file.path(con, paste0("colData", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        sampleMap <- sampleMap(object)
        assert(
            is(sampleMap, "DFrame"),
            hasNoDuplicates(rownames(sampleMap)),
            hasNoDuplicates(colnames(sampleMap))
        )
        files[["sampleMap"]] <-
            .exportDF(
                object = sampleMap,
                con = file.path(con, paste0("sampleMap", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        files <- Filter(Negate(is.null), files)
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "MultiAssayExperiment",
        con = "character"
    ),
    definition = `export,MAE`
)
