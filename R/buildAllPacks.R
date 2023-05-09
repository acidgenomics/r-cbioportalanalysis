## FIXME Rework this to use `loadStudy` internally, rather than relying on
## cDataPack...
## This will help add processing support for all studies.



#' Build all supported data packs
#'
#' @export
#' @note Updated 2023-05-03
#'
#' @param dir `character(1)`.
#' Target directory.
#'
#' @return `character`.
#' Named character vector containing RDS file paths.
buildAllPacks <- function(dir = getwd()) {
    assert(
        requireNamespaces("cBioPortalData"),
        isADir(dir)
    )
    df <- cancerStudies()
    idx <- which(df[["packBuild"]] == TRUE)
    df <- df[idx, ]
    studyIds <- sort(df[["studyId"]][idx])
    badStudyIds <- "brca_tcga_pan_can_atlas_2018"
    studyIds <- setdiff(studyIds, badStudyIds)
    cacheDir <- tempdir2()
    cBioPortalData::setCache(
        directory = cacheDir,
        verbose = FALSE,
        ask = FALSE
    )
    files <- lapply(
        X = studyIds,
        FUN = function(studyId) {
            file <- file.path(dir, paste0(studyId, ".rds"))
            if (isAFile(file)) {
                return(file)
            }
            object <- tryCatch(
                expr = {
                    ## FIXME Consider suppressing messages here to make this
                    ## less busy.
                    cBioDataPack(
                        cancer_study_id = studyId,
                        use_cache = TRUE,
                        cleanup = TRUE,
                        ask = FALSE
                    )
                },
                error = function(e) {
                    ## FIXME Just add an alert message to inform user of
                    ## pack build failure.
                    message(e)
                    NULL
                }
            )
            if (is.null(object)) {
                return(NULL)
            }
            saveRDS(object = object, file = file)
            file
        }
    )
    files <- Filter(Negate(is.null), files)
    unlink2(cacheDir)
    invisible(files)
}
