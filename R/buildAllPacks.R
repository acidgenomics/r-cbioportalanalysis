## FIXME Rework this to use `loadStudy` internally, rather than relying on
## cDataPack...
## This will help add processing support for all studies.



#' Build all supported data packs
#'
#' @export
#' @note Updated 2023-05-09.
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
    df <- df[idx, , drop = FALSE]
    studyIds <- sort(df[["studyId"]][idx])
    denylist <- c(
        "brca_tcga_pan_can_atlas_2018",
        "coadread_tcga_pan_can_atlas_2018",
        "ov_tcga_pan_can_atlas_2018",
        "sarc_tcga_pan_can_atlas_2018"
    )
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
                    cBioDataPack(
                        cancer_study_id = studyId,
                        use_cache = TRUE,
                        cleanup = TRUE,
                        ask = FALSE
                    )
                },
                error = function(e) {
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
