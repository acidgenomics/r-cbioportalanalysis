#' Build all supported data packs
#'
#' @export
#' @note Updated 2023-05-03
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
    cacheDir <- AcidBase::tempdir2()
    cBioPortalData::setCache(
        directory = cacheDir,
        verbose = FALSE,
        ask = FALSE
    )
    for (studyId in studyIds) {
        file <- file.path(dir, paste0(studyId, ".rds"))
        if (isAFile(file)) {
            continue()
        }
        object <- cBioDataPack(
            cancer_study_id = studyId,
            use_cache = TRUE,
            cleanup = TRUE,
            ask = FALSE
        )
        saveRDS(object = object, file = file)
    }
    AcidBase::unlink2(cacheDir)
    invisible(studies)
}
