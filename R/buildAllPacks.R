#' Build all supported data packs
#'
#' @export
#' @note Updated 2023-05-09.
#'
#' @param inputDir `character(1)`.
#' Input directory containing downloaded tarballs (`.tar.gz` files).
#'
#' @param outputDir `character(1)`.
#' Output directory where to save RDS files.
#'
#' @return `character`.
#' Named character vector containing RDS file paths.
buildAllPacks <- function(inputDir = getwd(), outputDir = getwd()) {
    assert(
        requireNamespaces("cBioPortalData"),
        isADir(inputDir)
    )
    tarballs <- sort(list.files(
        path = inputDir,
        pattern = "*.tar.gz",
        full.names = TRUE,
        recursive = FALSE
    ))
    assert(hasLength(tarballs))
    outputDir <- initDir(outputDir)
    df <- cancerStudies()
    studyIds <- sort(df[["studyId"]])
    assert(areIntersectingSets(x = basenameSansExt(tarballs), y = studyIds))
    cacheDir <- tempdir2()
    cBioPortalData::setCache(
        directory = cacheDir,
        verbose = FALSE,
        ask = FALSE
    )
    rdsFiles <- lapply(
        X = tarballs,
        FUN = function(tarball, cacheDir, outputDir) {
            studyId <- basenameSansExt(tarball)
            alert(studyId)
            rdsFile <- file.path(outputDir, paste0(studyId, ".rds"))
            if (isAFile(file)) {
                return(file)
            }
            object <- tryCatch(
                expr = {
                    filepath <- untarStudy(
                        cancer_study_file = tarball,
                        exdir = cacheDir
                    )
                    object <- loadStudy(
                        filepath = filepath,
                        names.field = c(
                            "Hugo_Symbol",
                            "Entrez_Gene_Id",
                            "Gene"
                        ),
                        cleanup = TRUE
                    )
                    object
                },
                error = function(e) {
                    message(e)
                    NULL
                }
            )
            if (is.null(object)) {
                return(NULL)
            }
            saveRDS(object = object, file = rdsFile)
            rdsFile
        },
        cacheDir = cacheDir,
        outputDir = outputDir
    )
    rdsFiles <- Filter(Negate(is.null), rdsFiles)
    unlink2(cacheDir)
    invisible(rdsFiles)
}
