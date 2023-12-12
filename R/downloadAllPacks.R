#' Download all cBioPortal study data packs
#'
#' @export
#' @note Updated 2023-12-12.
#'
#' @param dir `character(1)`.
#' Target directory.
#'
#' @return `character`.
#' File paths of study data pack tarballs.
downloadAllPacks <- function(dir = getwd()) {
    df <- cancerStudies()
    studies <- sort(df[["studyId"]])
    ## Updated 2023-05-09.
    ## https://www.cbioportal.org/datasets
    denylist <- c(
        "bowel_colitis_msk_2022",
        "makeanimpact_ccr_2023",
        "mtnn_msk_2022",
        "nbl_msk_2023",
        "prad_organoids_msk_2022",
        "rms_msk_2023",
        "sarcoma_msk_2023"
    )
    studies <- setdiff(studies, denylist)
    baseUrl <- "https://cbioportal-datahub.s3.amazonaws.com"
    files <- vapply(
        X = studies,
        FUN = function(study, baseUrl, dir) {
            bn <- paste0(study, ".tar.gz")
            url <- pasteUrl(baseUrl, bn)
            destfile <- file.path(dir, bn)
            if (isTRUE(file.exists(destfile))) {
                return(destfile)
            }
            ## FIXME Use AcidBase::download here instead.
            download.file(url = url, destfile = destfile)
            destfile
        },
        FUN.VALUE = character(1L),
        baseUrl = baseUrl,
        dir = dir
    )
    files
}
