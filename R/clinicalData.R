#' Get available clinical data for samples in a case list
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @inheritParams params
#'
#' @seealso
#' - `caseLists()`.
#' - `cancerStudies()`.
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- clinicalData(studyId = "pancan_pcawg_2020")
#' print(x)
clinicalData <- function(studyId, .api = NULL) {
    if (is.null(.api)) {
        .api <- .api()
    }
    assert(
        isString(studyId),
        is(.api, "cBioPortal")
    )
    x <- cBioPortalData::clinicalData(
        api = .api,
        studyId = studyId
    )
    assert(
        is.data.frame(x),
        hasNoDuplicates(x[["sampleId"]])
    )
    x <- as(x, "DataFrame")
    rownames(x) <- x[["sampleId"]]
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- x[sort(rownames(x)), sort(colnames(x))]
    x
}
