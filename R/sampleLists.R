## FIXME "pancan_pcawg_2020" has sequencing data on the website but this
## isn't returning here. Why not?



#' Get available case lists for a specific cancer study
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @seealso
#' - [cancerStudies()].
#'
#' @examples
#' x <- sampleLists(studyId = "pancan_pcawg_2020")
#' print(x)
sampleLists <- function(studyId, .api = NULL) {
    if (is.null(.api)) {
        .api <- .api()
    }
    assert(
        is(.api, "cBioPortal"),
        isString(studyId)
    )
    x <- cBioPortalData::sampleLists(
        api = .api,
        studyId = studyId
    )
    assert(
        is.data.frame(x),
        hasRows(x),
        hasColnames(x)
    )
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    assert(identical(
        x = colnames(x),
        y = c(
            "category",
            "name",
            "description",
            "sampleListId",
            "studyId"
        )
    ))
    x <- as(x, "DataFrame")
    rownames(x) <- x[["sampleListId"]]
    x <- x[sort(rownames(x)), ]
    x
}
