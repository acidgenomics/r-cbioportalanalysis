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
#' x <- sampleLists(studyId = "ccle_broad_2019")
#' print(x)
sampleLists <- function(studyId) {
    assert(isString(studyId))
    api <- .api()
    x <- cBioPortalData::sampleLists(
        api = api,
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
