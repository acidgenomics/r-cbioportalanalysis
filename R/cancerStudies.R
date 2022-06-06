#' Cancer studies available on cBioPortal
#'
#' Queries for datasets available on Cancer Genomics Data Server (CDGS).
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- cancerStudies()
#' print(x)
cancerStudies <- function() {
    x <- getCancerStudies(x = .cgds())
    assert(
        is.data.frame(x),
        hasRows(x),
        hasColnames(x)
    )
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    assert(identical(
        x = colnames(x),
        y = c("cancerStudyId", "name", "description")
    ))
    x <- as(x, "DataFrame")
    rownames(x) <- x[["cancerStudyId"]]
    x
}
