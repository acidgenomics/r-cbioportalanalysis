#' Cancer studies available on cBioPortal
#'
#' Queries for datasets available on Cancer Genomics Data Server (CDGS).
#'
#' @note Updated 2020-10-09.
#' @export
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- cancerStudies()
#' colnames(x)
#' head(x[["cancerStudyID"]])
cancerStudies <- function() {
    x <- getCancerStudies(x = .cgds())
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x))
    x <- as(x, "DataFrame")
    x
}
