#' Cancer studies available on cBioPortal
#'
#' Queries for datasets available on Cancer Genomics Data Server (CDGS).
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- cancerStudies()
#' print(x)
cancerStudies <- function() {
    x <- getCancerStudies(x = .cgds())
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- as(x, "DataFrame")
    x
}
