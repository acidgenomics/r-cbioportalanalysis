#' Get available case lists for a specific cancer study
#'
#' @export
#' @note Updated 2020-10-09.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' cancerStudy <- "ccle_broad_2019"
#' x <- caseLists(cancerStudy = cancerStudy)
caseLists <- function(cancerStudy) {
    assert(isString(cancerStudy))
    x <- getCaseLists(
        x = .cgds(),
        cancerStudy = cancerStudy
    )
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x))
    x <- as(x, "DataFrame")
    x
}
