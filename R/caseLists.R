#' Get available case lists for a specific cancer study
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @seealso [cancerStudies()].
#'
#' @examples
#' cancerStudy <- "ccle_broad_2019"
#' x <- caseLists(cancerStudy = cancerStudy)
#' print(x)
caseLists <- function(cancerStudy) {
    assert(isString(cancerStudy))
    x <- getCaseLists(
        x = .cgds(),
        cancerStudy = cancerStudy
    )
    ## FIXME NEED TO CHECK FOR MATCH FAILURE...
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- as(x, "DataFrame")
    x
}
