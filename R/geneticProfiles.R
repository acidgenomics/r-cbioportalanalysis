#' Get available genetic data profiles for a specific cancer study
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
#' x <- geneticProfiles(cancerStudy = cancerStudy)
geneticProfiles <- function(cancerStudy) {
    assert(isString(cancerStudy))
    x <- getGeneticProfiles(
        x = .cgds(),
        cancerStudy = cancerStudy
    )
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x))
    x <- as(x, "DataFrame")
    x
}
