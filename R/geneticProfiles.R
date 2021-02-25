#' Get available genetic data profiles for a specific cancer study
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' cancerStudy <- "ccle_broad_2019"
#' x <- geneticProfiles(cancerStudy = cancerStudy)
#' print(x)
geneticProfiles <- function(cancerStudy) {
    assert(isString(cancerStudy))
    x <- getGeneticProfiles(
        x = .cgds(),
        cancerStudy = cancerStudy
    )
    assert(is.data.frame(x))
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- as(x, "DataFrame")
    x
}
