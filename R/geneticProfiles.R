#' Get available genetic data profiles for a specific cancer study
#'
#' @export
#' @note Updated 2022-06-06.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- geneticProfiles(cancerStudy = "ccle_broad_2019")
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
    rownames(x) <- x[["geneticProfileId"]]
    x <- x[sort(rownames(x)), ]
    x
}
