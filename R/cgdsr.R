#' Construct a CGDS connection object
#'
#' @note Updated 2020-10-08.
#' @noRd
#'
#' @return `CGDS`.
.cgds <- function() {
    x <- CGDS(
        url = "http://www.cbioportal.org/",
        verbose = FALSE
    )
    assert(is(x, "CGDS"))
    x
}



#' Get cancer studies
#'
#' @note Updated 2020-10-08.
#' @noRd
.getCancerStudies <- function() {
    cgds <- .cgds()
    x <- getCancerStudies(cgds)
    assert(is.data.frame(x))
    x
}
