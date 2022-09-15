#' Construct a cBioPortal API object
#'
#' @note Updated 2022-09-15.
#' @noRd
#'
#' @return `cBioPortal`
.api <- function() {
    cBioPortal(
        hostname = "www.cbioportal.org",
        protocol = "https",
        api. = "/api/api-docs",
        token = character()
    )
}
