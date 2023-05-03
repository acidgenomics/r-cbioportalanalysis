#' Construct a cBioPortal API object
#'
#' @note Updated 2023-05-03.
#' @noRd
#'
#' @return `cBioPortal`
.api <- function() {
    assert(requireNamespaces("cBioPortalData"))
    suppressWarnings({
        api <- cBioPortalData::cBioPortal(
            hostname = "www.cbioportal.org",
            protocol = "https",
            api. = "/api/api-docs",
            token = character()
        )
    })
    assert(is(api, "cBioPortal"))
    api
}
