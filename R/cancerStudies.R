#' Cancer studies available on cBioPortal
#'
#' Queries for datasets available on Cancer Genomics Data Server (CDGS).
#'
#' @export
#' @note Updated 2023-05-03.
#'
#' @return `DFrame`.
#'
#' @examples
#' x <- cancerStudies()
#' print(x)
cancerStudies <- function(.api = NULL) {
    assert(requireNamespaces("cBioPortalData"))
    if (is.null(.api)) {
        .api <- .api()
    }
    assert(is(.api, "cBioPortal"))
    x <- cBioPortalData::getStudies(api = .api, buildReport = TRUE)
    assert(
        is.data.frame(x),
        hasRows(x),
        hasColnames(x)
    )
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x[["citation"]] <- NULL
    assert(isSubset(
        x = c("description", "groups", "name", "pmid", "studyId"),
        y = colnames(x)
    ))
    x <- as(x, "DFrame")
    rownames(x) <- x[["studyId"]]
    x <- x[sort(rownames(x)), ]
    x[["groups"]] <- CharacterList(strsplit(
        x = x[["groups"]],
        split = ";",
        fixed = TRUE
    ))
    x[["pmid"]] <- IntegerList(strsplit(
        x = x[["pmid"]],
        split = ",",
        fixed = TRUE
    ))
    x
}
