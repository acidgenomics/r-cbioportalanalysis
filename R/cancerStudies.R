#' Cancer studies available on cBioPortal
#'
#' Queries for datasets available on Cancer Genomics Data Server (CDGS).
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- cancerStudies()
#' print(x)
cancerStudies <- function(.api = NULL) {
    if (is.null(.api)) {
        .api <- .api()
    }
    assert(is(.api, "cBioPortal"))
    x <- getStudies(api = .api)
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
    x <- as(x, "DataFrame")
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
