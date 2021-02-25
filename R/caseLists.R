#' Get available case lists for a specific cancer study
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @seealso
#' - [cancerStudies()].
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
    assert(
        is.data.frame(x),
        hasRows(x),
        hasColnames(x)
    )
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    assert(identical(
        x = colnames(x),
        y = c(
            "caseListId",
            "caseListName",
            "caseListDescription",
            "cancerStudyId",
            "caseIds"
        )
    ))
    x <- as(x, "DataFrame")
    cases <- CharacterList(strsplit(
        x = x[["caseIds"]],
        split = " ",
        fixed = TRUE
    ))
    cases <- sort(unique(cases))
    x[["caseIds"]] <- cases
    rownames(x) <- x[["caseListId"]]
    x
}
