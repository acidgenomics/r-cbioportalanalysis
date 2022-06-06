#' Get available case lists for a specific cancer study
#'
#' @export
#' @note Updated 2022-06-06.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @seealso
#' - [cancerStudies()].
#'
#' @examples
#' x <- caseLists(cancerStudy = "ccle_broad_2019")
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
    x <- x[sort(rownames(x)), ]
    x
}
