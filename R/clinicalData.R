#' Get available clinical data for samples in a case list
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @seealso
#' - [caseLists()].
#' - [cancerStudies()].
#'
#' @return `DataFrame`.
#'
#' @examples
#' ## DLBCL TCGA ====
#' x <- clinicalData(caseList = "dlbc_tcga_pan_can_atlas_2018_rna_seq_v2_mrna")
#' print(x)
#'
#' ## CCLE Broad 2019 ====
#' x <- clinicalData(caseList = "ccle_broad_2019_sequenced")
#' print(x)
clinicalData <- function(caseList) {
    assert(isString(caseList))
    x <- tryCatch(
        expr = {
            getClinicalData(x = .cgds(), caseList = caseList)
        },
        error = function(e) {
            ## This currently happens for "ccle_broad_2019_all", for example.
            stop(sprintf("cgdsr query failure for '%s'.", caseList))
        }
    )
    assert(is.data.frame(x))
    assert(hasNoDuplicates(rownames(x)))
    rownames(x) <- makeNames(rownames(x))
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- sanitizeNA(x)
    x <- as(x, "DataFrame")
    x
}
