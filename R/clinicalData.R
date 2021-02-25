## FIXME This is erroring for "ccle_broad_2019_all"
## Warning: non-unique values when setting 'row.names':
## 'X1321N1_CENTRAL_NERVOUS_SYSTEM',
## 'X143B_BONE',
## 'X22RV1_PROSTATE',
## 'X2313287_STOMACH',
## 'X42MGBA_CENTRAL_NERVOUS_SYSTEM',
## 'X5637_URINARY_TRACT',
## 'X59M_OVARY',
## 'X639V_URINARY_TRACT',
## 'X647V_URINARY_TRACT',
## 'X697_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE',
## 'X769P_KIDNEY',
## 'X786O_KIDNEY',
## 'X8305C_THYROID',
## 'X8505C_THYROID',
## 'X8MGBA_CENTRAL_NERVOUS_SYSTEM'



#' Get available clinical data for samples in a case list
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' cancerStudy <- "dlbc_tcga_pan_can_atlas_2018"
#' caseList <- "dlbc_tcga_pan_can_atlas_2018_rna_seq_v2_mrna"
#' x <- clinicalData(cancerStudy = cancerStudy, caseList = caseListID)
#' print(x)
clinicalData <- function(
    cancerStudy,
    caseList
) {
    assert(
        isString(cancerStudy),
        isString(caseList)
    )
    cl <- caseLists(cancerStudy = cancerStudy)
    assert(isSubset(caseList, cl[["caseListId"]]))
    idx <- match(x = caseList, table = cl[["caseListId"]])
    assert(isInt(idx))
    cases <- sort(unique(strsplit(
        x = cl[idx, "caseIds", drop = TRUE],
        split = " ",
        fixed = TRUE
    )[[1L]]))

    x <- tryCatch(
        expr = {
            ## NOTE "cases" input doesn't seem to work here.
            ## Refer to source code for details:
            ## https://github.com/cBioPortal/cgdsr/blob/master/R/cgdsr.R#L102
            getClinicalData(
                x = .cgds(),
                caseList = caseList
                ## > cases = cases
            )
        },
        error = function(e) {
            ## This currently happens for "ccle_broad_2019", for example.
            stop("cgdsr query failure.")
        }
    )
    assert(is.data.frame(x))
    rownames(x) <- snakeCase(tolower(rownames(x)))
    colnames(x) <- camelCase(colnames(x), strict = TRUE)
    x <- sanitizeNA(x)
    x <- as(x, "DataFrame")
    x
}
