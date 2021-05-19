#' Get normalized RNA-seq expression data
#'
#' @export
#' @note Updated 2021-02-26.
#'
#' @details
#' Examples of cancer studies with different mRNA data types:
#'
#' - RNA-seq v2: `gbm_tcga_pub2013_rna_seq_v2_mrna`.
#' - RNA-seq v1: `nbl_target_2018_pub_rna_seq_mrna`.
#' - Microarray: `gbm_tcga_pub_mrna`.
#'
#'
#' @section The cBioPortal Z-Score calculation method:
#'
#' cBioPortal currently generates two z-score profiles using two different base
#' populations:
#'
#' Distribution based on **diploid** samples only: The expression distribution
#' for unaltered copies of the gene is estimated by calculating the mean and
#' variance of the expression values for samples in which the gene is diploid
#' (i.e. value is "0" as reported by discrete CNA data). We call this the
#' unaltered distribution. If the gene has no diploid samples, then its
#' normalized expression is reported as `NA`.
#'
#' Distribution based on **all** samples: The expression distribution of the
#' gene is estimated by calculating the mean and variance of all samples with
#' expression values (excludes zero's and non-numeric values like `NA`, `NULL`
#' or `NaN`). If the gene has samples whose expression values are all zeros or
#' non-numeric, then its normalized expression is reported as `NA`.
#'
#' Otherwise for every sample, the gene's normalized expression for both the
#' profiles is reported as:
#'
#' ```
#' (r - mu)/sigma
#' ```
#'
#' where `r` is the raw expression value, and `mu` and `sigma` are the mean and
#' standard deviation of the base population, respectively.
#'
#' See also:
#' - https://github.com/cBioPortal/cbioportal/blob/master/docs/
#'       Z-Score-normalization-script.md
#'
#' @param cancerStudy `character(1)`.
#'   Cancer study identifier (e.g. ""acc_tcga_pan_can_atlas_2018").
#'   See [cancerStudies()] for details.
#' @param geneNames `character`.
#'   HUGO gene symbols (e.g. "TP53").
#' @param zscore `character`.
#'   - `"all samples"`:
#'     mRNA expression z-Scores relative to all samples (log RNA Seq RPKM).
#'     Log-transformed mRNA z-Scores compared to the expression distribution of
#'     all samples (RNA Seq RPKM).
#'   - `"diploid samples"`:
#'     mRNA expression z-Scores relative to diploid samples (RNA Seq RPKM).
#'     mRNA z-Scores (RNA Seq RPKM) compared to the expression distribution of
#'     each gene tumors that are diploid for this gene.
#'
#' @return `SummarizedExperiment`.
#'   Samples (e.g. patient tumors) in the columns and genes in the rows.
#'
#' @examples
#' geneNames <- c("MYC", "TP53")
#'
#' ## ACC TCGA 2018 ====
#' cancerStudy <- "acc_tcga_pan_can_atlas_2018"
#' x <- rnaSeqData(cancerStudy = cancerStudy, geneNames = geneNames)
#' print(x)
#'
#' ## CCLE Broad 2019 ====
#' cancerStudy <- "ccle_broad_2019"
#' x <- rnaSeqData(cancerStudy = cancerStudy, geneNames = geneNames)
#' print(x)
rnaSeqData <- function(
    cancerStudy,
    geneNames,
    zscore = c("all samples", "diploid samples")
) {
    assert(
        isString(cancerStudy),
        isCharacter(geneNames),
        hasNoDuplicates(geneNames)
    )
    ## RNA-seq:
    ## [1] "_rna_seq_v2_mrna_median_Zscores"
    ## [2] "_rna_seq_v2_mrna_median_all_sample_Zscores"
    ## Microarray:
    ## [1] "_mrna_median_all_sample_Zscores"
    zscore <- match.arg(zscore)
    zscorePattern <- switch(
        EXPR = zscore,
        "all samples" = "_rna_seq(_v2)?_mrna_median_all_sample_Zscores$",
        "diploid samples" = "_rna_seq(_v2)?_mrna_median_Zscores$"
    )
    cgds <- .cgds()
    ## Get the case list identifier.
    df <- caseLists(cancerStudy = cancerStudy)
    ## e.g. "acc_tcga_pan_can_atlas_2018_rna_seq_v2_mrna".
    caseList <- grep(
        pattern = "_rna_seq(_v2)?_mrna$",
        x = df[["caseListId"]],
        value = TRUE
    )
    ## Fall back for CCLE (e.g. "ccle_broad_2019_sequenced").
    if (!isString(caseList)) {
        caseList <- grep(
            pattern = "_sequenced$",
            x = df[["caseListId"]],
            value = TRUE
        )
    }
    if (!isString(caseList)) {
        stop(sprintf(
            "No RNA-seq data: {.var %s}.",
            cancerStudy
        ))
    }
    ## Get mRNA expression.
    df <- geneticProfiles(cancerStudy = cancerStudy)
    keep <- grepl(
        pattern = zscorePattern,
        x = df[["geneticProfileId"]]
    )
    if (!any(keep)) {
        stop(sprintf(
            "Missing zscore: {.var %s} ({.var %s}).",
            cancerStudy, zscorePattern
        ))
    }
    df <- df[keep, , drop = FALSE]
    assert(
        nrow(df) == 1L,
        identical(
            x = df[["geneticAlterationType"]],
            y = "MRNA_EXPRESSION"
        ),
        identical(
            x = df[["showProfileInAnalysisTab"]],
            y = "true"
        )
    )
    geneticProfile <- df[["geneticProfileId"]]
    alert(sprintf(
        "Importing RNA-seq data: {.var %s}.",
        geneticProfile
    ))
    df <- getProfileData(
        x = cgds,
        genes = geneNames,
        caseList = caseList,
        geneticProfiles = geneticProfile
    )
    assert(
        is.data.frame(df),
        identical(colnames(df), geneNames),
        hasRownames(df)
    )
    counts <- makeDimnames(t(as.matrix(df)))
    ## Get the clinical metadata corresponding to the column values
    ## (e.g. patient and/or cell line info).
    colData <- clinicalData(caseList = caseList)
    assert(
        is(colData, "DataFrame"),
        isSubset(rownames(colData), colnames(counts))
    )
    colData <- colData[colnames(counts), , drop = FALSE]
    makeSummarizedExperiment(
        assays = list("counts" = counts),
        colData = colData,
        metadata = list(
            "caseLists" = caseLists,
            "geneticProfiles" = geneticProfiles
        ),
        denylist = FALSE
    )
}
