#' Get normalized RNA-seq expression data
#'
#' @export
#' @note Updated 2020-10-09.
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
#' https://github.com/cBioPortal/cbioportal/blob/master/docs/Z-Score-normalization-script.md
#'
#' @param cancerStudies `character`.
#'   Cancer study IDs.
#'   See [cancerStudies()] for details.
#' @param geneName `character`.
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
#' @return `DataFrame`.
#'
#' @examples
#' cancerStudies <- c("acc_tcga_pan_can_atlas_2018", "ccle_broad_2019")
#' geneNames <- c("MYC", "TP53")
#' df <- rnaSeqData(cancerStudies = cancerStudies, geneNames = geneNames)
#' print(df)
rnaSeqData <- function(
    cancerStudies,
    geneNames,
    zscore = c(
        "all samples",
        "diploid samples"
    )
) {
    assert(
        isCharacter(cancerStudies),
        isCharacter(geneNames)
    )
    zscore <- match.arg(zscore)
    ## RNA-seq:
    ## [1] "_rna_seq_v2_mrna_median_Zscores"
    ## [2] "_rna_seq_v2_mrna_median_all_sample_Zscores"
    ## Microarray:
    ## [1] "_mrna_median_all_sample_Zscores"
    zscorePattern <- switch(
        EXPR = zscore,
        "all samples" = "_rna_seq(_v2)?_mrna_median_all_sample_Zscores$",
        "diploid samples" = "_rna_seq(_v2)?_mrna_median_Zscores$"
    )
    list <- bplapply(
        X = cancerStudies,
        zscorePattern = zscorePattern,
        geneNames = geneNames,
        FUN = function(
            cancerStudy,
            zscorePattern,
            geneNames,
            cgds = .cgds()
        ) {
            caseList <- caseLists(cancerStudy = cancerStudy)
            caseListID <- grep(
                pattern = "_rna_seq(_v2)?_mrna$",
                x = caseList[["caseListID"]],
                value = TRUE
            )
            if (!isString(caseListID)) {
                cli_alert_warning(sprintf(
                    "No RNA-seq data: {.var %s}.",
                    cancerStudy
                ))
                return(NULL)
            }
            ## Get mRNA expression.
            prof <- geneticProfiles(cancerStudy = cancerStudy)
            keep <- grepl(
                pattern = zscorePattern,
                x = prof[["geneticProfileID"]]
            )
            if (!any(keep)) {
                cli_alert_warning(sprintf(
                    "Missing zscore: {.var %s} ({.var %s}).",
                    cancerStudy, zscorePattern
                ))
            }
            prof <- prof[keep, ]
            assert(
                nrow(prof) == 1L,
                identical(
                    x = prof[["geneticAlterationType"]],
                    y = "MRNA_EXPRESSION"
                ),
                identical(
                    x = prof[["showProfileInAnalysisTab"]],
                    y = "true"
                )
            )
            geneticProfileID <- prof[["geneticProfileID"]]
            cli_alert(sprintf(
                "Importing RNA-seq data: {.var %s}.",
                geneticProfileID
            ))
            df <- getProfileData(
                x = cgds,
                genes = geneNames,
                caseList = caseListID,
                geneticProfiles = geneticProfileID
            )
            assert(is.data.frame(df))
            df <- rownames_to_column(df, "sampleID")
            df[["cancerStudy"]] <- cancerStudy
            df
        }
    )
    df <- do.call(what = bind_rows, args = unname(list))
    df <- select(df, !!!syms(c("cancerStudy", "sampleID")), everything())
    df <- arrange_all(df)
    df <- as(df, "DataFrame")
    df
}
