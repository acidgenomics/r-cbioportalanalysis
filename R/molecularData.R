## FIXME What if we want to pull all TCGA 2018 PanCancer studies here?



#' Get molecular (i.e. expression) data
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @details
#' Examples of cancer studies with different mRNA data types:
#'
#' - RNA-seq v2: `gbm_tcga_pub2013_rna_seq_v2_mrna`.
#' - RNA-seq v1: `nbl_target_2018_pub_rna_seq_mrna`.
#' - Microarray: `gbm_tcga_pub_mrna`.
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
#' Z-Score-normalization-script.md
#'
#' @param cancerStudy `character(1)`.
#' Cancer study identifier (e.g. ""acc_tcga_pan_can_atlas_2018").
#' See `cancerStudies()` for details.
#'
#' @param geneNames `character`.
#' HUGO gene symbols (e.g. "TP53").
#'
#' @param zscore `character`.
#' - `"all samples"`:
#' mRNA expression z-Scores relative to all samples (log RNA Seq RPKM).
#' Log-transformed mRNA z-Scores compared to the expression distribution of
#' all samples (RNA Seq RPKM).
#' - `"diploid samples"`:
#' mRNA expression z-Scores relative to diploid samples (RNA Seq RPKM).
#' mRNA z-Scores (RNA Seq RPKM) compared to the expression distribution of
#' each gene tumors that are diploid for this gene.
#'
#' @return `SummarizedExperiment`.
#' Samples (e.g. patient tumors) in the columns and genes in the rows.
#'
#' @examples
#' geneNames <- c("MYC", "TP53")
#'
#' ## pancan_pcawg_2020 RNA-seq ====
#' studyId <- "pancan_pcawg_2020"
#' mp <- molecularProfiles(studyId = studyId)
#' molecularProfileId <- mp[["molecularProfileId"]][[1L]]
#' x <- molecularData(
#'     studyId = studyId,
#'     molecularProfileId = molecularProfileId,
#'     geneNames = geneNames
#' )
#' print(x)
#'
#' ## CCLE Broad 2019 ====
#' cancerStudy <- "ccle_broad_2019"
#' ## FIXME Get the molecular profile ID example here.
#' x <- molecularData(cancerStudy = cancerStudy, geneNames = geneNames)
#' print(x)
molecularData <-
    function(studyId,
             molecularProfileId,
             geneNames,
             .api = NULL) {
        if (is.null(.api)) {
            .api <- .api()
        }
        assert(
            isString(studyId),
            isString(molecularProfileId),
            isCharacter(geneNames),
            hasNoDuplicates(geneNames),
            is(.api, "cBioPortal")
        )
        map <- cBioPortalData::queryGeneTable(
            api = .api,
            by = "hugoGeneSymbol",
            genes = geneNames
        )
        assert(
            is.data.frame(map),
            isSubset(
                x = c("entrezGeneId", "hugoGeneSymbol"),
                y = colnames(map)
            ),
            identical(geneNames, map[["hugoGeneSymbol"]])
        )
        entrezGeneIds <- map[["entrezGeneId"]]
        si <- sampleInfo(studyId = studyId, .api = .api)
        sampleIds <- si[["sampleId"]]
        assert(isCharacter(sampleIds))
        x <- cBioPortalData::molecularData(
            api = .api,
            molecularProfileIds = molecularProfileId,
            entrezGeneIds = entrezGeneIds,
            sampleIds = sampleIds
        )
        assert(is.list(x))
        x <- x[[1L]]
        assert(
            is.data.frame(x),
            isSubset(
                x = c(
                    "entrezGeneId",
                    "sampleId",
                    "value"
                ),
                y = colnames(x)
            )
        )
        x <- x[
            ,
            c(
                "entrezGeneId",
                "sampleId",
                "value"
            )
        ]
        ## FIXME We need to write a base R version for AcidPlyr that works on
        ## S4 DataFrame class...the opposite of our `melt` function.
        ## This is called "cast" in the reshape2 package.
        ## https://stackoverflow.com/questions/7827815/
        x <- tidyr::pivot_wider(
            data = x,
            names_from = entrezGeneId,
            values_from = value,
            values_fill = NULL
        )
        x <- as.data.frame(x)
        assert(hasNoDuplicates(x[["sampleId"]]))
        rownames(x) <- x[["sampleId"]]
        x[["sampleId"]] <- NULL
        x <- as.matrix(x)
        ## Put the samples in columns, features in rows.
        x <- t(x)


        ## FIXME Draft update, come back to this...


        ## FIXME Need to get clinicalInfo that we can use as column data.

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
