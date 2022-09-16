#' Get molecular data
#'
#' Get expression data, including copy number amplification (CNA) and
#' normalized RNA-seq.
#'
#' @export
#' @note Updated 2022-09-16.
#'
#' @details
#' Examples of cancer studies with different mRNA data types:
#'
#' - RNA-seq v2: `gbm_tcga_pub2013_rna_seq_v2_mrna`.
#' - RNA-seq v1: `nbl_target_2018_pub_rna_seq_mrna`.
#' - Microarray: `gbm_tcga_pub_mrna`.
#'
#' @section cBioPortal z-score calculation method:
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
#' @inheritParams params
#'
#' @return `SummarizedExperiment`.
#' Samples (e.g. patient tumors) in the columns and genes in the rows.
#' Note that rownames return as Entrez gene identifiers.
#'
#' @examples
#' geneNames <- c("MYC", "TP53")
#'
#' ## Pan-cancer analysis of whole genomes ====
#' studyId <- "pancan_pcawg_2020"
#' mp <- molecularProfiles(studyId)
#' molecularProfileId <- rownames(mp)[[1L]]
#' x <- molecularData(
#'     studyId = studyId,
#'     molecularProfileId = molecularProfileId,
#'     geneNames = geneNames
#' )
#' print(x)
#'
#' ## CCLE Broad 2019 ====
#' studyId <- "ccle_broad_2019"
#' mp <- molecularProfiles(studyId)
#' molecularProfileId <- rownames(mp)[[1L]]
#' x <- molecularData(
#'     studyId = studyId,
#'     molecularProfileId = molecularProfileId,
#'     geneNames = geneNames
#' )
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
        assert(
            is.list(x),
            identical(names(x), molecularProfileId)
        )
        x <- x[[molecularProfileId]]
        assert(
            is.data.frame(x),
            isSubset(
                x = c("entrezGeneId", "sampleId", "value"),
                y = colnames(x)
            )
        )
        long <- x[, c("entrezGeneId", "sampleId", "value")]
        long <- as(long, "DataFrame")
        colnames(long) <- c("rowname", "colname", "value")
        wide <- cast(object = long, colnames = "colname", values = "value")
        assay <- as.matrix(wide)
        suppressMessages({
            rowData <- EntrezGeneInfo(organism = "Homo sapiens")
        })
        rowData <- as(rowData, "DataFrame")
        colData <- clinicalData(studyId)
        assert(
            isSubset(rownames(assay), rownames(rowData)),
            isSubset(colnames(assay), rownames(colData))
        )
        rowData <- rowData[rownames(assay), , drop = FALSE]
        colData <- colData[colnames(assay), , drop = FALSE]
        rownames(rowData) <- as.character(rowData[["geneName"]])
        rownames(assay) <- rownames(rowData)
        assays <- list(assay)
        names(assays) <- molecularProfileId
        makeSummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData,
            metadata = list(
                "studyId" = studyId,
                "molecularProfileId" = molecularProfileId,
                "geneNames" = geneNames
            ),
            denylist = FALSE
        )
    }
