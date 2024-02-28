#' @name predictSensitivity
#' @inherit AcidGenerics::predictSensitivity
#' @note Updated 2024-02-28.
#'
#' @details
#' Predict cell line sensitivity by calculation of Euclidean distance between
#' known biomarker genes up- or down-regulated by a treatment of interest.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param experiment `character(1)`.
#' Experiment name corresponding to RNA-seq zscores.
#' We recommend using `"mrna_seq_rpkm_zscores_ref_all_samples"` for TCGA
#' PanCancer datasets.
#'
#' @param upregulated `character`.
#' Genes observed to be upregulated by treatment.
#'
#' @param downregulated `character`.
#' Genes observed to be downregulated by treatment.
#'
#' @param sensitiveCutoff `numeric(1)`.
#' Euclidean distance (ds/dr) ratio cutoff to define as sensitive.
#'
#' @return `DFrame`.
#'
#' @examples
#' ## FIXME This needs a working example.
#'
#' ## MultiAssayExperiment ====
#' ## > object <- rnaseq
#' ## > genes <- rownames(object)
#' ## > upregulated <- genes[1L:5L]
#' ## > downregulated <- genes[6L:10L]
#' ## > df <- predictSensitivity(
#' ## >     object = object,
#' ## >     experiment = "mrna_seq_v2_rsem_zscores_ref_all_samples"
#' ## >     upregulated = upregulated,
#' ## >     downregulated = downregulated
#' ## > )
#' ## > print(df)
NULL



## Updated 2023-12-11.
`predictSensitivity,MAE` <- # nolint
    function(object,
             experiment,
             upregulated,
             downregulated,
             sensitiveCutoff = 1L) {
        assert(
            validObject(object),
            isString(experiment),
            isMatchingFixed(x = experiment, pattern = "_zscores_ref_"),
            isSubset(experiment, names(experiments(object))),
            isCharacter(upregulated),
            isCharacter(downregulated),
            hasNoDuplicates(upregulated),
            hasNoDuplicates(downregulated),
            areDisjointSets(x = upregulated, y = downregulated),
            isScalarNumeric(sensitiveCutoff)
        )
        cd <- colData(object)
        ## Currently uses patient ID instead of sample ID by default, which
        ## is not the behavior we want.
        rownames(cd) <- cd[["SAMPLE_ID"]]
        se <- experiments(object)[[experiment]]
        assert(
            is(se, "SummarizedExperiment"),
            isSubset(colnames(se), rownames(cd))
        )
        colData(se) <- cd[colnames(se), , drop = FALSE]
        colnames(colData(se)) <- camelCase(colnames(colData(se)))
        colnames(rowData(se)) <- camelCase(colnames(rowData(se)))
        colnames(rowData(se))[
            colnames(rowData(se)) == "hugoSymbol"] <- "geneName"
        colnames(rowData(se))[
            colnames(rowData(se)) == "entrezGeneId"] <- "geneId"
        if (all(is.na(rowData(se)[["geneId"]]))) {
            rowData(se)[["geneId"]] <- rowData(se)[["geneName"]]
        }
        ## FIXME The TCGA dataset has NA gene names, need to resolve.
        ## e.g. "sclc_ucologne_2015" dataset.
        if (hasDuplicates(rowData(se)[["geneName"]])) {
            dupes <- dupes(rowData(se)[["geneName"]])
            alertWarning(sprintf(
                "Dropping %d duplicate %s from analysis: %s.",
                length(dupes),
                ngettext(
                    n = length(dupes),
                    msg1 = "gene",
                    msg2 = "genes"
                ),
                toInlineString(dupes)
            ))
            i <- !rowData(se)[["geneName"]] %in% dupes
            se <- se[i, , drop = FALSE]
        }
        ## Our `mapGenesToRownames` function currently requires "geneName"
        ## and "geneId" columns to be defined in `rowData`. Consider relaxing
        ## this requirement in a future AcidExperiment update.
        up <- mapGenesToRownames(
            object = se,
            genes = upregulated,
            strict = TRUE
        )
        down <- mapGenesToRownames(
            object = se,
            genes = downregulated,
            strict = TRUE
        )
        se <- se[c(up, down), , drop = FALSE]
        mat <- assay(se)
        assert(
            isNegative(min(mat)),
            isPositive(max(mat)),
            msg = sprintf(
                paste(
                    "Experiment defined in {.var %s} doesn't appear to",
                    "contain z-score normalized values."
                ),
                experiment
            )
        )
        ## Determine the maximum and minimum values per gene across the cells.
        geneMax <- rowMaxs(mat)
        geneMin <- rowMins(mat)
        ## Calculate SI and RI per gene for up- and down-regulated genes.
        siUp <- geneMax[names(geneMax) %in% up]
        riUp <- geneMin[names(geneMin) %in% up]
        siDown <- geneMin[names(geneMin) %in% down]
        riDown <- geneMax[names(geneMax) %in% down]
        si <- c(siUp, siDown)
        ri <- c(riUp, riDown)
        ## Create SI and RI matrix to get max.
        siMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = si,
                simplify = FALSE
            )
        )
        riMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = ri,
                simplify = FALSE
            )
        )
        colnames(siMat) <- colnames(mat)
        colnames(riMat) <- colnames(mat)
        ## Calculate the summation of euclidean distances.
        sigmaS <- colSums2((mat - siMat) ** 2L)
        sigmaR <- colSums2((mat - riMat) ** 2L)
        ## Euclidean distance of sensitivity model.
        dsj <- sqrt(sigmaS)
        ## Euclidean distance of insensitivity model.
        drj <- sqrt(sigmaR)
        ratio <- dsj / drj
        ## Indicate whether we predict sensitive.
        pred <- ifelse(
            test = ratio < sensitiveCutoff,
            yes = "sensitive",
            no = "insensitive"
        )
        ## FIXME Bind useful colData here.
        out <- DataFrame(
            "ds" = dsj,
            "dr" = drj,
            "ratio" = ratio,
            "prediction" = pred,
            row.names = colnames(se)
        )
        ## Ensure we stash the user input in the output.
        metadata(out) <- list(
            "experiment" = experiment,
            "upregulated" = upregulated,
            "downregulated" = downregulated,
            "sensitiveCutoff" = sensitiveCutoff
        )
        out
    }



#' @rdname predictSensitivity
#' @export
setMethod(
    f = "predictSensitivity",
    signature = signature(object = "MultiAssayExperiment"),
    definition = `predictSensitivity,MAE`
)
