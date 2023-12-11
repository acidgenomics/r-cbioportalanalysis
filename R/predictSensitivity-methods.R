## FIXME Need to get the RNA-seq experiment here.



#' @name predictSensitivity
#' @inherit AcidGenerics::predictSensitivity
#' @note Updated 2023-12-11.
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
#' ## >     upregulated = upregulated,
#' ## >     downregulated = downregulated
#' ## > )
#' ## > print(df)
NULL



## FIXME Assert that user has input zscore experiment.

## Updated 2023-12-11.
`predictSensitivity,MAE` <- # nolint
    function(object,
             experiment = "mrna_seq_v2_rsem_zscores_ref_all_samples",
             upregulated,
             downregulated) {
        assert(
            validObject(object),
            isString(experiment),
            isMatchingFixed(x = experiment, pattern = "_zscores_ref_"),
            isSubset(experiment, names(experiments(object))),
            isCharacter(upregulated),
            isCharacter(downregulated),
            hasNoDuplicates(upregulated),
            hasNoDuplicates(downregulated),
            areDisjointSets(x = upregulated, y = downregulated)
        )
        cd <- colData(object)
        se <- experiments(object)[[experiment]]
        assert(isSubset(colnames(se), rownames(cd)))
        colData(se) <- cd[colnames(se), , drop = FALSE]




        assert(is(se, "SummarizedExperiment"))
        ## FIXME This doesn't work if geneId, geneName not defined.
        up <- mapGenesToRownames(
            object = se,
            genes = upregulated,
            strict = TRUE
        )
        ## FIXME This doesn't work if geneId, geneName not defined.
        down <- mapGenesToRownames(
            object = se,
            genes = downregulated,
            strict = TRUE
        )
        object <- object[c(up, down), , drop = FALSE]
        ## FIXME Don't calculate zscore here -- ensure we use pre-calculated
        ## zscore data.
        mat <- zscore(object)
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
            test = dsj <= drj,
            yes = "sensitive",
            no = "insensitive"
        )
        out <- DataFrame(
            "drj" = drj,
            "dsj" = dsj,
            "ratio" = ratio,
            "prediction" = pred,
            row.names = colnames(object)
        )
        ## Return with additional metadata useful for biologists.
        cd <- .simpleColData(object)
        out <- cbind(out, cd)
        ## Ensure we stash the user input in the output.
        metadata(out) <- list(
            "upregulated" = upregulated,
            "downregulated" = downregulated,
            "releaseDate" = metadata(object)[["releaseDate"]]
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
