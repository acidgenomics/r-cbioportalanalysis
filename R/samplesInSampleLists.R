#' Samples in sample lists
#'
#' @export
#' @note Updated 2023-05-03.
#'
#' @return `character`.
#' Sample identifiers.
#'
#' @examples
#' sampleListIds = c(
#'     "gbm_tcga_pub_expr_classical",
#'     "gbm_tcga_pub_expr_mesenchymal"
#' )
#' x <- samplesInSampleLists(sampleListIds = sampleListIds)
#' head(x)
samplesInSampleLists <-
    function(
        sampleListIds,
        .api = NULL
    ) {
        assert(requireNamespaces("cBioPortalData"))
        if (is.null(.api)) {
            .api <- .api()
        }
        assert(
            isCharacter(sampleListIds),
            is(.api, "cBioPortal")
        )
        x <- cBioPortalData::samplesInSampleLists(
            api = .api,
            sampleListIds = sampleListIds
        )
        assert(is(x, "CharacterList"))
        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        assert(hasNoDuplicates(x))
        x <- sort(x)
        x
    }
