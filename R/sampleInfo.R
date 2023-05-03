#' Sample information
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @inheritParams params
#'
#' @examples
#' x <- sampleInfo(studyId = "pancan_pcawg_2020")
#' print(x)
sampleInfo <-
    function(studyId, .api = NULL) {
        if (is.null(.api)) {
            .api <- .api()
        }
        assert(
            isString(studyId),
            is(.api, "cBioPortal")
        )
        x <- cBioPortalData::getSampleInfo(
            api = .api,
            studyId = studyId,
            ## Supported: "SUMMARY", "ID", "DETAILED", "META".
            ## Usage of "META" here is currently buggy.
            projection = "DETAILED"
        )
        x <- as(x, "DFrame")
        colnames(x) <- camelCase(colnames(x), strict = TRUE)
        assert(hasNoDuplicates(x[["sampleId"]]))
        rownames(x) <- x[["sampleId"]]
        x <- x[sort(rownames(x)), ]
        x
    }
