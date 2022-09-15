#' Get available molecular (i.e. genetic) data profiles for a specific cancer
#' study
#'
#' @export
#' @note Updated 2022-09-15.
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' x <- molecularProfiles(studyId = "pancan_pcawg_2020")
#' print(x)
molecularProfiles <-
    function(studyId, .api = NULL) {
        if (is.null(.api)) {
            .api <- .api()
        }
        assert(
            isString(studyId),
            is(.api, "cBioPortal")
        )
        x <- cBioPortalData::molecularProfiles(
            api = .api,
            studyId = studyId
        )
        assert(
            is.data.frame(x),
            hasNoDuplicates(x[["molecularProfileId"]])
        )
        x <- as(x, "DataFrame")
        rownames(x) <- x[["molecularProfileId"]]
        x <- x[sort(rownames(x)), sort(colnames(x))]
        x
    }
