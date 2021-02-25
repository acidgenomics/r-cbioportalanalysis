#' cBioPortalAnalysis
#'
#' Toolkit for performing queries on cBioPortal.
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom basejump alert alertWarning camelCase do.call lapply rbind
#'   sanitizeNA snakeCase
#' @importFrom cgdsr CGDS getCancerStudies getCaseLists getGeneticProfiles
#'   getProfileData
#' @importFrom goalie assert hasRownames isCharacter isInt isString isSubset
#' @importFrom methods as is
"_PACKAGE"



#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param cancerStudy `character(1)`.
#'   Cancer study identifier (e.g. "ccle_broad_2019").
#'   Refer to [cancerStudies()] for details.
#' @param caseList `character(1)`.
#'   Case list identifier (e.g. "ccle_broad_2019_sequenced").
#'   Refer to [caseLists()] for details.
NULL