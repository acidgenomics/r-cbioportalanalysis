#' cBioPortalAnalysis
#'
#' Toolkit for performing queries on cBioPortal.
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom basejump alert alertWarning camelCase do.call lapply rbind
#' @importFrom cgdsr CGDS getCancerStudies getCaseLists getGeneticProfiles
#'   getProfileData
#' @importFrom goalie assert hasRownames isCharacter isString
#' @importFrom methods as is
"_PACKAGE"



#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param cancerStudy `character(1)`.
#'   Cancer study ID.
#'   Refer to [cancerStudies()] for details.
NULL
