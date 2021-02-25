## FIXME Can we remove dependence on dplyr, rlang, and tibble here?



#' cBioPortalAnalysis
#'
#' Toolkit for performing queries on cBioPortal.
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom basejump alert alertWarning camelCase
#' @importFrom cgdsr CGDS getCancerStudies getCaseLists getGeneticProfiles
#'   getProfileData
#' @importFrom dplyr arrange_all bind_rows everything select
#' @importFrom goalie assert isCharacter isString
#' @importFrom methods as is
#' @importFrom rlang !!! !! sym syms
#' @importFrom tibble rownames_to_column
"_PACKAGE"



#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param cancerStudy `character(1)`.
#'   Cancer study ID.
#'   Refer to [cancerStudies()] for details.
NULL
