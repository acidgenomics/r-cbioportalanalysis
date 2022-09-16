#' @name params
#' @inherit AcidRoxygen::params return title
#' @keywords internal
#'
#' @param geneNames `character`.
#' HUGO gene symbols (e.g. `"MYC"`, `"TP53"`).
#'
#' @param molecularProfileId `character(1)`.
#' Molecular profile identifier (e.g. `"ccle_broad_2019_cna"`).
#' Refer to [molecularProfiles()] for details.
#'
#' @param studyId `character(1)`.
#' Cancer study identifier (e.g. `"ccle_broad_2019"`).
#' Refer to [cancerStudies()] for details.
#'
#' @param .api `cBioPortal` or `NULL`.
#' `cBioPortal` API connection.
#' Can be left `NULL` by default.
NULL
