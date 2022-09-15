## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DataFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase makeDimnames makeNames sanitizeNA
#' @importFrom BiocGenerics do.call lapply rbind sort unique
#' @importFrom IRanges gsub
#'
#' @importMethodsFrom pipette sanitizeNA
#' @importMethodsFrom syntactic camelCase makeDimnames makeNames
NULL



## Standard functions ==========================================================

#' @importFrom AcidCLI abort alert alertWarning
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom cBioPortalData cBioPortal getStudies
#' @importFrom goalie assert hasColnames hasNoDuplicates hasRownames hasRows
#' isCharacter isInt isString isSubset
#' @importFrom methods as is
NULL
