## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DataFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase makeDimnames makeNames sanitizeNA
#' @importFrom AcidPlyr cast
#' @importFrom BiocGenerics do.call lapply order rbind sort unique
#' @importFrom IRanges gsub
#'
#' @importMethodsFrom AcidPlyr cast
#' @importMethodsFrom pipette sanitizeNA
#' @importMethodsFrom syntactic camelCase makeDimnames makeNames
NULL



## Standard functions ==========================================================

#' @importFrom AcidCLI abort alert alertWarning
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes EntrezGeneInfo
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom cBioPortalData cBioPortal getStudies
#' @importFrom goalie areSetEqual assert hasColnames hasNoDuplicates hasRownames
#' hasRows isCharacter isInt isString isSubset
#' @importFrom methods as is
NULL
