## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase makeDimnames makeNames sanitizeNA
#' @importFrom AcidPlyr cast
#' @importFrom BiocGenerics do.call lapply order rbind sort unique
#' @importFrom IRanges gsub
NULL

#' @importMethodsFrom AcidPlyr cast
#' @importMethodsFrom pipette sanitizeNA
#' @importMethodsFrom syntactic camelCase makeDimnames makeNames
NULL



## Standard functions ==========================================================

# > #' @importFrom cBioPortalData cBioPortal getStudies

#' @importFrom AcidBase pasteURL tempdir2 unlink2
#' @importFrom AcidCLI abort alert alertWarning
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes NcbiGeneInfo
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom goalie areSetEqual assert hasColnames hasNoDuplicates hasRownames
#' hasRows isADir isAFile isCharacter isInt isString isSubset requireNamespaces
#' @importFrom methods as is
NULL
