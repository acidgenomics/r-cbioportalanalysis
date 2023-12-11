#' cBioPortalAnalysis
#'
#' Toolkit for performing queries on cBioPortal.
"_PACKAGE"



## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase cast export makeDimnames makeNames
#' mapGenesToRownames predictSensitivity sanitizeNa
#' @importFrom BiocGenerics do.call lapply order rbind sort unique
#' @importFrom IRanges gsub
NULL

#' @importMethodsFrom AcidExperiment mapGenesToRownames
#' @importMethodsFrom AcidPlyr cast
#' @importMethodsFrom pipette sanitizeNa
#' @importMethodsFrom syntactic camelCase makeDimnames makeNames
NULL



## Standard functions ==========================================================

# > #' @importFrom cBioPortalData cBioPortal getStudies

#' @importFrom AcidBase basenameSansExt initDir pasteUrl tempdir2 unlink2
#' @importFrom AcidCLI abort alert alertWarning
#' @importFrom AcidExperiment makeSummarizedExperiment
#' @importFrom AcidGenomes NcbiGeneInfo
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom MultiAssayExperiment experiments sampleMap
#' @importFrom goalie areDisjointSets areIntersectingSets areSetEqual assert
#' hasColnames hasLength hasNoDuplicates hasRownames hasRows isADir isAFile
#' isCharacter isInt isString isSubset requireNamespaces
#' @importFrom methods as is
NULL
