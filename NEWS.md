# cBioPortalAnalysis 0.1.0 (2022-06-06)

### Major changes

- Now requiring R 4.2 / Bioconductor 3.15.
- Added code coverage using testthat, and updated lintr checks.

### Minor changes

- Updated NAMESPACE, splitting out basejump imports into original packages.

## cBioPortalAnalysis 0.0.2 (2021-02-26)

### Major changes

- Reworked `rnaSeqData` function to return as `SummarizedExperiment`.
  Only a single `cancerStudy` is now intentionally supported per function
  call, making looping a little bit easier to grasp.
- Added support via `clinicalData` for downloading of patient and cell line
  metadata, which gets defined into `colData` for the `SummarizedExperiment`
  now returned by `rnaSeqData`.

### Minor changes

- Simplified the NAMESPACE, using the basejump v0.14 release series.
- Removed dependency on dplyr, rlang, and other tidyverse software.
- Hardened internal assert checks.

## cBioPortalAnalysis 0.0.1 (2020-10-09)

Initial release.
