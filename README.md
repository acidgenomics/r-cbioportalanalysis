# cBioPortalAnalysis

![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

Toolkit for performing queries on [cBioPortal][].

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "cBioPortalAnalysis",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

## Details

- [RNA expression values](https://docs.cbioportal.org/1.-general/faq#rna)
- [Z-score normalization method](https://github.com/cBioPortal/cbioportal/blob/master/docs/Z-Score-normalization-script.md)

Currently using [cgdsr][] internally, but am evaluating the new [cBioPortalData][] package for inclusion in a future release.

[cbioportal]: https://www.cbioportal.org/
[cbioportaldata]: https://bioconductor.org/packages/cBioPortalData/
[cgdsr]: https://cran.r-project.org/package=cgdsr
[r]: https://www.r-project.org/
