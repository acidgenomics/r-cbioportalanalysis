# cBioPortalAnalysis

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

### [Docker][] method

```sh
image='acidgenomics/r-packages:cbioportalanalysis'
workdir='/mnt/work'
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

## Details

- [RNA expression values](https://docs.cbioportal.org/1.-general/faq#rna)
- [Z-score normalization method](https://github.com/cBioPortal/cbioportal/blob/master/docs/Z-Score-normalization-script.md)

Currently using [cgdsr][] internally, but am evaluating the new [cBioPortalData][] package for inclusion in a future release.

[cbioportal]: https://www.cbioportal.org/
[cbioportaldata]: https://bioconductor.org/packages/cBioPortalData/
[cgdsr]: https://cran.r-project.org/package=cgdsr
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
