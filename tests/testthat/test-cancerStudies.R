test_that("cancerStudies", {
    x <- cancerStudies()
    expect_s4_class(x, "DFrame")
    expect_identical(rownames(x)[[1L]], "acbc_mskcc_2015")
})
