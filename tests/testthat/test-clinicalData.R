test_that("clinicalData", {
    x <- clinicalData("ccle_broad_2019_sequenced")
    expect_s4_class(x, "DFrame")
    expect_identical(rownames(x)[[1L]], "A101D_SKIN")
})
