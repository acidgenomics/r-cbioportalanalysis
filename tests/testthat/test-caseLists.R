test_that("caseLists", {
    x <- caseLists("ccle_broad_2019")
    expect_s4_class(x, "DFrame")
    expect_identical(rownames(x)[[1L]], "ccle_broad_2019_all")
})
