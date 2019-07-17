test_that("meta_vars works", {
    expect_is(meta_vars(foo), "quosures")
    expect_is(meta_vars(foo, bar), "quosures")
})
