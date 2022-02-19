test_that("add_oov_when_greater_than works", {
    # setup
    corpus <- list(
      a = setNames(seq_along(letters), letters),
      A = setNames(seq_along(letters), LETTERS)[1:10]
    )

    # evaluate
    res <- add_oov_when_greater_than(corpus, 20)

    # test
    expect_type(res, "list")

    expect_type(res[["a"]], "integer")
    expect_equivalent(res[["a"]], c(1:20, rep(21, 6)))

    expect_type(res[["A"]], "integer")
    expect_equivalent(res[["A"]], c(1:10))

    expect_named(
        res[["a"]],
        c(
            names(corpus[["a"]])[1:20],
            rep("__OOV__", 6)
        )
    )
    expect_named(res[["A"]], names(corpus[["A"]]))
})
