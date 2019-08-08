test_base_ngram <- create_ngram(c(1, 2, 2, 4, 4, 4, 4, 3, 3, 3))

test_that("create_ngram for basic structure", {
    expect_is(test_base_ngram, "dictionary")
    expect_named(
        test_base_ngram,
        c("4__GRAM__4", "3__GRAM__3", "1__GRAM__2", "2__GRAM__2", "2__GRAM__4", "4__GRAM__3")
    )
    expect_equivalent(unclass(test_base_ngram), 1:6 + 4L)
    expect_equivalent(
        unclass(get_frequencies(test_base_ngram)),
        c(3, 2, 1, 1, 1, 1)
    )
})


test_that("create_ngram for mixdb", {
    expect_is(create_ngram(test_mixdb), "dictionary")
    expect_named(
        create_ngram(test_mixdb),
        c('annotated__GRAM__foo', 'another__GRAM__notes', 'bar__GRAM__notes',
          'foo__GRAM__notes')
    )
    expect_setequal(create_ngram(test_mixdb), 1:4 + 5L)
    expect_true(all(
        create_ngram(test_mixdb) > max(attr(test_mixdb, "dictionary"))
    ))
})


test_that("create_ngram works on test_tbl", {
    test_tbl_ngram_1 <- create_ngram(test_tbl_mixdb[["x"]][[1]])

    expect_is(test_tbl_ngram_1, "dictionary")
    expect_setequal(test_tbl_ngram_1, 1:85 + 31L)
    expect_equal(get_frequencies(test_tbl_ngram_1)[[1]], 47)
})


test_that("bug molteplicità singolo documento", {
    test_bug_1 <- mixdb(
        tibble::tibble(
            text = c("a b b", "a b c c c c"),
            class = 0:1
        )
    )

    expect_equal(get_frequencies(create_ngram(test_bug_1))[["c__GRAM__c"]], 3L)
    expect_equal(get_frequencies(create_ngram(test_bug_1))[["a__GRAM__b"]], 2L)
    expect_equal(get_frequencies(create_ngram(test_bug_1))[["b__GRAM__b"]], 1L)
    expect_equal(get_frequencies(create_ngram(test_bug_1))[["b__GRAM__c"]], 1L)
})


# test_that("works with n > 2", {
#     create_ngram(c(1, 2, 3, 4), 3)
# })
