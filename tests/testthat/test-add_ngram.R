test_that("add_ngram() returned the correct class", {
    expect_is(add_ngram(test_mixdb, 1), "mixdb")
    expect_identical(test_mixdb, add_ngram(test_mixdb, 1))

    expect_is(add_ngram(test_mixdb), "mixdb")

    expect_is(add_ngram(test_mixdb, 3), "mixdb")
    expect_is(add_ngram(test_mixdb, 13), "mixdb")
})

test_that("add_ngram() default method work properly", {
    expect_message(try(add_ngram(1)), "provided is of class")
    expect_message(try(add_ngram(1)), "must inherits.*mixdb")
    expect_error(add_ngram(1), "mixdb.*x", class = "usethis_error")

    expect_message(
        try(add_ngram(test_mixdb, "two")),
        "provided is of class"
    )
    expect_message(
        try(add_ngram(test_mixdb, "two")),
        "must be an.*integer"
    )
    expect_error(
        add_ngram(test_mixdb, "two"),
        "integer.*n_gram",
        class = "usethis_error"
    )
})


test_that("add_ngram() correct dictionary", {
    expect_equal(
        add_ngram(test_mixdb, 2)[["x"]][[1]],
        c("foo" = 2, "notes" = 1, "foo__GRAM__notes" = 9)
    )
})


test_that("add_bigram works on a small example", {
    ex_mixdb_gram <- mixdb(
        tibble::tibble(
            text = c("a b b", "a b c c c c"),
            class = 0:1
        )
    )

    ex_bigram <- add_ngram(ex_mixdb_gram)

    expect_is(ex_bigram, "mixdb")
    expect_equivalent(unclass(get_dictionary(ex_bigram)), 1:7) # 3 + 4
    expect_equal(
        names(ex_bigram[["x"]][[1]]),
        c("a", "b", "b", "a__GRAM__b", "b__GRAM__b")
    )
    expect_equal(
        names(ex_bigram[["x"]][[2]]),
        c("a", "b", "c", "c", "c", "c",
          "c__GRAM__c", "c__GRAM__c", "c__GRAM__c", "a__GRAM__b", "b__GRAM__c")
    )
    expect_equal(
        names(get_dictionary(ex_bigram)),
        c("c", "b", "c__GRAM__c", "a", "a__GRAM__b", "b__GRAM__b", "b__GRAM__c")
    )
    expect_equivalent(
        unclass(get_frequencies(get_dictionary(ex_bigram))),
        c("c" = 4, "b" = 3, "c__GRAM__c" = 3, "a" = 2, "a__GRAM__b" = 2,
          "b__GRAM__b" = 1, "b__GRAM__c" = 1)
    )

})

test_that("add_trigram works on a small example", {
    ex_mixdb_gram <- mixdb(
        tibble::tibble(
            text = c("a b b", "a b c c c c"),
            class = 0:1
        )
    )

    ex_trigram <- add_ngram(ex_mixdb_gram, 3)

    expect_is(ex_trigram, "mixdb")
    expect_equivalent(unclass(get_dictionary(ex_trigram)), 1:11) # 3 + 4 + 4
    expect_equal(
        names(ex_trigram[["x"]][[1]]),
        c("a", "b", "b", "a__GRAM__b", "b__GRAM__b", "a__GRAM__b__GRAM__b")
    )
    expect_equal(
        names(ex_trigram[["x"]][[2]]),
        c("a", "b", "c", "c", "c", "c",
          "c__GRAM__c", "c__GRAM__c", "c__GRAM__c", "c__GRAM__c__GRAM__c",
          "c__GRAM__c__GRAM__c", "a__GRAM__b", "b__GRAM__c", "a__GRAM__b__GRAM__c",
          "b__GRAM__c__GRAM__c")
    )
    expect_equal(
        names(get_dictionary(ex_trigram)),
        c("c", "b", "c__GRAM__c", "a", "a__GRAM__b", "c__GRAM__c__GRAM__c",
          "a__GRAM__b__GRAM__b", "a__GRAM__b__GRAM__c", "b__GRAM__b", "b__GRAM__c",
          "b__GRAM__c__GRAM__c")
    )
    expect_equivalent(
        unclass(get_frequencies(get_dictionary(ex_trigram))),
        c("c" = 4, "b" = 3, "c__GRAM__c" = 3, "a" = 2, "a__GRAM__b" = 2,
          "c__GRAM__c__GRAM__c" = 2, "a__GRAM__b__GRAM__b" = 1, "b__GRAM__b" = 1,
          "b__GRAM__c" = 1, "a__GRAM__b__GRAM__c" = 1, "b__GRAM__c__GRAM__c" = 1)
    )
})

test_that("add_ngram() works on test_tbl", {
    test_dict_add_gram <- get_dictionary(add_ngram(test_tbl_mixdb))
    test_freq_add_gram <- get_frequencies(test_dict_add_gram)
    expect_gt(max(test_freq_add_gram), 1L)
    expect_equal(test_freq_add_gram[[1L]], 1357L)
    expect_equal(test_freq_add_gram[[2L]], 727L)
    expect_equal(test_dict_add_gram[[1L]], 1L)
    expect_equal(test_dict_add_gram[[2L]], 2L)
    expect_equal(names(test_dict_add_gram[1L]), "2")
    expect_equal(names(test_dict_add_gram[2L]), "2__GRAM__2")
})
