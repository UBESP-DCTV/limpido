test_mixdb <- mixdb(test_db, meta_vars(id, gender))
test_meta <- attr(test_mixdb, "meta")
test_dictionary <- attr(test_mixdb, "dictionary")

test_that("mixdb() returned the correct class", {
  expect_is(test_mixdb, "mixdb")
})

test_that("mixdb() default method work properly", {
    expect_output(try(mixdb(1)), "provided is of class")
    expect_output(try(mixdb(1)), "must inherits.*data\\.frame")
    expect_error(mixdb(1), "data.frame.*\\.data",
        class = "usethis_error"
    )
})

test_that("mixdb() return a named list", {
    expect_type(test_mixdb, "list")
    expect_named(test_mixdb, c("x", "y"))
})

test_that("mixdb's x and y have correct shapes", {
    expect_equal(length(test_mixdb$x), length(test_mixdb$y))
    expect_is(test_mixdb$x, "list")
    expect_true(all(
        purrr::map_lgl(test_mixdb$x, ~class(.x) == "integer")
    ))
    expect_is(test_mixdb$y, "factor")
})

test_that("mixdb's `meta` is a coherent tibble", {
    expect_is(test_meta, "tbl_df")
    expect_equal(nrow(test_meta), length(test_mixdb$x))
})

test_that("names are correctly passed by `meta`", {
    expect_equal(names(test_meta), c("id", "gender", "notes", "class"))
})

test_that("mixdb's `dictionary`` is sorted table", {
    expect_is(test_dictionary, "table")
    expect_identical(
        test_dictionary,
        sort(test_dictionary, decreasing = TRUE)
    )
})

