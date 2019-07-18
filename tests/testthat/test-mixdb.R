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


test_that("mixdb's `dictionary`` is a dictionary", {
    expect_is(test_mixdb_dictionary, "dictionary")
})

test_that("all work on test_tbl", {
    expect_is(test_tbl_mixdb, "mixdb")
    expect_setequal(test_tbl_dict, 1:48)
    expect_equal(test_tbl_freq[[1]], 1357L)
})


test_that("add id to meta and not gender to data", {
  out_a <- mixdb(test_db, meta_vars(id, gender))
  expect_named(attr(out_a, "meta"), c("id", "gender", "notes", "class"))
  expect_length(out_a[["x"]][[1]], 2L)
  expect_false(any(stringr::str_detect(
    names(out_a[["x"]][[1]]),
    "\\[sep\\]"
  )))
})


test_that("works with added columns", {
  out_b <- mixdb(test_db, meta_vars(id))
  expect_named(attr(out_b, "meta"), c("id", "notes", "gender", "class"))
  expect_length(out_b[["x"]][[1]], 4L)
  expect_false(any(stringr::str_detect(names(out_b[["x"]][[1]]), " ")))
  expect_equal(names(out_b[["x"]][[1L]]), c("foo", "notes", "[SEP]", "male"))
})
