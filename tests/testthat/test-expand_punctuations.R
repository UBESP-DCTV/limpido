test_that("expansion works", {
    target <- c("abc de", "a.b", "A.B", "c'è", "s.p.a.")
    expected <- c("abc de", "a . b", "A . B", "c ' è", "s . p . a .")
    expect_equal(
        expand_punctuations(target),
        expected
    )
})


test_that("don't detouch tags", {
    expect_equal(expand_punctuations("[NUM]"), "[NUM]")
    expect_equal(expand_punctuations("abc[NUM]"), "abc [NUM]")
})


test_that("options for words with numbers", {
    expect_equal(expand_punctuations("1a2b"), "1a2b")
})
