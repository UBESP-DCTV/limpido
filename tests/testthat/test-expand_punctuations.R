test_that("expansion works", {
    target <- c("abc de", "a.b", "A.B", "c'è", "s.p.a.",
                "a;;b", "a,,b", "a;,b", "ciao... ciao")
    expected <- c("abc de", "a . b", "A . B", "c ' è", "s . p . a .",
                  "a ; b", "a , b", "a ; , b", "ciao __ELIPSIS__ ciao")
    expect_equal(
        expand_punctuations(target),
        expected
    )
})


test_that("don't detouch tags", {
    expect_equal(expand_punctuations("__NUM__"), "__NUM__")
    expect_equal(expand_punctuations("abc__NUM__"), "abc__NUM__")
})


test_that("options for words with numbers", {
    expect_equal(expand_punctuations("1a2b"), "1a2b")
})
