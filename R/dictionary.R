#' Dictionary
#'
#' Create a \code{\link{dictionary}} object.
#'
#' A \code{\link{dictionary}} is an ordered and dense integer vector
#' representing the coding of the words in some vocabulary.
#' The vector is named with names corresponding to the (single) words
#' in the vocabulary.
#'
#' It has an attribute `frequencies` reporting (in order from the most
#' frequent to the most rare) the original word-frequency of the
#' document(s) which created the dictionary. The `frequencies`
#' attributes is accessibly by the function \code{\link{get_frequencies}}
#'
#' @param x an object from which create the \code{\link{dictionary}}
#'
#' @return a \code{\link{dictionary}}
#' @name dictionary
#' @seealso \code{\link{get_frequencies}}
#' @export
#'
#' @examples
#' dic <- dictionary(c("a", "b", "b"))
dictionary <- function(x) {
    UseMethod("dictionary", x)
}

#' @rdname dictionary
#' @export
dictionary.character <- function(x) {
    freq <- sort.int(table(x), decreasing = TRUE)

    tokens <- names(freq)
    freq <- as.integer(freq)

    structure(
        purrr::set_names(seq_along(tokens), tokens),

        frequencies = purrr::set_names(freq, tokens),

        class = "dictionary"
    )
}

#' @rdname dictionary
#' @export
dictionary.list <- function(x) {
    if (!all(purrr::map_lgl(x, is.character))) ui_stop(
        "All the element of {ui_field('x')} must inherits to {ui_value('character')}"
    )

    dictionary(unlist(x))
}
