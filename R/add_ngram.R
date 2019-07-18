#' Add ngram to a mixdb
#'
#' @param x a \code{\link{mixdb}} object.
#' @param n_gram (int, default = 2) maximum level of ngram to consider
#'    to enrich \code{\link{mixdb}}'s texts and dictionary.
#'
#' @return a \code{\link{mixdb}} wiht the ngram added
#'
#' @export
#'
#' @examples
#' library(tibble)
#' library(limpido)
#' test <- tribble(
#'   ~class, ~id, ~notes,                  ~gender,
#'      0  ,   1, "foo notes foo",          "male",
#'      0  ,   2, "bar notes foo",          "female",
#'      1  ,   1, "another notes",          "male",
#'      1  ,   3, "annotated foo one two",  "female"
#' )
#' foo <- mixdb(test, meta_vars(id, gender))
#' add_ngram(foo)
#' add_ngram(foo, 3)
add_ngram <- function(x, n_gram = 2L) {
    UseMethod("add_ngram", x)
}

#' @rdname add_ngram
#' @export
add_ngram.default <- function(x, n_gram = 2L) {
    ui_info(
        "{ui_field('x')} must inherits from {ui_value('mixdb')}"
    )
    ui_oops(
        "{ui_field('x')} provided is of class {ui_value(class(x))}"
    )
    ui_stop(c(
        "Please, provide a {ui_value('mixdb')} to {ui_field('x')}"
    ))

}

#' @rdname add_ngram
#' @export
add_ngram.mixdb <- function(x, n_gram = 2L) {

    if (
        !is.numeric(n_gram) ||
        n_gram != as.integer(n_gram) ||
        n_gram == 0L
    ) {
        ui_info(
            "{ui_field('n_gram')} must be an {ui_value('integer')}"
        )
        ui_oops(
            "{ui_field('n_gram')} provided is of class {ui_value(class(n_gram))}"
        )
        ui_stop(c(
            "Please, provide an {ui_value('integer')} to {ui_field('n_gram')}"
        ))
    }

    if (n_gram == 1L) return(x)

    dict_x <- get_dictionary(x)
    dict_ngrams <- create_ngram(x, n_gram = n_gram)
    corpus_dict <- c_dict(dict_x, dict_ngrams)

    x[["x"]][] <- furrr::future_map(x[["x"]], ~{
        freq_ngrams <- create_ngram(names(.x), n_gram = n_gram) %>%
            get_frequencies()

        c(
            corpus_dict[names(.x)],
            corpus_dict[rep(names(freq_ngrams), freq_ngrams)]
        )
    },.progress = TRUE)

    attr(x, "dictionary") <- corpus_dict
    x
}
