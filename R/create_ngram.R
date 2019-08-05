#' Create ngram set
#'
#' @param x an object of class \code{\link{mixdb}}.
#' @param n_gram (int, default = 2L) number ($n$) of gram to
#'   create.
#'
#' @return (table) named sorted table of
create_ngram <- function(x, n_gram = 2L) {
    UseMethod("create_ngram", x)
}

#' @rdname create_ngram
#' @export
create_ngram.numeric <- function(x, n_gram = 2) {
    create_ngram(as.character(x), n_gram = n_gram)
}

#' @rdname create_ngram
#' @export
create_ngram.character <- function(x, n_gram = 2L) {

    stopifnot(
        is.numeric(n_gram) &&
        n_gram == as.integer(n_gram) &&
        n_gram > 0L
    )

    if (n_gram == 1L) return(dictionary(x))



    skip <- length(unique(x))
    n_gram[[1L]] <- min(length(x), n_gram)

    n_ngrams <- integer(1)
    dictionaries <- vector("list", n_gram - 1L)

    for (i in seq_len(n_gram)[-1]) {
        n_ngrams[[1L]] <- length(x) - i + 1L

        indices <- purrr::map(seq_len(n_ngrams), ~ .:(. + i - 1L))

        tokens <- purrr::map_chr(indices, ~ {
            paste0(x[.x], collapse = "[GRAM]")
        })

        dictionaries[[i - 1L]] <- dictionary(tokens) + skip
        skip[[1L]] <- skip[[1L]] + n_ngrams
    }


    if (n_gram > 2L) {
        purrr::reduce(dictionaries, c_dict)
    } else {
        dictionaries[[1L]]
    }
}

create_ngram.mixdb <- function(x, n_gram = 2L) {
    ngrams_list <- furrr::future_map(x[["x"]],
        ~create_ngram(names(.x), n_gram = n_gram),
        .progress = TRUE
    )

    tokens <- unlist(purrr::map(ngrams_list, ~{
        freq <- get_frequencies(.x)
        rep(names(freq), freq)
    }))

    dictionary(tokens) + max(attr(x, "dictionary"))
}
