#' expand punctuation
#'
#' @param text (chr) vector
#'
#' @return a character vector with all punctuation, sourrendered by
#'    spaces
#' @export
#'
#' @examples
#' expand_punctuations(c("abc de", "a.b", "A.B", "c'è", "s.p.a."))
expand_punctuations <- function(text) {
    UseMethod("expand_punctuations", text)
}

#' @describeIn expand_punctuations method for characters
#' @export
expand_punctuations.character <- function(text) {

    # the aim is to shrink punctuation repetitions
    regex <- "(\\W)\\1*"

    # avoid shrink on elipsis
    text <- stringr::str_replace_all(text, "\\.{3}", " __ELIPSIS__ ")

    # add spaces around (single occurrences of) punctuation
    text <- stringr::str_replace_all(text, regex, " \\1 ")

    # remove extra spaces
    stringr::str_squish(text)
}
