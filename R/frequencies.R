#' Extractor for the attribute `frequencies`, of objects of class
#' \code{\link{dictionary}}
#' @param dict a \code{\link{dictionary}} object.
#'
#' @return (int) named vector of the words' frequencies in the
#'     \code{\link{dictionary}}
#'
#' @seealso \code{\link{dictionary}}
#' @export
#'
#' @examples
#' dic <- dictionary(c("a", "b", "b"))
#' get_frequencies(dic)
get_frequencies <- function(dict) {
    UseMethod("get_frequencies", dict)
}

#' @rdname  get_frequencies
#' @export
get_frequencies.dictionary <- function(dict) {
    attr(dict, "frequencies")
}
