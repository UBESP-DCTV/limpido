#' This helper is intended to provide equivalent semantics to
#' \code{\link[dplyr]{select}()}
#'
#' @param ... Variables to include/exclude like meta in the creation
#'   of a \code{\link{mixdb}} object
#'
#' @export
meta_vars <- function(...) {
    enquos(...)
}
