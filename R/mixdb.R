#' mixdb object
#'
#' \code{\link[limpido]{mixdb}} reads a raw general database stored in a
#' data.frame and convert it in a parsed list for mlt algorithms (keras
#'  and tensorflow) aimed to Natural Language or mixed formad analyses.
#'
#' @details The `raw`` data frame in input must have the following
#'   (named) column:
#'   \describe{
#'     \item{class}{(factor) classification's labels, stored in a
#'       factor. It accepts also an integer (or something cohercible to
#'       it) or character vector, creating a factor from them. Any other
#'       class will return an error.
#'     }
#'   }
#'
#'   All the other columns listed in the `meta` parameter will
#'   represents metadata of the datasets, useful for other purposes then
#'   NLP, e.g., possible future non-textual covariates for the models.
#'
#'   All the character columns not listed in `meta` will be treated as
#'   textual ones and converted, accordingly to the dictionary provided
#'
#'   All the other columns will be ignored and not saved anywhere.
#'
#' @param raw (data frame) of raw data. (see **details** for specs)
#' @param meta (list, default `NULL``) optional list of (unquoted)
#'   columns' names representing metadata. (see **details** for specs)
#'
#' @return an object of class \code{\link[limpido]{mixdb}}
#'
#' @note As a convention, "0" does not stand for a specific word, but
#'   instead is used to encode any unknown word.
#' @export
#'
#' @examples
#' library(tibble)
#' library(limpido)
#' test <- tribble(
#'   ~class, ~id, ~notes,      ~gender,
#'      0  ,   1, "foo notes",     "male",
#'      0  ,   2, "bar notes",     "female",
#'      1  ,   1, "another notes", "male",
#'      1  ,   3, "annotated foo", "female"
#' )
#' mixbd(test, meta = list(id, gender))
#'
mixdb <- function(raw, meta) {
 ui_stop()
}
