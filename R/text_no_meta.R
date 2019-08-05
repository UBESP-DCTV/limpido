#' Select textual column excluding meta information
#'
#' @param x (data frame) input data from which extract only the textual
#'     columns not included in `meta`
#' @param meta unquoted list of column names passed using the
#'     \code{\link{meta_vars}()} function.
text_no_meta <- function(x, meta = NULL) {

    # meta should be a `quosures`
    if (inherits(meta, "quosures")) {
        x <- dplyr::select(x, -c(!!!unique(meta)))
    } else if (!is.null(meta)) {
        ui_info(
            "{ui_field('meta')} provided inherits to {ui_value(class(meta))}."
        )
        ui_stop(
            "{ui_field('meta')} must be {ui_value('NULL')} or inherits to {ui_value('quosures')}."
        )
    }

    dplyr::select_if(x, is.character)
}
