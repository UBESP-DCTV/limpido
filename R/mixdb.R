#' mixdb object
#'
#' \code{\link[limpido]{mixdb}} reads a .data general database stored in a
#' data.frame and convert it in a parsed list for mlt algorithms (keras
#'  and tensorflow) aimed to Natural Language or mixed formad analyses.
#'
#' @details The `.data`` data frame in input must have the following
#'   (named) column:
#'   \describe{
#'     \item{class}{(factor) classification's labels, stored in a
#'       factor. It accepts also an integer (or something cohercible to
#'       it) or character vector, creating a factor from them. Any other
#'       class will return an error.
#'     }
#'   }
#'
#'   All the other columns listed in the `.meta_vars` parameter will
#'   represents metadata of the datasets, useful for other purposes then
#'   NLP, e.g., possible future non-textual covariates for the models.
#'
#'   All the character columns not listed in `.meta_vars` will be
#'   treated as textual ones and converted, accordingly to the
#'   dictionary provided
#'
#'   All the other columns will be ignored and not saved anywhere.
#'
#' @param .data (data frame) of .data data. (see **details** for specs)
#' @param .meta_vars optional list of (unquoted) columns' names generated
#'   by \code{\link{meta_vars}()}, and representing metadata of
#'   interest. (see **details** for specs)
#'
#' @return an object of class \code{\link[limpido]{mixdb}}, with an
#'   attribute **meta** of class [tibble][tibble::tibble-package]
#'   reporting the additional provided and selected (by `.meta_vars`)
#'   information.
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
mixdb <- function(.data,
                  .meta_vars = NULL,
                  lowercase = TRUE,
                  ...,
                  locale = "en"
) {
    UseMethod("mixdb", .data)
}


mixdb.default <- function(.data,
                          .meta_vars = NULL,
                          lowercase = TRUE,
                          ...,
                          locale = "en"
) {
    ui_info(
        "{ui_field('.data')} must inherits from {ui_value('data.frame')}"
    )
    ui_oops(
        "{ui_field('.data')} provided is of class {ui_value(class(.data))}"
    )
    ui_stop(c(
        "Please, provide a {ui_value('data.frame')} to {ui_field('.data')}"
    ))
}


mixdb.data.frame <- function(.data = test_db,
                             .meta_vars = NULL,
                             lowercase = TRUE,
                             ...,
                             locale = "en"
) {
    # define the textual base dataset: not meta character columns
    text_df <- dplyr::select(.data, -c(!!!unique(.meta_vars))) %>%
        dplyr::select_if(is.character)

    # define the meta dataset (including text and classes)
    meta_df <- .data %>%
        dplyr::select(!!!c(
            # they are disjoint by costruction!
            .meta_vars,
            syms(c(names(text_df), "class")))
        )

    # extract words (token will be applyed later, eventualy)
    text <- as.list(purrr::reduce(text_df, paste, sep = " [SEP] ")) %>%
        furrr::future_map(
            stringi::stri_extract_all_words,
            .progress = TRUE
        ) %>%
        purrr::flatten()

    if (lowercase) {
        text <- map(text, stringr::str_to_lower, locale = locale)
    }

    # define the dictionary (sorted table of named integers)
    dictionary <- unlist(text) %>%
        table() %>%
        sort.int(decreasing = TRUE)


    structure(
        list(
            # this is a list of integer vector representing the mapping
            # to the vocabulary for the text. each entry is an
            # observation.
            x = furrr::future_map(text, ~as.integer(dictionary[.])),

            # this is a factor of the output classes. each entry is
            # to the corresponding observation.
            y = as.factor(.data[["class"]])
        ),
        meta  = meta_df,
        dictionary = dictionary,
        class = "mixdb"
    )
}
