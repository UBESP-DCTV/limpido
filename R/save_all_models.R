#' Save all the relevant object for a DL model
#'
#' it saves:
#' @param model a keras model
#' @param history  akeras history
#' @param params output of [setup_input_data]
#' @param plot (ggplot, default = NULL) if a plot is provided, it save
#'     a PNG of the plot too
#' @param .now (POSIXct, dafault `lubridate::now()`) time to include in
#'    the saved files'names.
#' @param fine_tuned (lgl, default = FALSE) is the model the fine tuned
#'    one?.
#'
#' @return invisibly `TRUE`
#' @export
save_all_models <- function(
    model,
    history,
    params,
    plot = NULL,
    .now = lubridate::now(),
    id = NULL,
    fine_tuned = FALSE
) {
    output_path <- params[["output_path"]]
    if (is.null(id)) {
        files <- here::here("R") %>%
            list.files("^[0-9]+.*\\.R$", full.names = TRUE)
        last_modified <- file.info(files)[["mtime"]] %>% which.max()

        id <- dplyr::if_else(length(files) > 0,
            true = paste0("id",
                basename(files[[last_modified]]) %>%
                    stringr::str_extract("^[0-9]+")
            ),
            false = "noid"
        )
    }

    if (fine_tuned) {
        id <- paste0(id, "-fine_tuned")
    }

    current <- gsub('\\D', '', .now)
    model_file <- file.path(output_path,
        glue::glue("{current}-model-{id}.hdf5")
    )
    keras::save_model_hdf5(model, model_file)

    model_raw <- keras::serialize_model(model)
    raw_file <- file.path(output_path,
        glue::glue("{current}-history_and_raw-{id}.rda")
    )

    preds <- predict_otiti_classes(model, params)

    save(history, model_raw, params, preds, file = raw_file)


    # Plots and summaries ---------------------------------------------
    if (inherits(plot, "ggplot")) {
        p_path <- file.path(output_path,
            glue::glue("{current}-learning_curves-{id}.png")
        )
        ggplot2::ggsave(p_path, plot, width = 8.3, height = 11.7)
    }

    invisible(TRUE)
}
