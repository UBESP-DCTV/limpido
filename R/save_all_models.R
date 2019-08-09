#' Save all the relevant object for a DL model
#'
#' it saves:
#' @param model a keras model
#' @param history  akeras history
#' @param output_path (chr) the path for the output folder
#' @param plot (ggplot, default = NULL) if a plot is provided, it save
#'     a PNG of the plot too
#' @param .now (POSIXct, dafault `lubridate::now()`) time to include in
#'    the saved files'names.
#'
#' @return invisibly `TRUE`
#' @export
save_all_models <- function(
    model,
    history,
    output_path,
    plot = NULL,
    .now = lubridate::now()
) {

    current <- gsub('\\D', '', .now)
    model_file <- file.path(output_path,
        glue::glue("keras-model-{current}.hdf5")
    )
    keras::save_model_hdf5(model, model_file)

    model_raw <- keras::serialize_model(model)
    raw_file <- file.path(output_path,
        glue::glue("keras-ws-{current}.rda")
    )
    save(history, model_raw, file = raw_file)


    # Plots and summaries ---------------------------------------------
    if (inherits(plot, "ggplot")) {
        p_path <- file.path(output_path,
            glue::glue("keras-gg-{current}.png")
        )
        ggplot2::ggsave(gg_file, plot, width = 8.3, height = 11.7)
    }

    invisible(TRUE)
}
