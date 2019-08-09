#' Custom ggplot for keras' historys
#'
#' @param keras_plot a keras plot object
#' @param history a [keras]{history} object
#' @param architecture (chr) a description of the architecture used
#' @param params output of [setup_input_data]
#' @param train_time (POSIXct, dafault `lubridate::now()`) time to
#'    include in the plot.
#'
#' @return a [ggplot2]{ggplot} object
#' @export
gg_history <- function(
    keras_plot,
    history,
    architecture,
    params,
    train_time = lubridate::now()
) {

    notes_db <- as.data.frame(history) %>%
        dplyr::filter(epoch == 15) %>%
        dplyr::mutate_if(is.numeric, round, 4)

    keras_plot +
        ggplot2::xlab("Epoch") +
        ggplot2::ggtitle(
            "Loss and categorical accuracy by epochs for the Otiti's training and validation set.",
            subtitle = glue::glue(
                "Architecture: {architecture}\n",
                "Full exploration ({params$train_len + params$validation_len} observation overall)\n",
                "Training set: {params$train_len} (random seed: {params$random_seed})\n",
                "Average training sequence length: {round(params$mean_train_len, 2)}\n",
                "Validation set: {params$validation_len}\n",
                "Average validation sequence length: {round(params$mean_validation_len, 2)}\n",
                "Words in the embedding dictionary: {params$pedia_dict_size}\n",
                "Words in the corpus: {params$corpus_dict_size}\n",
                "Embedding (input, output): ({params$max_words} (padded/truncated at/from the end), {params$embedding_dim})\n",
                "Embedding-output layer size: {params$maxlen}\n",
                "Batch size: {params$batch_size}\n",
                "Epochs: {params$epochs}\n",
                "Mutually excluding classes: {params$n_class}\n",
                "Loss: {str_replace(params$loss, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
                "Optimizer: {str_replace(params$optimizer, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
                "Optimization metric: {str_replace(params$metrics, '[^a-zA-Z]', ' ') %>% str_to_title()}\n",
                "Overall time ellapsed (fit-only): {round(params$train_time, 2)} {attr(params$train_time, 'units')}"
            )
        ) +
        ggplot2::geom_text(data = notes_db, ggplot2::aes(
            x = epoch + 0.5, y = value + 0.04, label = 100 * value
        ))
}
