#' imdb_fasttext
#' https://github.com/rstudio/keras/blob/master/vignettes/examples/imdb_fasttext.R
#'
#' This example demonstrates the use of fasttext for text classification
#'
#' Based on Joulin et al’s paper: “Bags of Tricks for Efficient Text
#' Classification” https://arxiv.org/abs/1607.01759
#'
#' Results on IMDB datasets with uni and bi-gram embeddings: Uni-gram:
#' 0.8813 test accuracy after 5 epochs. 8s/epoch on i7 CPU Bi-gram :
#' 0.9056 test accuracy after 5 epochs. 2s/epoch on GTx 980M GPU
#'
library(data.table) # to load before of purr
library(purrr)
library(furrr) # to load after purrr
    no_cores <- availableCores() - 1
    plan(multicore, workers = no_cores)

library(keras) # to load after furrr
library(depigner)


# Function Definitions ----------------------------------------------------
tictoc::tic()
create_ngram_set <- function(input_list, ngram_value = 2L) {
    n_ngrams <- length(input_list) - ngram_value + 1L

    indices <- map(seq_len(n_ngrams), ~ .:(. + ngram_value - 1L))

    unique(map_chr(indices, ~ {
        paste0(input_list[.x], collapse = "|")
    }))

}

add_ngram <- function(sequences, token_indice, ngram_range = 2) {
    ngrams <- future_map(sequences, ~ create_ngram_set(.x, ngram_range))

    token_indice_token <- token_indice$token
    token_indice_ngrams <- token_indice$ngrams

    future_map2(sequences, ngrams, ~ {
        # this is the slowes part! (that's why data.table::`%chin%`())
        token_used_ideces <- token_indice_ngrams %chin% .y
        c(.x, token_indice_token[token_used_ideces])
    }, .progress = TRUE)
}


    # Parameters --------------------------------------------------------------

    # ngram_range = 2 will add bi-grams features
    ngram_range <- 1L
    max_features <- 20000L
    maxlen <- 400L
    batch_size <- 32L
    embedding_dims <- 50L
    epochs <- 5L


    # Data Preparation --------------------------------------------------------

    # Load data
    imdb_data <- dataset_imdb(num_words = max_features)

    # Train sequences
    train_len <- length(imdb_data$train$x)
    print(train_len)
    print(sprintf("Average train sequence length: %f", mean(map_int(imdb_data$train$x, length))))

    # Test sequences
    test_len <- length(imdb_data$test$x)
    print(test_len)
    print(sprintf("Average test sequence length: %f", mean(map_int(imdb_data$test$x, length))))

    if (ngram_range > 1L) {

        # Create set of unique n-gram from the training set.
        ngrams <- future_map(imdb_data$train$x, ~ {
                create_ngram_set(.x)
            }, .progress = TRUE) %>%
                unlist() %>%
                unique()

        # Dictionary mapping n-gram token to a unique integer
        # Integer values are greater than max_features in order
        # to avoid collision with existing features
        token_indice <- data.frame(
            ngrams = ngrams,
            token  = seq_along(ngrams) + max_features,
            stringsAsFactors = FALSE
        )

        # max_features is the highest integer that could be found in the dataset
        max_features <- max(token_indice$token) + 1L
        # Augmenting x_train and x_test with n-grams features
        imdb_data$train$x <- add_ngram(imdb_data$train$x, token_indice, ngram_range)
        imdb_data$test$x <- add_ngram(imdb_data$test$x, token_indice, ngram_range)
    }

    # Pad sequences
    imdb_data$train$x <- pad_sequences(imdb_data$train$x, maxlen = maxlen)
    imdb_data$test$x <- pad_sequences(imdb_data$test$x, maxlen = maxlen)


    # Model Definition --------------------------------------------------------

    model <- keras_model_sequential()

    model %>%
        layer_embedding(
            input_dim = max_features, output_dim = embedding_dims,
            input_length = maxlen
        ) %>%
        layer_global_average_pooling_1d() %>%
        layer_dense(1, activation = "sigmoid")

    model %>% compile(
        loss = "binary_crossentropy",
        optimizer = "adam",
        metrics = "accuracy"
    )


    # Fitting -----------------------------------------------------------------
        model %>% fit(
            imdb_data$train$x, imdb_data$train$y,
            batch_size = batch_size,
            epochs = epochs,
            validation_data = list(imdb_data$test$x, imdb_data$test$y)
        )
 tictoc::toc()

