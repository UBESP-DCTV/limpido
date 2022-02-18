# Packages ========================================================
library(limpido)
library(tidyverse)
library(keras)


# Parameters ======================================================
params <- setup_input_data(
    validation_len = 300L,
    max_words = Inf,
    embedding_dim  = "300",
    maxlen = 300L,
    data_path   = here::here("../../data/"),
    output_path = here::here("../../output/"),
    random_seed = sample.int(1e4, 1),
    mixdb_name  = "mixdb_otiti_tagged.rds",
    verbose = TRUE,
    batch_size = 8L,
    epochs = 30L,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam"
)


# Model definition ================================================
architecture <- glue::glue("
    fixed pedianet embedding +
    conv2x128_same_relu  + max_pool2 + batch_norm + dropout0.1 +
    conv2x256_same_relu  + max_pool2 + batch_norm + dropout0.1 +
    conv3x512_same_relu  + max_pool3 + batch_norm + dropout0.1 +
    conv5x1024_same_relu + max_pool5 + batch_norm + dropout0.1 +
    flatten +
    fc6_softmax
")

model <- keras_model_sequential() %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix
    ) %>%
    layer_conv_1d(
        filters = 128,
        kernel_size = 2L,
        strides = 1L,
        padding = "same",
        activation = "relu"
    ) %>%
    layer_max_pooling_1d(2L) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    layer_conv_1d(
        filters = 256,
        kernel_size = 2L,
        strides = 1L,
        padding = "same",
        activation = "relu"
    ) %>%
    layer_max_pooling_1d(2L) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    layer_conv_1d(
        filters = 512,
        kernel_size = 3L,
        strides = 1L,
        padding = "same",
        activation = "relu"
    ) %>%
    layer_max_pooling_1d(3L) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    layer_conv_1d(
        filters = 1024,
        kernel_size = 5L,
        strides = 1L,
        padding = "same",
        activation = "relu"
    ) %>%
    layer_max_pooling_1d(5L) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    # layer_global_max_pooling_1d() %>%
    layer_flatten() %>%
    # layer_dense(units = 32L, activation = "relu") %>%
    layer_dense(units = 6L, activation = "softmax")

summary(model)


# Model compile ===================================================
model %>%
    compile(
        loss      = params$loss,
        optimizer = params$optimizer,
        metrics   = params$metrics
    )

# Run =============================================================
{
start_time <- lubridate::now()

    history <- fit(model,
        # train ------------------------
        x = params$train_x,
        y = params$train_y,
        # validation -------------------
        validation_data = list(
            params$validation_x,
            params$validation_y
        ),
        # learning pace ----------------
        batch_size = params$batch_size,
        epochs     = params$epochs
    )

train_time <- lubridate::now() - start_time
}



# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(history, architecture, params, train_time)

save_all_models(
  model, history, params$output_path, plot = p, start_time
)
