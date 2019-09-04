# Packages ========================================================
library(limpido)
library(tidyverse)
library(keras)


# Parameters ======================================================
params <- setup_input_data(
    validation_len = 300L,
    max_words = Inf,
    embedding_dim  = "300",
    maxlen = 500L,
    data_path   = here::here("../../data/"),
    output_path = here::here("../../output/"),
    random_seed = sample.int(1e4, 1),
    mixdb_name  = "mixdb_otiti_tagged.rds",
    verbose = TRUE,
    batch_size = 8L,
    epochs = 100L,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam"
)


# Model definition ================================================
architecture <- glue::glue("
    fixed pedianet embedding +
    conv(2x256) +
    batch_norm + max_pool2 +
    flatten +
    fc128_relu +
    fc6_softmax
")

# model -----------------------------------------------------------
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
        padding = "same",
        activation = "relu",
        name = "l1_1gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    ) %>%
    layer_batch_normalization(name = "l1_batchnorm") %>%
    layer_max_pooling_1d(pool_size = 2L) %>%
    layer_flatten() %>%
    layer_dense(units = 128L, activation = "relu") %>%
    layer_dense(units = 6L, activation = "softmax")

model


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
        epochs     = params$epochs,
        # callbacks --------------------
        callbacks = list(
          callback_early_stopping("categorical_accuracy", patience = 10L)
        )
    )

train_time <- lubridate::now() - start_time
}

warnings()

# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(history, architecture, params, train_time)

save_all_models(
  model, history, params$output_path, plot = p, start_time
)
