# Packages ========================================================
library(limpido)
library(tidyverse)
library(keras)
library(depigner)
start_bot_for_chat("otiti")
errors_to_telegram("otiti")

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
run_name <- "143-deep-bilstm"
architecture <- glue::glue("
    (filename: {run_name})

    embedding + dropout02 +
    [
      (|-> + gmax) +
      (conv2x128 + batch_norm + gmax + dropout02) +
      (conv3x64 + batch_norm + gmax + dropout02) +
      (conv5x32 + batch_norm + gmax + dropout02) +
      (
        bi-lstm128 w/seq + rec_dropout02 +
        bi-lstm64 + rec_dropout02 +
        batch_norm + dropout02
      )
    ] +
    fc256 + batch_norm + dropout05 +
    fc6_softmax

    (trainable/) parameters: (956.198/) 4.177.614
")

# Layer 1 =========================================================

# l1_input --------------------------------------------------------
l1_input <- layer_input(shape = c(params$maxlen))

l1_embedding <- l1_input %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_embedding"
    ) %>%
    layer_dropout(0.2)

summary(l1_embedding)


# # l2_emb
# l2_emb <- l1_embedding # %>%
#   # layer_global_max_pooling_1d(name = "l2_emb-gmax")
#
# summary(l2_emb)

# l2_conv2 ---------------------------------------------------------
l2_conv2 <- l1_embedding %>%
    layer_conv_1d(filters = 128, kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l2_conv2"
    ) %>%
    layer_batch_normalization() %>%
    # layer_global_max_pooling_1d(name = "l2_conv2-gmax") %>%
    layer_dropout(0.2, name = "l2_conv2-dout02")

summary(l2_conv2)

# l2_conv3 ---------------------------------------------------------
l2_conv3 <- l1_embedding %>%
    layer_conv_1d(filters = 64, kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l2_conv3"
    ) %>%
    layer_batch_normalization() %>%
    # layer_global_max_pooling_1d(name = "l2_conv3-gmax") %>%
    layer_dropout(0.2, name = "l2_conv3-dout02")

summary(l2_conv3)

# l2_conv3 ---------------------------------------------------------
l2_conv5 <- l1_embedding %>%
    layer_conv_1d(filters = 32, kernel_size = 5L,
        padding = "same",
        activation = "relu",
        name = "l2_conv5"
    ) %>%
    layer_batch_normalization() %>%
    # layer_global_max_pooling_1d(name = "l2_conv5-gmax") %>%
    layer_dropout(0.2, name = "l2_conv5-dout02")

summary(l2_conv5)


# l2 --------------------------------------------------------------
l2 <- layer_concatenate(
      list(l1_embedding, l2_conv2, l2_conv3, l2_conv5),
      name = "l2"
    ) %>%
  layer_max_pooling_1d()

summary(l2)


# l3_deep-rnn -----------------------------------------------------
l3_deep_rnn <- l2 %>%
    bidirectional(
        layer_lstm(
            units = 256L,
            return_sequences = TRUE,
            recurrent_dropout = 0.2
        ),
        name = "l3_bilstm-1"
    ) %>%
    bidirectional(
        layer_lstm(
            units = 256L,
            return_sequences = TRUE,
            recurrent_dropout = 0.2
        ),
        name = "l3_bilstm-2"
    ) %>%
    layer_batch_normalization(name = "l3_deep_rnn-batchnorm") %>%
    layer_max_pooling_1d() %>%
    layer_dropout(0.2, name = "l3_deep_rnn-dout02")

summary(l3_deep_rnn)



# l4_conv ---------------------------------------------------------

# l4_conv2 ---------------------------------------------------------
l4_conv2 <- l3_deep_rnn %>%
    layer_conv_1d(filters = 256, kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l4_conv2"
    ) %>%
    layer_batch_normalization() %>%
    layer_dropout(0.2, name = "l4_conv2-dout02")

summary(l4_conv2)

# l4_conv3 ---------------------------------------------------------
l4_conv3 <- l3_deep_rnn %>%
    layer_conv_1d(filters = 128, kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l4_conv3"
    ) %>%
    layer_batch_normalization() %>%
    layer_dropout(0.2, name = "l4_conv3-dout02")

summary(l4_conv3)

# l4_conv5 ---------------------------------------------------------
l4_conv5 <- l3_deep_rnn %>%
    layer_conv_1d(filters = 64, kernel_size = 5L,
        padding = "same",
        activation = "relu",
        name = "l4_conv5"
    ) %>%
    layer_batch_normalization() %>%
    layer_dropout(0.2, name = "l4_conv5-dout02")

summary(l4_conv5)

# l4_emb ----------------------------------------------------------
l4_emb <- l1_embedding %>%
  layer_max_pooling_1d(4)

summary(l4_emb)

# l4 --------------------------------------------------------------
l4 <- layer_concatenate(
      list(l4_emb, l4_conv2, l4_conv3, l4_conv5),
      name = "l4"
    ) %>%
  layer_global_max_pooling_1d()

summary(l4)


# l5_fc748 --------------------------------------------------------
l5_fc748 <- l4 %>%
    layer_dense(units = 748, activation = "relu", name = "l5_fc748") %>%
    layer_batch_normalization() %>%
    layer_dropout(0.5)

summary(l5_fc748)


# output ----------------------------------------------------------
output <- l5_fc748 %>%
    layer_dense(units = 6L, activation = "softmax", name = "out_fc")

summary(output)


# model -----------------------------------------------------------
model <- keras_model(l1_input, output)

summary(model)

# Model compile ===================================================
model %>%
    compile(
        loss      = params$loss,
        optimizer = params$optimizer,
        metrics   = params$metrics
    )


# dir_log <- file.path("~/2019-otiti/output/logs/", run_name)
# if (!dir.exists(dir_log)) dir.create(dir_log)

callbacks <- list(
    callback_early_stopping("categorical_accuracy", patience = 25L)
)

# Run =============================================================
{
# tensorboard(dir_log)
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
        callbacks = callbacks
    )

train_time <- lubridate::now() - start_time
}

glue::glue("{run_name}: base training finished") %>%
  send_to_telegram()


# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!

p <- plot(history) %>%
  gg_history(history, architecture, params, train_time)

send_to_telegram(p)

save_all_models(
  model, history, params, plot = p, start_time
)


# Fine tuning -----------------------------------------------------

model %>%
  unfreeze_weights()

model %>%
    compile(
        loss      = params$loss,
        optimizer = optimizer_adam(lr = 1e-5), #params$optimizer,
        metrics   = params$metrics
    )

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
        callbacks = callbacks
    )

train_time <- lubridate::now() - start_time
}

glue::glue("{run_name}: fine tuning finished") %>%
  send_to_telegram()

# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(
    history, architecture, params, train_time, fine_tuned = TRUE
  )

send_to_telegram(p)

save_all_models(
  model, history, params, plot = p, start_time, fine_tuned = TRUE
)
