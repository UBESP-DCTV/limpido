# Packages ========================================================
library(limpido)
library(tidyverse)
library(keras)
library(depigner)
start_bot_for_chat("otiti")
errors_to_telegram("otiti")


p_vali <- seq(100, 0, by = -20)

for (p in p_vali) {
    vali_not_in_train <- 723 - round(723 * p/100)

  # Parameters ======================================================
  params <- setup_input_data(
      validation_len = vali_not_in_train,
      max_words = Inf,            # ~20% of the full dictionary (Pareto)
      embedding_dim  = "300",
      maxlen = 1e3,
      data_path   = here::here("../../data/"),
      output_path = here::here("../../output/"),
      random_seed = 1234L, # sample.int(1e4, 1), #
      mixdb_name  = "mixdb_otiti_tagged.rds",
      verbose = TRUE,
      batch_size = 8L,
      epochs = 203L,

      loss      = "categorical_crossentropy",
      metrics   = "categorical_accuracy",
      optimizer = "adam",
      is_test   = TRUE
  )


  # Model definition ================================================
  run_name <- glue::glue("310-cnn2_batch8_do05_f128-203e-{p}vali")
  architecture <- glue::glue("
      (filename: {run_name})

      fixed pedianet embedding + batch_norm +
      conv(2x128) + batch_norm + dropout05 +
      global_max +
      fc6_softmax
  ")
  send_to_telegram(glue::glue("start {run_name}"))

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
    layer_batch_normalization(name = "l1_batch-norm") %>%
    layer_dropout(0.2, name = "l1_dropout")


  # output ----------------------------------------------------------
  output <- l1_embedding %>%
      layer_conv_1d(
          filters = 128,
          kernel_size = 2L,
          padding = "same",
          activation = "relu",
          name = "l2_2gram_conv"
      ) %>%
      layer_batch_normalization() %>%
      layer_dropout(0.5) %>%
      layer_global_max_pooling_1d() %>%
      layer_dense(units = 6L, activation = "softmax")


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

  glue::glue("{run_name}: base training finished") %>%
    send_to_telegram()

  # Plot and save ===================================================
  p <- plot(history) %>%
    gg_history(history, architecture, params, train_time)

  send_to_telegram(p)

  save_all_models(
    model, history, params, plot = p, start_time, id = run_name
  )

}

