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
      max_words = Inf,
      embedding_dim  = "300",
      maxlen = 1e3,
      data_path   = here::here("../../data/"),
      output_path = here::here("../../output/"),
      random_seed = 1234L,
      mixdb_name  = "mixdb_otiti_tagged.rds",
      verbose = TRUE,
      batch_size = 8, #16L, #
      epochs = 30L,

      loss      = "categorical_crossentropy",
      metrics   = "categorical_accuracy",
      optimizer = "adam",
      is_test   = TRUE
  )


  # Model definition ================================================
  run_name <- glue::glue("340-kemb23f128_k23f256_batch8_do05-30ep-{p}vali")
  architecture <- glue::glue("
      (filename: {run_name})

      [embedding + conv(2+3)x128] + batch_norm + maxp2 + dropout05 +
      (conv(2+3)x128) + batch_norm + dropout05 + global_max +
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


  # l1_conv2 --------------------------------------------------------
  l1_conv2 <- l1_embedding %>%
      layer_conv_1d(filters = 128, kernel_size = 2L,
          padding = "same",
          activation = "relu",
          name = "l1_conv2"
      )

  summary(l1_conv2)


  # l1_conv3 --------------------------------------------------------
  l1_conv3 <- l1_embedding %>%
      layer_conv_1d(filters = 128, kernel_size = 3L,
          padding = "same",
          activation = "relu",
          name = "l1_conv3"
      )

  summary(l1_conv3)


  # l1 --------------------------------------------------------------
  l1 <- layer_concatenate(
        list(l1_embedding, l1_conv2, l1_conv3),
        name = "l1"
      ) %>%
      layer_batch_normalization() %>%
      layer_max_pooling_1d() %>%
      layer_dropout(0.5)

  # Layer 2 =========================================================
  # l2_conv2 --------------------------------------------------------
  l2_conv2 <- l1 %>%
    layer_conv_1d(filters = 256, kernel_size = 2L,
          padding = "same",
          activation = "relu",
          name = "l2_conv2"
      )

  summary(l2_conv2)

  # l2_conv2 --------------------------------------------------------
  l2_conv3 <- l1 %>%
    layer_conv_1d(filters = 256, kernel_size = 3L,
          padding = "same",
          activation = "relu",
          name = "l2_conv3"
      )

  summary(l2_conv3)

  # l2 --------------------------------------------------------------
  l2 <- layer_concatenate(list(l2_conv2, l2_conv3), name = "l2") %>%
      layer_batch_normalization() %>%
      layer_global_max_pooling_1d() %>%
      layer_dropout(0.5)


  summary(l2)



  # output ----------------------------------------------------------
  output <- l2 %>%
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
          epochs     = params$epochs
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
    model, history, params, plot = p, start_time, id = run_name
  )
}
