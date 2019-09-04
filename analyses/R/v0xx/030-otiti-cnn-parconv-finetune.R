# Packages ========================================================
library(limpido)
library(tidyverse)
library(keras)


# Parameters ======================================================
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
    conv(1+2+3)x( 32+ 64+ 32) + batch_norm + max_pool2 + dropout0.5 +
    conv(1+2+3)x( 64+128+ 53) + batch_norm + max_pool2 + dropout0.5 +
    conv(1+2+3)x(128+256+128) + batch_norm + max_pool2 + dropout0.5 +
    fc512_relu + batch_norm + dropout0.5 +
    fc6_softmax
")

# Layer 1 =========================================================

# l1_input --------------------------------------------------------
l1_input <- layer_input(shape = c(params$maxlen), name = "l1_input")

l1_embedding <- l1_input %>%
  layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_1gram_embedding"
    )

# 1gram -----------------------------------------------------------
l1_1gram <- l1_embedding %>%
  layer_conv_1d(
        filters = 32,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l1_1gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l1_1gram)

# 2gram -----------------------------------------------------------
l1_2gram <- l1_embedding %>%
  layer_conv_1d(
        filters = 64,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l1_2gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l1_2gram)


# 3gram -----------------------------------------------------------
l1_3gram <- l1_embedding %>%
  layer_conv_1d(
        filters = 32,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l1_3gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l1_3gram)



# l1 --------------------------------------------------------------

l1 <- layer_concatenate(
    list(l1_1gram, l1_2gram, l1_3gram),
    name = "l1"
) %>%
    layer_batch_normalization(name = "l1_batchnorm") %>%
    layer_max_pooling_1d(pool_size = 2L, name = "l1_maxpool") %>%
    layer_dropout(rate = 0.5, name = "l1_dropout")

summary(l1)


# Layer 2 =========================================================

# 1gram -----------------------------------------------------------
l2_1gram <- l1 %>%
  layer_conv_1d(
        filters = 64,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l2_1gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l2_1gram)

# 2gram -----------------------------------------------------------
l2_2gram <- l1 %>%
  layer_conv_1d(
        filters = 128,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l2_2gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l2_2gram)


# 3gram -----------------------------------------------------------
l2_3gram <- l1 %>%
  layer_conv_1d(
        filters = 64,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l2_3gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l2_3gram)



# l2 --------------------------------------------------------------

l2 <- layer_concatenate(
    list(l2_1gram, l2_2gram, l2_3gram),
    name = "l2"
) %>%
    layer_batch_normalization(name = "l2_batchnorm") %>%
    layer_max_pooling_1d(pool_size = 2L, name = "l2_maxpool") %>%
    layer_dropout(rate = 0.5, name = "l2_dropout")

summary(l2)



# Layer 3 =========================================================

# 1gram -----------------------------------------------------------
l3_1gram <- l2 %>%
  layer_conv_1d(
        filters = 128,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l3_1gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l3_1gram)

# 2gram -----------------------------------------------------------
l3_2gram <- l2 %>%
  layer_conv_1d(
        filters = 256,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l3_2gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l3_2gram)


# 3gram -----------------------------------------------------------
l3_3gram <- l2 %>%
  layer_conv_1d(
        filters = 128,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l3_3gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l3_3gram)



# l3 --------------------------------------------------------------

l3 <- layer_concatenate(
    list(l3_1gram, l3_2gram, l3_3gram),
    name = "l3"
) %>%
    layer_batch_normalization(name = "l3_batchnorm") %>%
    layer_max_pooling_1d(pool_size = 5L, name = "l3_maxpool") %>%
    layer_dropout(rate = 0.5, name = "l3_dropout")

summary(l3)


# output ----------------------------------------------------------
output <- l3 %>%
    layer_flatten() %>%
    layer_dense(units = 512, activation = "relu") %>%
    layer_batch_normalization(name = "out_batchnorm") %>%
    layer_dropout(rate = 0.5, name = "out_dropout") %>%
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
        epochs     = params$epochs,
        # callbacks --------------------
        callbacks = list(
          callback_early_stopping("categorical_accuracy", patience = 20L)
        )
    )

train_time <- lubridate::now() - start_time
}

# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(history, architecture, params, train_time)

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
        callbacks = list(
          callback_early_stopping("categorical_accuracy", patience = 10L)
        )
    )

train_time <- lubridate::now() - start_time
}


# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(
    history, architecture, params, train_time, fine_tuned = TRUE
  )

save_all_models(
  model, history, params, plot = p, start_time, fine_tuned = TRUE
)
