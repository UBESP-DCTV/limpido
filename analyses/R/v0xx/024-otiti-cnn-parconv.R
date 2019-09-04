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
    batch_size = 4L,
    epochs = 100L,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam"
)


# Model definition ================================================
architecture <- glue::glue("
    fixed pedianet embedding +
    conv(1+2+3+4)x128 + max_pool2 + batch_norm + dropout0.2 +
    conv(1+2+3+4)x256 + max_pool2 + batch_norm + dropout0.2 +
    conv(1+2+3+4)x512 + max_pool3 + batch_norm + dropout0.2 +
    fc6_softmax
")

# Layer 1 =========================================================

# l1_input --------------------------------------------------------
l1_input <- layer_input(shape = c(params$maxlen), name = "l1_input")

# 1gram -----------------------------------------------------------
l1_1gram <-  l1_input %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_1gram_embedding"
    ) %>%
    layer_conv_1d(
        filters = 128,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l1_1gram_conv",
        input_shape = c(params$maxlen, params$embedding_dim)
    )

summary(l1_1gram)

# 2gram -----------------------------------------------------------
l1_2gram <-  l1_input %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_2gram_embedding"
    ) %>%
    layer_conv_1d(
        filters = 128,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l1_2gram"
    )

summary(l1_2gram)


# 3gram -----------------------------------------------------------
l1_3gram <- l1_input %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_3gram_embedding"
    ) %>%
    layer_conv_1d(
        filters = 128,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l1_3gram"
    )

summary(l1_3gram)


# 4gram -----------------------------------------------------------
l1_4gram <- l1_input %>%
    layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "l1_4gram_embedding"
    ) %>%
    layer_conv_1d(
        filters = 128,
        kernel_size = 4L,
        padding = "same",
        activation = "relu",
        name = "l1_4gram"
    )

summary(l1_4gram)


# l1 --------------------------------------------------------------

l1 <- layer_concatenate(
    list(l1_1gram, l1_2gram, l1_3gram, l1_4gram),
    name = "l1"
) %>%
    layer_max_pooling_1d(pool_size = 2L, name = "l1_maxpool") %>%
    layer_batch_normalization(name = "l1_batchnorm") %>%
    layer_dropout(rate = 0.2, name = "l1_dropout")

summary(l1)


# Layer 2 =========================================================
# l2_1gram --------------------------------------------------------
l2_1gram <- l1 %>%
    layer_conv_1d(
        filters = 256,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l2_1gram"
    )
summary(l2_1gram)

# l2_2gram --------------------------------------------------------
l2_2gram <- l1 %>%
    layer_conv_1d(
        filters = 256,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l2_2gram"
    )
summary(l2_2gram)


# l2_3gram --------------------------------------------------------
l2_3gram <- l1 %>%
    layer_conv_1d(
        filters = 256,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l2_3gram"
    )
summary(l2_3gram)


# l2_4gram --------------------------------------------------------
l2_4gram <- l1 %>%
    layer_conv_1d(
        filters = 256,
        kernel_size = 4L,
        padding = "same",
        activation = "relu",
        name = "l2_4gram"
    )
summary(l2_4gram)


# l2 --------------------------------------------------------------

l2 <- layer_concatenate(
    list(l2_1gram, l2_2gram, l2_3gram, l2_4gram),
    name = "l2"
) %>%
    layer_max_pooling_1d(pool_size = 2L, name = "l2_maxpool") %>%
    layer_batch_normalization(name = "l2_batchnorm") %>%
    layer_dropout(rate = 0.2, name = "l2_dropout")

summary(l2)


# Layer 3 =========================================================
# l3_1gram --------------------------------------------------------
l3_1gram <- l2 %>%
    layer_conv_1d(
        filters = 512,
        kernel_size = 1L,
        padding = "same",
        activation = "relu",
        name = "l3_1gram"
    )
summary(l3_1gram)

# l3_2gram --------------------------------------------------------
l3_2gram <- l2 %>%
    layer_conv_1d(
        filters = 512,
        kernel_size = 2L,
        padding = "same",
        activation = "relu",
        name = "l3_2gram"
    )
summary(l3_2gram)


# l3_3gram --------------------------------------------------------
l3_3gram <- l2 %>%
    layer_conv_1d(
        filters = 512,
        kernel_size = 3L,
        padding = "same",
        activation = "relu",
        name = "l3_3gram"
    )
summary(l3_3gram)


# l3_4gram --------------------------------------------------------
l3_4gram <- l2 %>%
    layer_conv_1d(
        filters = 512,
        kernel_size = 4L,
        padding = "same",
        activation = "relu",
        name = "l3_4gram"
    )
summary(l3_4gram)


# l3 --------------------------------------------------------------

l3 <- layer_concatenate(
    list(l3_1gram, l3_2gram, l3_3gram, l3_4gram),
    name = "l3"
) %>%
    layer_max_pooling_1d(pool_size = 3L, name = "l3_maxpool") %>%
    layer_batch_normalization(name = "l3_batchnorm") %>%
    layer_dropout(rate = 0.2, name = "l3_dropout")

summary(l3)


















# output ----------------------------------------------------------
output <- l3 %>%
    layer_flatten() %>%
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



# Plot and save ===================================================
# plot(history) not in gg_history b/c keras'plot() is not exported!
p <- plot(history) %>%
  gg_history(history, architecture, params, train_time)

save_all_models(
  model, history, params$output_path, plot = p, start_time
)
