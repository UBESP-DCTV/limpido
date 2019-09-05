library(limpido)
library(keras)

# Parameters ======================================================
params <- setup_input_data(
    validation_len = 300L,
    max_words = Inf,            # ~20% of the full dictionary (Pareto)
    embedding_dim  = "300",
    maxlen = 500L,
    data_path   = here::here("../../data/"),
    output_path = here::here("../../output/"),
    random_seed = 1234L, # sample.int(1e4, 1), #
    mixdb_name  = "mixdb_otiti_tagged.rds",
    verbose = TRUE,
    batch_size = 8L, #16L, #
    epochs = 300L,

    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam",
    is_test   = FALSE
)


input <- layer_input(shape = c(params$maxlen), name = "input")

embedding <- input %>%
  layer_embedding(
        input_dim = params$max_words,
        output_dim = params$embedding_dim,
        input_length = params$maxlen,
        trainable = FALSE,
        weights = params$embedding_matrix,
        name = "1gram_embedding"
    )

model <- keras_model(input, embedding)

all.equal(
    params$embedding_matrix[[1L]][1L, ],
    predict(model, matrix(0:499, nrow = 1L))[1L, 1L, ]
)
