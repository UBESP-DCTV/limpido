#' Setup for DL
#'
#' Construnct all the necesserary stuff for deep learning with Keras
#' and TensorFlow.
#'
#' @details
#'
#'     The validation set should come from the data distribution of
#'     the test set. The test is unseen but the validation set can be
#'     (and would) be useful for the tuning of the hyper parameter,
#'     i.e., to choose between different models. If the validation set
#'     has a considerable dimension it is not necessary to use all of
#'     it only for the validation but it could be usefull to incorporate
#'     a portion into the training set (AKA the train-validation set).
#'     The parameter `validation_len` allow to decide how meny
#'     observation should be kept for the proper validation only. All
#'     the observation in the given validation set that (randomly)
#'     exceede `validation_len` will be incorporate in the training set.
#'
#'     Embedding vectors are dense representation of the feature. Those
#'     vector can have been trained with different lengths. Pedianet
#'     DB has two pretrained embeddings produced with the
#'     FastText-skipgram procedure, one with vectors of length 100, and
#'     one with vectors of length 300 (see [pedianet_fasttext]).
#'
#'     Neural network have to receive an imput of known an dfixed
#'     dimension. `maxlen` define that dimension, i.e., every
#'     observation which has more then `maxlen` words will be truncated,
#'     and every observation which has less then `maxlen` words will be
#'     padded with 0s down or up to `maxlen` length.
#'
#'     If `is_test` is `TRUE`, the validation records not sampled like
#'     into `validation_len` will continued to be added to the training
#'     set as always (hence, if you want, for the final model, to merge
#'     all the train and test set, set `validation_len` to 0). Next,
#'     the full test set take the place of the training set. (the names
#'     does not change yet, and for teh moment you wil continue to see
#'     "validation" instead of "test" even in this situation.)
#'
#' @param validation_len (int, default = 300L) Number of example in the
#'     validation set (see Datails)
#' @param embedding_dim (int, default = 300L) Dimension of the embedding
#'     vectors (see Details)
#' @param max_words (int, default = Inf) Maximum number from the
#'     vocabulary to considera (Inf means all possible, i.e. the minimum
#'     between the number of words in the corpus and the number of words
#'     in the vocabulary, plus 1 for the "out of vocabulary" words)
#' @param data_path (chr) path to the folder in which find the data
#'     needed for the setup
#' @param output_path (chr) path to the folder in which to save to
#'     output objects
#' @param random_seed (int, default is random) seed for the random
#'     computation
#' @param maxlen (chr, defaul = 300L) Maximum number of words to
#'     considered for each recrods (see Details)
#' @param batch_size (int, default = 8L) number of samples passed to the
#'     learner each full step of learning.
#' @param epochs (int, default = 15L) At each step of learning a
#'     progressive `batch_size` of samples of the whole are used in by
#'     the learner. Every now the learner complete to see all the sample
#'     in the training data, they said it is passed one epoch. `epochs`
#'     is the number of time the learner will see the full trainng
#'     dataset for its training.
#' @param mixdb_name (chr) name (within `data_path` to the RDS file
#'     containing [mixdb] with the training and the validation data.
#' @param verbose (lgl, default TRUE) should info on the progress
#'     displayed?
#' @param loss (chr, default "categorical_crossentropy") the loss
#'     function for the model
#' @param metrics (chr, default "categorical_accuracy") the metrics to
#'     estimate the performance
#' @param optimizer (chr, dafault "adam") the optimizer for the DL model
#' @param is_test (lgl, default FALSE) is the training made for the
#'     final models to test (on the test data)?
#'
#' @return a named list including:
#'    - **train_x**: list of named integers representig the training set
#'    - **validation_x**: list of named integers representig the
#'      validation set
#'    - **train_y**: response variable for training (+ train-validation)
#'      set
#'    - **validation_y**: response variable for validation
#'      (- train-validation) set
#'    - **embedding_matrix**: a list containing the embedding matrix
#'      (`max_words` + 1 rows, `embedding_dim` columns)
#'    - **random_seed**: the seed used,
#'    - **n_class**: the number of classes,
#'    - **mean_train_len**: the mean length of a training document,
#'    - **mean_validation_len**: the mean length of a validation
#'      document,
#'    - **train_len**: number of observation in the trainin gset (note:
#'      the number of observation in the validation set is one of the
#'      paramenter passed to the function)
#'    - values of all the imput parameters
#' @export
setup_input_data <- function(
    validation_len = 300L,
    max_words = Inf,
    embedding_dim  = c("300", "100"),
    maxlen = 500L,
    batch_size = 8L,
    epochs = 15L,
    data_path = here::here("../../data/"),
    output_path = here::here("../../output/"),
    random_seed = sample.int(1e4, 1),
    mixdb_name = "mixdb_otiti_tagged.rds",
    verbose = TRUE,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam",
    is_test   = FALSE

) {

    embedding_dim <- match.arg(embedding_dim)


    # Setup -----------------------------------------------------------
    set.seed(random_seed)

    mixdb_path <- file.path(data_path, mixdb_name)
    mixdb_otiti_tagged <- readr::read_rds(mixdb_path)
    ui_done("mixdb otiti imported")

    max_words <- min(
        max_words,
        length(limpido:::get_dictionary(mixdb_otiti_tagged)),
        122607L,                            # all words in the pretrained
        na.rm = TRUE
    )


    # Parameters ------------------------------------------------------
    # number of training and validation samples
    sets <- attr(mixdb_otiti_tagged, "meta")$set
    sets_len <- table(sets)

    # remove some indeces if it is problematic
    admitted_cases_indeces  <- seq_len(sum(sets_len))

    all_train_indeces <- which(sets == "train")
    all_validation_indeces <- which(sets == "validation")
    all_test_indeces <- which(sets == "test")

    validation_indeces <- sample(all_validation_indeces, validation_len)

    train_vali_indeces <- setdiff(
        all_validation_indeces,
        validation_indeces
    )
    train_indeces <-  c(all_train_indeces, train_vali_indeces)
    train_indeces <- sample(train_indeces)

    if (is_test) {
        validation_indeces <- all_test_indeces
        ui_warn("Test set has taken the place of the validation set!!")
    }


    # Training set ----------------------------------------------------
    train_x <- mixdb_otiti_tagged$x[train_indeces] %>%
        limpido:::add_oov_when_greater_than(max_words)
    train_lens <- purrr::map_int(train_x, length)
    train_dist <- quantile(train_lens, c(.5, .75, .90, .95, .99))
    mean_train_len <- mean(train_lens)
    train_x  <- train_x %>%
        keras::pad_sequences(
            maxlen, padding = "post", truncating = "post",
            # 1:max_word sono gli indici delle parole,
            # max_word + 1L è l'indice di __OOV__
            # mettiamo max_word + 2L come indice di __PAD__, ovvero una
            # parola che non c'è!!
            value = max_words + 2L
        )

    train_y <- as.integer(mixdb_otiti_tagged$y[train_indeces]) - 1L
    names(train_y) <- rep("train", length(train_y))
    train_y <- keras::to_categorical(train_y)
    n_class <- ncol(train_y)
    ui_done("Training set ready")


    # Validation set --------------------------------------------------
    validation_x <- mixdb_otiti_tagged$x[validation_indeces] %>%
        limpido:::add_oov_when_greater_than(max_words)
    validation_lens <- purrr::map_int(validation_x, length)
    validation_dist <- quantile(validation_lens, c(.5, .75, .90, .95, .99))
    mean_validation_len <- mean(validation_lens)
    validation_x <- validation_x %>%
        keras::pad_sequences(maxlen,
        padding = "post", truncating = "post",
        value = max_words + 2L
    )

    validation_y <- as.integer(mixdb_otiti_tagged$y[validation_indeces]) - 1L
    names(validation_y) <- rep("validation", length(validation_y))
    validation_y <- keras::to_categorical(validation_y)
    ui_done("Validation set ready")

    # Load pretrained -------------------------------------------------
    pretrained_path <- switch(embedding_dim,
        "100" = file.path(data_path, "pedianet_fasttext_pretrained_100.vec"),
        "300" = file.path(data_path, "pedianet_fasttext_pretrained_300.vec")
    )
    fasttext_pretrained <- readr::read_lines(pretrained_path, skip = 1L)
    ui_done("Pretrained loaded")

    # Load embeddings -------------------------------------------------

    embedding_matrix_path <- switch(embedding_dim,
        "100" = file.path(data_path, "otiti_embedding_matrix_100.rds"),
        "300" = file.path(data_path, "otiti_embedding_matrix_300.rds")
    )
    embedding_matrix <- readr::read_rds(embedding_matrix_path)
    ui_done("Embedding matrix loaded")
    # __OOV__ è max_word + 1L, __PAD__ è max_word + 2L
    if (dim(embedding_matrix)[[1]] != (max_words + 2L)) {
        ui_todo("Adjusting embedding matrix to the new bounduaries...")
        embedding_matrix <- limpido:::embedding_mtrx(
            mixdb_otiti_tagged,
            fasttext_pretrained,
            embedding_dim,
            max_words
        )
        ui_done("Embedding matrix adjusted to the new bounduaries")
        saveRDS(embedding_matrix, embedding_matrix_path)
        ui_done("New embedding matrix stored")
    }

    if (is_test) {
        ui_warn(
            "Remember that now the 'validation' set is the test set!!"
        )
    }
    # Return ----------------------------------------------------------
    list(
        train_x = train_x - 1L,
        train_y = train_y,
        train_indeces = train_indeces,
        validation_x = validation_x - 1L,
        validation_y = validation_y,
        validation_indeces = validation_indeces,
        mixdb_used = mixdb_otiti_tagged,
        embedding_matrix = list(embedding_matrix),
        train_dist = train_dist,
        mean_train_len = mean_train_len,
        validation_dist = validation_dist,
        mean_validation_len = mean_validation_len,
        pedia_dict_size = length(fasttext_pretrained),
        corpus_dict_size = length(get_dictionary(mixdb_otiti_tagged)),
        n_class = n_class,
        random_seed = random_seed,
        validation_len = nrow(validation_x),
        train_len = nrow(train_x),
        max_words = max_words + 2L,
        embedding_dim = as.integer(embedding_dim),
        maxlen = maxlen,
        batch_size = batch_size,
        epochs = epochs,
        data_path = data_path,
        output_path = output_path,
        random_seed = random_seed,
        mixdb_name = mixdb_name,
        verbose = verbose,
        loss      = loss,
        metrics   = metrics,
        optimizer = optimizer,
        is_test = is_test
      )
}
