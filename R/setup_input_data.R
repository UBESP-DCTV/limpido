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
#' @param mixdb_path (chr) the path to the RDS file containing [mixdb]
#'     with the training and the validation data.
#' @param verbose (lgl, default TRUE) should info on the progress
#'     displayed?
#' @param loss (chr, default "categorical_crossentropy") the loss
#'     function for the model
#' @param metrics (chr, default "categorical_accuracy") the metrics to
#'     estimate the performance
#' @param optimizer (chr, dafault "adam") the optimizer for the DL model
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
    embedding_dim  = 300L,
    maxlen = c("300", "100"),
    batch_size = 8L,
    epochs = 15L,
    data_path = here::here("../data/"),
    random_seed = sample.int(1e4, 1),
    mixdb_path = file.path(data_path, "mixdb_otiti_tagged.rds"),
    verbose = TRUE,
    loss      = "categorical_crossentropy",
    metrics   = "categorical_accuracy",
    optimizer = "adam"

) {

    maxlen <- match.arg(maxlen)
    # Setup -----------------------------------------------------------
    pb <- depigner::pb_len(8L)
    depigner::tick(pb, "setup")

    set.seed(random_seed)

    max_words <- min(
        max_words,
        10733L,                      # all words in the train-validation
        122607,                            # all words in the pretrained
        na.rm = TRUE
    )

    depigner::tick(pb, "read mixdb")
    mixdb_otiti_tagged <- readr::read_rds(mixdb_path)

    if (verbose) {
        token_per_case <- mixdb_otiti_tagged[["x"]] %>%
            purrr::map_int(length)

        ui_info("min token/observation: {ui_value(min(token_per_case))}")
        ui_info("IQ token/observation: {ui_value(quantile(token_per_case, 0.25))}")
        ui_info("mean token/observation: {ui_value(mean(token_per_case))}")
        ui_info("median token/observation: {ui_value(median(token_per_case))}")
        ui_info("IIIQ token/observation: {ui_value(quantile(token_per_case, 0.75))}")
        ui_info("p95 token/observation: {ui_value(quantile(token_per_case, 0.95))}")
        ui_info("p99 token/observation: {ui_value(quantile(token_per_case, 0.99))}")
        ui_info("max token/observation: {ui_value(max(token_per_case))}")
    }


    # Parameters ------------------------------------------------------
    depigner::tick(pb, "params")

    # number of training and validation samples
    sets <- attr(mixdb_otiti_tagged, "meta")$set
    sets_len <- table(sets)

    # remove some indeces if it is problematic
    admitted_cases_indeces  <- seq_len(sum(sets_len))

    all_validation_indeces <- which(sets == "validation")
    validation_indeces <- sample(all_validation_indeces, validation_len)
    train_indeces <- admitted_cases_indeces[-validation_indeces]



    # Training set ----------------------------------------------------
    depigner::tick(pb, "train set")

    train_x <- mixdb_otiti_tagged$x[train_indeces] %>%
        add_oov_when_greater_than(max_words)
    mean_train_len <- mean(purrr::map_int(train_x, length))
    train_x  <- train_x %>%
        keras::pad_sequences(
            maxlen, padding = "post", truncating = "post"
        )

    train_y <- as.integer(mixdb_otiti_tagged$y[train_indeces])
    names(train_y) <- rep("train", length(train_y))
    train_y <- keras::to_categorical(train_y - 1L)



    # Validation set --------------------------------------------------
    depigner::tick(pb, "validation set")

    validation_x <- mixdb_otiti_tagged$x[validation_indeces] %>%
        add_oov_when_grater_than(max_words)
    mean_validation_len <- mean(purrr::map_int(validation_x, length))
    validation_x <- validation_x %>%
        pad_sequences(maxlen,
        padding = "post", truncating = "post"
    )

    validation_y <- as.integer(mixdb_otiti_tagged$y[validation_indeces])
    names(validation_y) <- rep("validation", length(validation_y))
    validation_y <- keras::to_categorical(validation_y - 1L)
    n_class <- ncol(train_y)


    if (verbose) {
        tibble::tibble(
            class = c(train_y, validation_y),
            set = names(c(train_y, validation_y))
        ) %>%
            dplyr::group_by(set, class) %>%
            dplyr::summarise(n = dplyr::n()) %>%
            dplyr::mutate(p = n/sum(n)) %>%
            ggplot2::ggplot(aes(x = class, y = p, fill = set)) +
            ggplot2::geom_bar(
                stat = "identity",
                position = ggplot2::position_dodge()
            )
    }


    # Load pretrained -------------------------------------------------
    depigner::tick(pb, "load pretrained")

    pretrained_path <- switch(embedding_dim,
        "100" = file.path(data_path, "model_100.vec"),
        "300" = file.path(data_path, "fasttext_pretrained.vec")
    )
    fasttext_pretrained <- readr::read_lines(pretrained_path, skip = 1L)


    # Embedding vectors -----------------------------------------------
    depigner::tick(pb, "embedding vectors")

    values <- stringr::str_split(fasttext_pretrained, " ")
    word   <- character(1L)
    embeddings_index <- new.env(hash = TRUE, parent = emptyenv())

    for (i in seq_along(fasttext_pretrained)) {
        word <- values[[i]][[1L]]
        vctr <- as.double(values[[i]][-c(1L, embedding_dim + 2L)])
        stopifnot(length(vctr) == embedding_dim)

        embeddings_index[[word]] <- vctr
    }

    # Embedding matrix ------------------------------------------------
    depigner::tick(pb, "embedding matrix")

    words <- c(
      names(get_dictionary(mixdb_otiti_tagged)[seq_len(max_words)]),
      "__OOV__"
    )
    max_words <- max_words + 1L
    embedding_matrix <- array(0, c(length(words), embedding_dim))
    embedding_vector <- double(embedding_dim)

    for (i in seq_along(words)) {
        word <- words[[i]]
        embedding_vector <- embeddings_index[[word]]
        # ui_info(pryr::address(embedding_vector))
        # Words not found in the embedding index will be all zeros.
        if (is.null(embedding_vector)) next

        embedding_matrix[i, ] <- embedding_vector
    }

    list(
        train_x = train_x,
        train_y = train_y,
        validation_x = validation_x,
        validation_y = validation_y,
        embedding_matrix = list(embedding_matrix),
        mean_train_len = mean_train_len,
        mean_validation_len = mean_validation_len,
        pedia_dict_size = max(get_dictionary(mixdb_otiti_tagged)),
        corpus_dict_size = length(words),
        n_class = n_class,
        random_seed = random_seed,
        validation_len = validation_len,
        train_len = sets_len - validation_len,
        max_words = max_words,
        embedding_dim = embedding_dim,
        maxlen = maxlen,
        batch_size = batch_size,
        epochs = epochs,
        data_path = data_path,
        random_seed = random_seed,
        mixdb_path = mixdb_path,
        verbose = verbose,
        loss      = loss,
        metrics   = metrics,
        optimizer = optimizer

        )

}
