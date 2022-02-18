#' Create the embedding matrix
#'
#' @param mixdb_otiti_tagged the [mixdb] containing the original data
#' @param fasttext_pretrained (chr) the fasttext pretrained character vector
#' @param embedding_dim (chr or int) the feature dimension of the output embedding
#' @param max_words (int) maximum number of words considered
#'     (ranked by frequency)
#'
#' @return the embedding matrix
#' @export
embedding_mtrx <- function(
    mixdb_otiti_tagged, fasttext_pretrained, embedding_dim, max_words
) {
    embedding_dim <- as.integer(embedding_dim)
    # Embedding vectors -----------------------------------------------
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
    words <- c(
      names(get_dictionary(mixdb_otiti_tagged)[seq_len(max_words)]),
      "__OOV__", "__PAD__"
    )
    max_words <- max_words + 2L
    embedding_matrix <- array(0, c(length(words), embedding_dim))
    embedding_vector <- double(embedding_dim)

    pb <- depigner::pb_len(max_words)
    for (i in seq_along(words)) {
        depigner::tick(pb, "Embedding matrix")
        word <- words[[i]]
        embedding_vector <- embeddings_index[[word]]
        # ui_info(pryr::address(embedding_vector))
        # Words not found in the embedding index will be all zeros.
        if (is.null(embedding_vector)){
          # PAD resta esattamente zero
          if (word == "__PAD__") next
          # una parola sconosciuta (come tutte le OOV, e in teoria solo
          # loro...) vengono inizializzate a caso ma ocn un vettore
          # "piccolo"
          embedding_vector <- runif(embedding_dim, -0.1, 0.1)
        }

        embedding_matrix[i, ] <- embedding_vector
    }
    embedding_matrix
}
