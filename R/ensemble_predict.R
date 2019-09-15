#' Ensemble predict
#'
#' @param models (lst) a list of (unserialized) models
#' @param params object params output stored in the training phase
#'
#' @return list of performances
#' @export
ensemble_predict <- function(models, params) {

    newx <- params$validation_x
    newy <- params$validation_y %>%
        limpido:::onehot_to_label() %>%
        `-`(1L)

    pred_prob <- purrr::map(models, stats::predict, newx)
    pred_labs <- (purrr::reduce(pred_prob, `+`) / length(models)) %>%
        limpido:::onehot_to_label() %>%
        `-`(1L)

    conf_mtx <- table(pred_labs, newy)

    tp <- diag(conf_mtx)
    pred_pos <- rowSums(conf_mtx, na.rm = TRUE)
    pos <- colSums(conf_mtx, na.rm = TRUE)

    crude_acc <- sum(tp, na.rm = TRUE) / sum(conf_mtx)

    crude_prec  <- sum(tp, na.rm = TRUE) / sum(pred_pos, na.rm = TRUE)
    bal_prec    <- mean(tp / pred_pos, na.rm = TRUE)

    crude_rec <- sum(tp, na.rm = TRUE) / sum(pos, na.rm = TRUE)
    bal_rec   <- mean(tp / pos, na.rm = TRUE)

    crude_f <- 2 * (crude_prec * crude_rec) / (crude_prec + crude_rec)
    bal_f   <- 2 * (bal_prec * bal_rec) / (bal_prec + bal_rec)

    are_wrongs <- pred_labels != newy


   list(
        pred_prob = pred_prob,
        pred_labs = pred_labs,
        original   = newy,
        metrics = list(
            crude_accuracy = crude_acc,
            crude_precision = crude_prec,
            balanced_precision = bal_prec,
            crude_recall = crude_rec,
            balanced_rec = bal_rec,
            crude_f = crude_f,
            balanced_f = bal_f
        ),
        wrongs = list(
            wrong_pred = pred_labs[are_wrongs],
            wrong_gold = y[are_wrongs],
            wrong_records = attr(params$mixdb_used, "meta")[
                params$validation_indeces[are_wrongs],
            ]
        )
    )

}
