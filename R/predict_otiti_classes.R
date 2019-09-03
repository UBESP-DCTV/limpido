#' predict otitis classes with metrics
#'
#' given a Keras model, and parameters as output of [setup_input_data],
#' it predicts the validation classes and compute some metrics of
#' interest
#'
#' @param .model a Keras model
#' @param params a list as output of [setup_input_data]
#'
#' @return a list with four slots:
#'   - vector of validation predictions
#'   - vactor of validation gold labels
#'   - list of metrices (crude and balanced accuracy, precision, recall,
#'       and  f1)
#'   - list of wrong classified information (classes predicted, and gold
#'       ones, and an extraction of wrong records from the full dataset)
#' @export
#'
#' @examples
predict_otiti_classes <- function(.model, params) {

    y <- as.factor(
        onehot_to_label(params$validation_y) - 1
    )
    # ui_info("str y: {str(y, 1)}")

    n <- length(y)
    # ui_info("len y: {n}")

    pred <- .model %>%
        stats::predict(params$validation_x)
    # ui_info("prediction on x: {str(pred, 1)}")


    pred_labels <- factor(
        onehot_to_label(pred) - 1,
        levels = levels(y)
    )
    # ui_info("pred labels: {str(pred_labels, 1)}")

    pred_prob <- pred[matrix(c(seq_len(n), pred_labels), n)] %>%
        purrr::set_names(paste0("label_", pred_labels))
    # ui_info("pred probs: {str(pred_prob, 1)}")

    conf_mtx <- table(pred_labels, y)
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

    are_wrongs <- pred_labels != y

    list(
        pred_prob  = pred_prob,
        original   = y,
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
            wrong_pred = pred_labels[are_wrongs],
            wrong_gold = y[are_wrongs],
            wrong_records = attr(params$mixdb_used, "meta")[
                params$validation_indeces[are_wrongs],
            ]
        )
    )

}
