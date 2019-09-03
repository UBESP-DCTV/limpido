evaluators_metrics <- function(a, b = NULL, truth = NULL) {
    if (is.null(b) && is.null(truth))
        stop("At least one between b and truth must be provided")

    k    <- NULL
    f1_a <- NULL
    f1_b <- NULL

    if (!is.null(b)) {
        k <- psych::cohen.kappa(cbind(a, b))
    }

    if (!is.null(truth)) {
        f1_a <- compute_f1(a, truth)

        if (!is.null(b)) {
            f1_b <- compute_f1(b, truth)
        }
    }

    list(
        k = k,
        a = f1_a,
        b = f1_b
    )
}

compute_f1 <- function(a, truth) {
    conf_mtx <- table(a, truth)

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

    are_wrongs <- a != truth


    list(
        tp = tp,
        pred_pos = pred_pos,
        true_pos = pos,
        acc = crude_acc,
        precision = crude_prec,
        bal_prec = bal_prec,
        recall = crude_rec,
        bal_rec = bal_rec,
        f1 = crude_f,
        bal_f1 = bal_f,
        wrongs = are_wrongs
    )
}
