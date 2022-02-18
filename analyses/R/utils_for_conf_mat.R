conf_mat <- function(preds) {
    names(preds$pred_prob) %>%
        str_remove("\\D+") %>%
        as.integer() %>%
        as.factor() %>%
        table(pred = ., original = preds$original)
}

conf_mat_purged <- function(preds) {
    gold <- preds$original
    pred <- preds$pred_prob

    retain <- (!gold %in% c("4", "5")) &
        (!names(pred) %in% c("label_4", "label_5"))

    gold_purged <- gold[retain] %>%
        fct_drop()
    pred_purged <- preds$pred_prob[retain]

    conf_mat(
        list(pred_prob = pred_purged, original = gold_purged)
    )
}

conf_mat_purged(preds) %>%
    compute_f1_tab()


compute_f1_tab <- function(conf_mtx) {
    # conf_mtx <- table(a, truth)

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

    # are_wrongs <- a != truth


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
        bal_f1 = bal_f#,
        # wrongs = are_wrongs
    )
}

