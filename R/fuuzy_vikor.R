#' Process Weights for Fuzzy MCDM
#'
#' @description Internal helper to handle either explicit fuzzy weights or BWM calculation.
#' @keywords internal
.get_final_weights <- function(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst) {

  n_crit <- ncol(decision_mat) / 3

  if (!missing(weights)) {
    if (length(weights) != ncol(decision_mat)) {
      stop("Length of 'weights' must match the total columns in decision matrix (n * 3).")
    }
    return(weights)
  }

  if (!missing(bwm_criteria) && !missing(bwm_best) && !missing(bwm_worst)) {
    message("Weights not provided. Calculating using BWM...")
    bwm_res <- calculate_bwm_weights(bwm_criteria, bwm_best, bwm_worst)

    crisp_w <- bwm_res$criteriaWeights

    if (length(crisp_w) != n_crit) {
      stop("Calculated BWM weights do not match the number of criteria.")
    }

    fuzzy_weights <- rep(crisp_w, each = 3)
    return(fuzzy_weights)
  }

  stop("Provide either 'weights' or BWM parameters.")
}

#' Fuzzy VIKOR Method
#'
#' @description Implements the Fuzzy VIKOR MCDM technique.
#' @param decision_mat Matrix (m x 3n). Alternatives x fuzzy criteria.
#' @param criteria_types Character vector length n ("max" or "min").
#' @param v Numeric in [0,1]. Strategy weight.
#' @param weights Optional fuzzy weights (length 3n).
#' @param bwm_criteria Optional BWM criteria.
#' @param bwm_best Optional BWM best-to-others vector.
#' @param bwm_worst Optional BWM others-to-worst vector.
#' @return Object of class `fuzzy_vikor_res`.
#' @export
fuzzy_vikor <- function(decision_mat, criteria_types, v = 0.5,
                        weights, bwm_criteria, bwm_best, bwm_worst) {

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- rep(criteria_types, each = 3)

  pos_ideal <- ifelse(fuzzy_cb == "max",
                      apply(decision_mat, 2, max),
                      apply(decision_mat, 2, min))

  neg_ideal <- ifelse(fuzzy_cb == "min",
                      apply(decision_mat, 2, max),
                      apply(decision_mat, 2, min))

  d_mat <- matrix(0, nrow(decision_mat), n_cols)

  for (i in seq(1, n_cols, 3)) {
    denom <- pos_ideal[i+2] - neg_ideal[i]
    if (denom == 0) denom <- 1e-9

    if (fuzzy_cb[i] == "max") {
      d_mat[, i]   <- (pos_ideal[i]   - decision_mat[, i+2]) / denom
      d_mat[, i+1] <- (pos_ideal[i+1] - decision_mat[, i+1]) / denom
      d_mat[, i+2] <- (pos_ideal[i+2] - decision_mat[, i])   / denom
    } else {
      d_mat[, i]   <- (decision_mat[, i]   - pos_ideal[i+2]) / denom
      d_mat[, i+1] <- (decision_mat[, i+1] - pos_ideal[i+1]) / denom
      d_mat[, i+2] <- (decision_mat[, i+2] - pos_ideal[i])   / denom
    }
  }

  weighted_d <- d_mat %*% diag(final_w)

  S <- rowSums(weighted_d[, seq(1, n_cols, 3), drop=FALSE])
  R <- apply(weighted_d[, seq(1, n_cols, 3), drop=FALSE], 1, max)

  S_star <- min(S); S_minus <- max(S)
  R_star <- min(R); R_minus <- max(R)

  Q <- v * (S - S_star) / (S_minus - S_star) +
    (1 - v) * (R - R_star) / (R_minus - R_star)

  res <- data.frame(
    Alternative = 1:nrow(decision_mat),
    S = S,
    R = R,
    Q = Q,
    Ranking = rank(Q, ties.method = "first")
  )

  out <- list(results = res, params = list(v = v))
  class(out) <- "fuzzy_vikor_res"
  out
}
