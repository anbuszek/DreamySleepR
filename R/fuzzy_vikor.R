# Internal helper: handle explicit fuzzy weights or BWM weights
.get_final_weights <- function(decision_matrix, weights,
                               bwm_criteria, bwm_best, bwm_worst) {

  n_crit <- ncol(decision_matrix) / 3

  if (!missing(weights)) {
    if (length(weights) != ncol(decision_matrix)) {
      stop("Length of 'weights' must match the total columns in decision matrix (n * 3).")
    }
    return(weights)
  }

  if (!missing(bwm_criteria) && !missing(bwm_best) && !missing(bwm_worst)) {
    bwm_res <- calculate_bwm_weights(bwm_criteria, bwm_best, bwm_worst)
    crisp_w <- bwm_res$criteriaWeights

    if (length(crisp_w) != n_crit) {
      stop("Calculated BWM weights do not match number of criteria.")
    }

    return(rep(crisp_w, each = 3))
  }

  stop("Provide either 'weights' or BWM parameters.")
}


#' Fuzzy VIKOR Method
#'
#' @description Implements the Fuzzy VIKOR MCDM technique.
#' @param decision_matrix Matrix (m x 3n). Alternatives x fuzzy criteria (triangular TFN).
#' @param criteria_direction Character vector length n: "max" (benefit) or "min" (cost).
#' @param strategy_weight Numeric in [0,1]. Weight for the strategy of maximum group utility.
#' @param weights Optional fuzzy weights (length 3n).
#' @param bwm_criteria Optional BWM criteria names (if weights missing).
#' @param bwm_best Optional BWM best-to-others vector.
#' @param bwm_worst Optional BWM others-to-worst vector.
#' @return Object of class `fuzzy_vikor_res`.
#' @export
fuzzy_vikor <- function(decision_matrix, criteria_direction, strategy_weight = 0.5,
                        weights, bwm_criteria, bwm_best, bwm_worst) {

  # --- Basic checks ---
  if (!is.matrix(decision_matrix)) stop("'decision_matrix' must be a matrix.")
  if (!is.numeric(strategy_weight) || length(strategy_weight) != 1 ||
      strategy_weight < 0 || strategy_weight > 1) {
    stop("'strategy_weight' must be a single number in [0, 1].")
  }

  n_crit <- ncol(decision_matrix) / 3
  if (length(criteria_direction) != n_crit) {
    stop("'criteria_direction' length must match number of criteria (ncol(decision_matrix) / 3).")
  }

  # --- Weights handling ---
  final_w <- .get_final_weights(decision_matrix, weights, bwm_criteria, bwm_best, bwm_worst)

  # --- Expand criteria direction to fuzzy columns ---
  n_cols <- ncol(decision_matrix)
  fuzzy_cb <- rep(criteria_direction, each = 3)

  # --- Ideal solutions ---
  pos_ideal <- ifelse(fuzzy_cb == "max",
                      apply(decision_matrix, 2, max),
                      apply(decision_matrix, 2, min))

  neg_ideal <- ifelse(fuzzy_cb == "min",
                      apply(decision_matrix, 2, max),
                      apply(decision_matrix, 2, min))

  # --- Linear normalization (distance matrix) ---
  d_mat <- matrix(0, nrow = nrow(decision_matrix), ncol = n_cols)

  for (i in seq(1, n_cols, 3)) {
    denom <- pos_ideal[i + 2] - neg_ideal[i]
    if (denom == 0) denom <- 1e-9

    if (fuzzy_cb[i] == "max") {
      d_mat[, i]   <- (pos_ideal[i]   - decision_matrix[, i + 2]) / denom
      d_mat[, i+1] <- (pos_ideal[i+1] - decision_matrix[, i + 1]) / denom
      d_mat[, i+2] <- (pos_ideal[i+2] - decision_matrix[, i])     / denom
    } else {
      d_mat[, i]   <- (decision_matrix[, i]     - pos_ideal[i + 2]) / denom
      d_mat[, i+1] <- (decision_matrix[, i + 1] - pos_ideal[i + 1]) / denom
      d_mat[, i+2] <- (decision_matrix[, i + 2] - pos_ideal[i])     / denom
    }
  }

  weighted_d <- d_mat %*% diag(final_w)

  # --- VIKOR indices (renamed for clarity/uniqueness) ---
  TotalLoss <- rowSums(weighted_d[, seq(1, n_cols, 3), drop = FALSE])
  MaxLoss   <- apply(weighted_d[, seq(1, n_cols, 3), drop = FALSE], 1, max)

  TotalLoss_best  <- min(TotalLoss)
  TotalLoss_worst <- max(TotalLoss)
  MaxLoss_best    <- min(MaxLoss)
  MaxLoss_worst   <- max(MaxLoss)

  denom_total <- (TotalLoss_worst - TotalLoss_best)
  denom_max   <- (MaxLoss_worst - MaxLoss_best)
  if (denom_total == 0) denom_total <- 1e-9
  if (denom_max == 0) denom_max <- 1e-9

  Compromise <- strategy_weight * (TotalLoss - TotalLoss_best) / denom_total +
    (1 - strategy_weight) * (MaxLoss - MaxLoss_best) / denom_max

  res <- data.frame(
    Alternative = 1:nrow(decision_matrix),
    TotalLoss = TotalLoss,
    MaxLoss = MaxLoss,
    Compromise = Compromise,
    Rank = rank(Compromise, ties.method = "first")
  )

  out <- list(
    results = res,
    params = list(strategy_weight = strategy_weight)
  )

  class(out) <- "fuzzy_vikor_res"
  return(out)
}
