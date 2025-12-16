#' Bubble plot for fuzzy VIKOR results
#'
#' @param x object of class fuzzy_vikor_res
#' @param ... passed to plot
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$results
  stopifnot(all(c("Def_S", "Def_R", "Def_Q") %in% names(df)))

  plot(
    df$Def_S, df$Def_R,
    cex = sqrt(pmax(df$Def_Q, 0)) + 0.5,
    pch = 19,
    xlab = "S (defuzzified)",
    ylab = "R (defuzzified)",
    ...
  )

  text(df$Def_S, df$Def_R, labels = df$Alternative, pos = 3, cex = 0.8)
}
