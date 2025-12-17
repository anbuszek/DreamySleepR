#' Bubble plot for fuzzy VIKOR results
#'
#' @param x object of class fuzzy_vikor_res
#' @param ... passed to plot
#' @importFrom graphics plot text
#' @export
#' @method plot fuzzy_vikor_res
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$results

  plot(
    df$TotalLoss, df$MaxLoss,
    cex = sqrt(pmax(df$Compromise, 0)) + 0.5,
    pch = 19,
    xlab = "TotalLoss",
    ylab = "MaxLoss",
    ...
  )

  text(df$TotalLoss, df$MaxLoss, labels = df$Alternative, pos = 3, cex = 0.8)
}
