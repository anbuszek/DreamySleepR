#' @export
plot.rozmyty_meta_ranking_wynik <- function(
    x,
    metoda = "Meta_Agregacja",
    ...
) {
  
  df <- x$porownanie
  
  if (!metoda %in% colnames(df)) {
    stop("Niepoprawna metoda meta-rankingu.")
  }
  
  df$Wynik <- df[[metoda]]
  
  df <- df[order(df$Wynik), ]
  
  df$Alternatywa <- factor(
    df$Alternatywa,
    levels = rev(df$Alternatywa)
  )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Alternatywa,
      y = Wynik,
      fill = Wynik
    )
  ) +
    
    ggplot2::geom_col() +
    
    ggplot2::coord_flip() +
    
    ggplot2::labs(
      title = "Meta-ranking aplikacji",
      subtitle = paste("Metoda:", metoda),
      x = "Alternatywa",
      y = "Pozycja rankingowa"
    ) +
    
    ggplot2::theme_minimal(base_size = 13) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 18
      ),
      
      legend.position = "none"
    )
}
