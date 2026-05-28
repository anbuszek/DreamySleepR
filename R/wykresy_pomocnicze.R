utils::globalVariables(c("x", "y", "grupa", "Waga", "Kryterium"))

.wykres_tfn <- function() {
  x_przyklady <- c(3, 5, 7)
  kolory <- c("#6366f1", "#8b5cf6", "#a78bfa")
  etykiety <- paste0("TFN(", x_przyklady, ")")

  dane_tfn <- data.frame()
  for (i in seq_along(x_przyklady)) {
    xv <- x_przyklady[i]
    x_pkt <- c(xv - 1, xv, xv + 1)
    y_pkt <- c(0, 1, 0)
    x_pelna <- seq(xv - 1, xv + 1, length.out = 50)
    y_pelna <- ifelse(x_pelna <= xv, (x_pelna - (xv - 1)), ((xv + 1) - x_pelna))
    dane_tfn <- rbind(
      dane_tfn,
      data.frame(
        x = x_pelna,
        y = y_pelna,
        grupa = etykiety[i]
      )
    )
  }

  dane_tfn$grupa <- factor(dane_tfn$grupa, levels = etykiety)

  ggplot2::ggplot(dane_tfn, ggplot2::aes(x = x, y = y, fill = grupa)) +
    ggplot2::geom_area(alpha = 0.4, position = "identity") +
    ggplot2::geom_line(ggplot2::aes(color = grupa), linewidth = 1.2) +
    ggplot2::scale_fill_manual(values = kolory) +
    ggplot2::scale_color_manual(values = kolory) +
    ggplot2::scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.1)) +
    ggplot2::labs(
      title = "Trojkatna liczba rozmyta (TFN)",
      x = "Ocena",
      y = "Stopien przynaleznosci",
      fill = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18),
      legend.position = "bottom"
    )
}

.wykres_bwm <- function(wynik_bwm) {
  df <- data.frame(
    Kryterium = wynik_bwm$nazwy_kryteriow,
    Waga = wynik_bwm$wagi_kryteriow
  )

  df$Kryterium <- factor(df$Kryterium, levels = rev(df$Kryterium))

  ggplot2::ggplot(df, ggplot2::aes(x = Kryterium, y = Waga)) +
    ggplot2::geom_col(fill = "#3b82f6", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = c(0, max(df$Waga) * 1.15)) +
    ggplot2::labs(
      title = "Wagi kryteriow BWM",
      x = NULL,
      y = "Waga"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18),
      axis.text.y = ggplot2::element_text(size = 12)
    )
}
