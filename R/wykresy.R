#' @title Wewnetrzny motyw graficzny
#'
#' @description Ujednolicony styl wykresow dla calego pakietu.
#'
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#90A4AE", high = "#2E7D32"),
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}


#' Mapa decyzyjna Fuzzy VIKOR
#'
#' @description
#' Wizualizacja typu cIPMA.
#' Os X: efektywnosc grupowa (odwrocone S).
#' Os Y: ryzyko / zal (R).
#' Wielkosc bąbla: sila kompromisu zależna od Q.
#'
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {
  df <- x$wyniki
  
  s_min <- min(df$Def_S)
  s_max <- max(df$Def_S)
  
  if (s_max == s_min) {
    df$Wydajnosc <- 50
  } else {
    df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min)) * 100
  }
  
  q_min <- min(df$Def_Q)
  q_max <- max(df$Def_Q)
  
  if (q_max == q_min) {
    q_inv <- rep(1, nrow(df))
  } else {
    q_inv <- 1 - ((df$Def_Q - q_min) / (q_max - q_min))
  }
  
  df$Rozmiar <- (q_inv + 0.1)^3
  
  srodek_perf <- median(df$Wydajnosc, na.rm = TRUE)
  srodek_ryzyko <- median(df$Def_R, na.rm = TRUE)
  
  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +
    annotate(
      "rect",
      xmin = srodek_perf,
      xmax = Inf,
      ymin = -Inf,
      ymax = srodek_ryzyko,
      fill = "#E8F5E9",
      alpha = 0.5
    ) +
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +
    annotate(
      "text",
      x = max(df$Wydajnosc),
      y = min(df$Def_R),
      label = "STABILNY LIDER",
      hjust = 1,
      vjust = 0,
      size = 3,
      fontface = "bold.italic",
      color = "darkgreen"
    ) +
    annotate(
      "text",
      x = min(df$Wydajnosc),
      y = max(df$Def_R),
      label = "UNIKAC",
      hjust = 0,
      vjust = 1,
      size = 3,
      fontface = "italic",
      color = "#B71C1C"
    ) +
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.5) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    labs(
      title = "Mapa decyzyjna Fuzzy VIKOR",
      subtitle = "Zielona strefa oznacza najlepszy kompromis.",
      x = "Indeks wydajnosci grupy (odwrocone S)",
      y = "Indeks ryzyka / zalu (R)",
      size = "Dominacja",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}


#' Mapa decyzyjna Fuzzy TOPSIS
#'
#' @description
#' Pokazuje odleglosc od rozwiazania idealnego.
#' Os X: dystans od antywzorca (D-).
#' Os Y: dystans do wzorca (D+).
#'
#' @param x Obiekt klasy `rozmyty_topsis_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_topsis_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (df$Wynik)^4
  
  cel_x <- max(df$D_minus) * 1.02
  cel_y <- min(df$D_plus) * 0.98
  
  df$OdlegloscWizualna <- sqrt((df$D_minus - cel_x)^2 + (df$D_plus - cel_y)^2)
  
  ggplot(df, aes(x = D_minus, y = D_plus)) +
    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey50") +
    geom_label(
      aes(
        x = (D_minus + cel_x) / 2,
        y = (D_plus + cel_y) / 2,
        label = sprintf("%.3f", OdlegloscWizualna)
      ),
      size = 2.5,
      color = "grey30",
      label.size = 0,
      alpha = 0.7
    ) +
    geom_point(aes(size = Rozmiar, fill = Wynik), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.6) +
    annotate("point", x = cel_x, y = cel_y, shape = 18, size = 6, color = "#FFD700") +
    annotate("text", x = cel_x, y = cel_y, label = "IDEAL", vjust = 2, size = 3.5, fontface = "bold") +
    labs(
      title = "Mapa decyzyjna Fuzzy TOPSIS",
      subtitle = "Linie przerywane pokazuja droge do rozwiazania idealnego.",
      x = "Dystans od antywzorca (D-)",
      y = "Dystans do wzorca (D+)",
      size = "Bliskosc^4",
      fill = "Wynik (CC)"
    ) +
    .motyw_mcda()
}


#' Mapa decyzyjna Fuzzy MULTIMOORA
#'
#' @description
#' Wizualizacja porownuje dwa glowne skladniki wyniku MULTIMOORA:
#' Ratio System oraz Full Multiplicative Form.
#' Wielkosc punktu zalezy od koncowej sumy rang,
#' a kolor od pozycji w rankingu koncowym.
#'
#' @param x Obiekt klasy `rozmyty_multimoora_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_multimoora_wynik <- function(x, ...) {
  df <- x$wyniki
  
  df$Rozmiar <- max(df$Suma_Rang) - df$Suma_Rang + 1
  
  ggplot(df, aes(x = RatioSystem, y = FullMultiplicativeForm)) +
    geom_point(aes(size = Rozmiar, fill = Ranking), shape = 21, color = "black", alpha = 0.85) +
    geom_text_repel(aes(label = Alternatywa), box.padding = 0.5) +
    labs(
      title = "Mapa decyzyjna Fuzzy MULTIMOORA",
      subtitle = "Wykres porownuje Ratio System i Full Multiplicative Form.",
      x = "Ratio System",
      y = "Full Multiplicative Form",
      size = "Pozycja",
      fill = "Ranking"
    ) +
    .motyw_mcda()
}


utils::globalVariables(
  c(
    "Alternatywa",
    "Def_Q",
    "Def_R",
    "Def_S",
    "D_minus",
    "D_plus",
    "FullMultiplicativeForm",
    "OdlegloscWizualna",
    "Ranking",
    "RatioSystem",
    "Rozmiar",
    "Suma_Rang",
    "Wydajnosc",
    "Wynik"
  )
)
