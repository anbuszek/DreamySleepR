#' @title Generowanie tabeli APA
#'
#' @description
#' Funkcja przeksztalca wyniki analizy MCDA
#' (TOPSIS, VIKOR, MULTIMOORA, meta-ranking)
#' w sformatowana tabele zgodna ze standardem APA.
#'
#' @param x Obiekt wynikowy z funkcji pakietu.
#' @param tytul Opcjonalny tytul tabeli.
#'
#' @return Obiekt klasy `flextable`.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}


#' @export
tabela_apa.rozmyty_topsis_wynik <- function(
    x,
    tytul = "Wyniki metody Fuzzy TOPSIS"
) {
  df <- x$wyniki
  
  names(df) <- c("Alternatywa", "D+", "D-", "Wynik (CC)", "Ranking")
  
  df$`D+` <- round(df$`D+`, 3)
  df$`D-` <- round(df$`D-`, 3)
  df$`Wynik (CC)` <- round(df$`Wynik (CC)`, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 1", tytul),
    note = "CC - wspolczynnik bliskosci (im wyzszy, tym lepiej)."
  )
}


#' @export
tabela_apa.rozmyty_vikor_wynik <- function(
    x,
    tytul = "Wyniki metody Fuzzy VIKOR"
) {
  df <- x$wyniki
  
  names(df) <- c("Alternatywa", "S", "R", "Q", "Ranking")
  
  df$S <- round(df$S, 3)
  df$R <- round(df$R, 3)
  df$Q <- round(df$Q, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 2", tytul),
    note = "S - uzytecznosc grupy, R - zal indywidualny, Q - indeks kompromisu (im mniejszy, tym lepiej)."
  )
}


#' @export
tabela_apa.rozmyty_multimoora_wynik <- function(
    x,
    tytul = "Wyniki metody Fuzzy MULTIMOORA"
) {
  df <- x$wyniki
  
  if (!is.data.frame(df)) {
    stop("Obiekt `x$wyniki` musi byc ramka danych.")
  }
  
  if (ncol(df) != 9) {
    stop("Dla `rozmyty_multimoora_wynik` oczekiwano 9 kolumn w `x$wyniki`.")
  }
  
  names(df) <- c(
    "Alternatywa",
    "Ratio System",
    "Ranking Ratio",
    "Reference Point",
    "Ranking Reference",
    "Full Multiplicative Form",
    "Ranking Full Multiplicative",
    "Suma Rang",
    "Ranking"
  )
  
  df$`Ratio System` <- round(df$`Ratio System`, 4)
  df$`Reference Point` <- round(df$`Reference Point`, 4)
  df$`Full Multiplicative Form` <- round(df$`Full Multiplicative Form`, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = "MULTIMOORA laczy trzy podejscia: Ratio System, Reference Point oraz Full Multiplicative Form."
  )
}


#' @export
tabela_apa.rozmyty_meta_ranking_wynik <- function(
    x,
    tytul = "Meta-ranking (konsensus)"
) {
  if (is.null(x$porownanie)) {
    stop("To nie jest obiekt meta-rankingu.")
  }
  
  df <- x$porownanie
  names(df) <- gsub("_", " ", names(df))
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 4", tytul),
    note = "Zestawienie rankingow TOPSIS, VIKOR, MULTIMOORA oraz rankingow zagregowanych."
  )
}
