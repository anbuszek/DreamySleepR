#' @title Teoria dominacji dla rankingu
#'
#' @description
#' Funkcja pomocnicza wyznaczająca ranking konsensusu na podstawie reguły
#' większości. Iteracyjnie sprawdza, która alternatywa najczęściej wygrywa
#' na danej pozycji w rankingach cząstkowych.
#'
#' @param r1 Wektor numeryczny rang metody 1.
#' @param r2 Wektor numeryczny rang metody 2.
#' @param r3 Wektor numeryczny rang metody 3.
#'
#' @return Wektor numeryczny zawierający końcowy ranking alternatyw.
#' @keywords internal
.oblicz_ranking_dominacji <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)
  
  for (obecna_pozycja in seq_len(n)) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf
    
    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])
    najlepszy_r3 <- which.min(obecna_macierz[, 3])
    
    kandydaci <- c(najlepszy_r1, najlepszy_r2, najlepszy_r3)
    tabela_czestosci <- table(kandydaci)
    
    zwyciezca_idx <- as.numeric(names(tabela_czestosci)[which.max(tabela_czestosci)])
    
    if (length(tabela_czestosci) == 3) {
      c1 <- najlepszy_r1
      c2 <- najlepszy_r2
      c3 <- najlepszy_r3
      
      c1_wygrane <- sum(macierz_rang[c1, ] < macierz_rang[c2, ]) +
        sum(macierz_rang[c1, ] < macierz_rang[c3, ])
      
      c2_wygrane <- sum(macierz_rang[c2, ] < macierz_rang[c1, ]) +
        sum(macierz_rang[c2, ] < macierz_rang[c3, ])
      
      c3_wygrane <- sum(macierz_rang[c3, ] < macierz_rang[c1, ]) +
        sum(macierz_rang[c3, ] < macierz_rang[c2, ])
      
      wygrane <- c(c1_wygrane, c2_wygrane, c3_wygrane)
      zwyciezca_idx <- c(c1, c2, c3)[which.max(wygrane)]
    }
    
    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] <- FALSE
  }
  
  finalny_ranking
}

#' @title Rozmyty metaranking alternatyw
#'
#' @description
#' Funkcja agreguje wyniki metod \emph{Fuzzy VIKOR}, \emph{Fuzzy TOPSIS}
#' oraz \emph{Fuzzy MULTIMOORA}, tworząc końcowy ranking konsensusu
#' alternatyw.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna zawierająca oceny
#'   alternatyw względem kryteriów.
#' @param typy_kryteriow Wektor znakowy określający typ kryteriów:
#'   \code{"min"} dla kryteriów kosztowych i \code{"max"} dla kryteriów
#'   zyskowych.
#' @param wagi Opcjonalny wektor wag kryteriów.
#' @param bwm_kryteria Opcjonalny wektor nazw kryteriów dla metody BWM.
#' @param bwm_najlepsze Opcjonalny wektor porównań Best-to-Others dla metody BWM.
#' @param bwm_najgorsze Opcjonalny wektor porównań Others-to-Worst dla metody BWM.
#' @param v Parametr metody VIKOR z zakresu od 0 do 1. Domyślnie \code{0.5}.
#'
#' @return Lista zawierająca dwa elementy:
#' \describe{
#'   \item{porownanie}{Ramka danych z rankingami cząstkowymi metod
#'   Fuzzy VIKOR, Fuzzy TOPSIS i Fuzzy MULTIMOORA oraz rankingami
#'   zagregowanymi.}
#'   \item{korelacje}{Macierz korelacji rang Spearmana pomiędzy rankingami
#'   cząstkowymi i zagregowanymi.}
#' }
#'
#' @details
#' Funkcja najpierw ustala wagi kryteriów. Jeśli użytkownik nie poda wag
#' ani parametrów BWM, wagi są wyznaczane metodą entropii. Następnie
#' uruchamiane są trzy metody: \code{rozmyty_vikor()},
#' \code{rozmyty_topsis()} oraz \code{rozmyty_multimoora()}.
#'
#' Rangi alternatyw są agregowane trzema sposobami:
#' \enumerate{
#'   \item ranking oparty na sumie rang cząstkowych,
#'   \item ranking wyznaczany zgodnie z teorią dominacji,
#'   \item ranking konsensusu wyznaczany z użyciem pakietu \pkg{RankAggreg}.
#' }
#'
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#'
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_kryteria = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 v = 0.5) {
  if (!is.matrix(macierz_decyzyjna)) {
    stop("`macierz_decyzyjna` musi być macierzą.")
  }
  
  liczba_kryteriow <- ncol(macierz_decyzyjna) / 3
  
  if (ncol(macierz_decyzyjna) %% 3 != 0) {
    stop("Liczba kolumn `macierz_decyzyjna` musi być podzielna przez 3.")
  }
  
  if (length(typy_kryteriow) != liczba_kryteriow) {
    stop("Długość `typy_kryteriow` musi być równa liczbie kryteriów.")
  }
  
  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag i parametrów BWM. Obliczam wagi metodą entropii...")
    wagi <- oblicz_wagi_entropii(macierz_decyzyjna)
  }
  
  args_baza <- list(
    macierz_decyzyjna = macierz_decyzyjna,
    typy_kryteriow = typy_kryteriow
  )
  
  if (!is.null(wagi)) {
    args_baza$wagi <- wagi
  }
  
  if (!is.null(bwm_najlepsze) && !is.null(bwm_najgorsze)) {
    if (is.null(bwm_kryteria)) {
      bwm_kryteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
    }
    
    if (is.null(bwm_kryteria)) {
      bwm_kryteria <- paste0("C", seq_len(liczba_kryteriow))
    }
    
    args_baza$bwm_kryteria <- bwm_kryteria
    args_baza$bwm_najlepsze <- bwm_najlepsze
    args_baza$bwm_najgorsze <- bwm_najgorsze
  }
  
  args_vikor <- c(args_baza, list(v = v))
  
  res_vikor <- do.call(rozmyty_vikor, args_vikor)
  res_topsis <- do.call(rozmyty_topsis, args_baza)
  res_multimoora <- do.call(rozmyty_multimoora, args_baza)
  
  alternatywy <- rownames(macierz_decyzyjna)
  
  r_vikor <- res_vikor$wyniki$Ranking[
    match(alternatywy, res_vikor$wyniki$Alternatywa)
  ]
  
  r_topsis <- res_topsis$wyniki$Ranking[
    match(alternatywy, res_topsis$wyniki$Alternatywa)
  ]
  
  r_multimoora <- res_multimoora$wyniki$Ranking[
    match(alternatywy, res_multimoora$wyniki$Alternatywa)
  ]
  
  if (is.null(r_vikor) || is.null(r_topsis) || is.null(r_multimoora)) {
    stop("Każda metoda musi zwracać kolumnę `Ranking` w elemencie `wyniki`.")
  }
  
  suma_pkt <- r_vikor + r_topsis + r_multimoora
  ranking_suma <- rank(suma_pkt, ties.method = "first")
  
  ranking_dominacja <- .oblicz_ranking_dominacji(
    r_vikor,
    r_topsis,
    r_multimoora
  )
  
  macierz_dla_ra <- rbind(
    order(r_vikor),
    order(r_topsis),
    order(r_multimoora)
  )
  
  n_alt <- nrow(macierz_decyzyjna)
  
  if (n_alt <= 10) {
    ra_wynik <- RankAggreg::BruteAggreg(
      x = macierz_dla_ra,
      k = n_alt,
      distance = "Spearman"
    )
  } else {
    ra_wynik <- RankAggreg::RankAggreg(
      x = macierz_dla_ra,
      k = n_alt,
      method = "GA",
      distance = "Spearman",
      verbose = FALSE
    )
  }
  
  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(n_alt)
  
  for (pozycja in seq_len(n_alt)) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }
  
  alternatywy <- rownames(macierz_decyzyjna)
  if (is.null(alternatywy)) {
    alternatywy <- paste0("A", seq_len(n_alt))
  }
  
  porownanie_df <- data.frame(
    Alternatywa = alternatywy,
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    R_MULTIMOORA = r_multimoora,
    Meta_Suma = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Agregacja = wektor_ra
  )
  
  macierz_kor <- stats::cor(porownanie_df[, -1], method = "spearman")
  
  wynik <- list(
    porownanie = porownanie_df,
    korelacje = macierz_kor
  )
  
  class(wynik) <- "rozmyty_meta_ranking_wynik"
  return(wynik)
}
