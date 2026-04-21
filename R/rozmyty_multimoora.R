#' @title Rozmyty MULTIMOORA
#'
#' @description
#' Wyznacza ranking alternatyw metodą MULTIMOORA dla macierzy decyzyjnej.
#'
#' @param macierz_decyzyjna Macierz decyzyjna, gdzie wiersze oznaczają
#'   alternatywy, a kolumny kryteria.
#' @param typy_kryteriow Wektor typów kryteriów: \code{"max"} albo \code{"min"}.
#' @param wagi Opcjonalny wektor wag kryteriów.
#' @param bwm_najlepsze Opcjonalny wektor Best-to-Others dla BWM.
#' @param bwm_najgorsze Opcjonalny wektor Others-to-Worst dla BWM.
#' @param bwm_kryteria Opcjonalny wektor nazw kryteriów dla BWM.
#'
#' @return Obiekt klasy \code{"rozmyty_multimoora_wynik"}.
#' @export
rozmyty_multimoora <- function(macierz_decyzyjna,
                               typy_kryteriow,
                               wagi = NULL,
                               bwm_najlepsze = NULL,
                               bwm_najgorsze = NULL,
                               bwm_kryteria = NULL) {
  if (is.null(macierz_decyzyjna)) {
    stop("`macierz_decyzyjna` nie może być pusta.")
  }
  
  if (!is.matrix(macierz_decyzyjna) && !is.data.frame(macierz_decyzyjna)) {
    stop("`macierz_decyzyjna` musi być macierzą lub ramką danych.")
  }
  
  macierz_decyzyjna <- as.matrix(macierz_decyzyjna)
  storage.mode(macierz_decyzyjna) <- "numeric"
  
  if (ncol(macierz_decyzyjna) %% 3 != 0) {
    stop("Liczba kolumn `macierz_decyzyjna` musi być podzielna przez 3 (l, m, u dla każdego kryterium).")
  }
  
  liczba_kryteriow <- ncol(macierz_decyzyjna) / 3
  liczba_alternatyw <- nrow(macierz_decyzyjna)
  
  if (length(typy_kryteriow) != liczba_kryteriow) {
    stop("Długość `typy_kryteriow` musi być równa liczbie kryteriów.")
  }
  
  if (!all(typy_kryteriow %in% c("min", "max"))) {
    stop("`typy_kryteriow` może zawierać wyłącznie 'min' i 'max'.")
  }
  
  # Wagi
  if (is.null(wagi)) {
    if (!is.null(bwm_najlepsze) && !is.null(bwm_najgorsze)) {
      if (!exists("oblicz_wagi_bwm", mode = "function")) {
        stop("Nie znaleziono funkcji `oblicz_wagi_bwm()`. Podaj `wagi` ręcznie albo dodaj funkcję BWM.")
      }
      
      if (is.null(bwm_kryteria)) {
        stop("Jeśli używasz BWM, musisz podać `bwm_kryteria`.")
      }
      
      wynik_bwm <- oblicz_wagi_bwm(
        nazwy_kryteriow = bwm_kryteria,
        najlepsze_do_innych = bwm_najlepsze,
        inne_do_najgorszego = bwm_najgorsze
      )
      
      wagi <- wynik_bwm$wagi_kryteriow
    } else {
      if (!exists("oblicz_wagi_entropii", mode = "function")) {
        stop("Nie znaleziono funkcji `oblicz_wagi_entropii()`. Podaj `wagi` ręcznie albo dodaj funkcję entropii.")
      }
      
      wagi <- oblicz_wagi_entropii(macierz_decyzyjna)
    }
  }
  
  if (length(wagi) != liczba_kryteriow) {
    stop("Długość `wagi` musi być równa liczbie kryteriów.")
  }
  
  if (any(is.na(wagi)) || any(wagi < 0)) {
    stop("`wagi` muszą być nieujemne i nie mogą zawierać NA.")
  }
  
  if (sum(wagi) == 0) {
    stop("Suma wag nie może być równa 0.")
  }
  
  wagi <- wagi / sum(wagi)
  
  # Normalizacja wektorowa
  mianowniki <- sqrt(colSums(macierz_decyzyjna^2, na.rm = TRUE))
  mianowniki[mianowniki == 0] <- 1
  
  macierz_norm <- sweep(macierz_decyzyjna, 2, mianowniki, "/")
  
  # Ważenie
  macierz_wazona <- sweep(macierz_norm, 2, wagi, "*")
  
  idx_max <- which(typy_kryteriow == "max")
  idx_min <- which(typy_kryteriow == "min")
  
  # 1. Ratio System
  ratio_benefit <- if (length(idx_max) > 0) {
    rowSums(macierz_wazona[, idx_max, drop = FALSE])
  } else {
    rep(0, liczba_alternatyw)
  }
  
  ratio_cost <- if (length(idx_min) > 0) {
    rowSums(macierz_wazona[, idx_min, drop = FALSE])
  } else {
    rep(0, liczba_alternatyw)
  }
  
  ratio_system <- ratio_benefit - ratio_cost
  ranking_ratio <- rank(-ratio_system, ties.method = "first")
  
  # 2. Reference Point
  punkt_ref <- numeric(liczba_kryteriow)
  
  for (j in seq_len(liczba_kryteriow)) {
    if (typy_kryteriow[j] == "max") {
      punkt_ref[j] <- max(macierz_wazona[, j], na.rm = TRUE)
    } else {
      punkt_ref[j] <- min(macierz_wazona[, j], na.rm = TRUE)
    }
  }
  
  odchylenia <- matrix(0, nrow = liczba_alternatyw, ncol = liczba_kryteriow)
  
  for (j in seq_len(liczba_kryteriow)) {
    odchylenia[, j] <- abs(punkt_ref[j] - macierz_wazona[, j])
  }
  
  reference_point <- apply(odchylenia, 1, max)
  ranking_reference <- rank(reference_point, ties.method = "first")
  
  # 3. Full Multiplicative Form
  eps <- 1e-12
  macierz_log <- log(pmax(macierz_decyzyjna, eps))
  
  czesc_benefit <- if (length(idx_max) > 0) {
    rowSums(sweep(macierz_log[, idx_max, drop = FALSE], 2, wagi[idx_max], "*"))
  } else {
    rep(0, liczba_alternatyw)
  }
  
  czesc_cost <- if (length(idx_min) > 0) {
    rowSums(sweep(macierz_log[, idx_min, drop = FALSE], 2, wagi[idx_min], "*"))
  } else {
    rep(0, liczba_alternatyw)
  }
  
  full_multiplicative_form <- czesc_benefit - czesc_cost
  ranking_multiplicative <- rank(-full_multiplicative_form, ties.method = "first")
  
  # Ranking końcowy
  suma_rang <- ranking_ratio + ranking_reference + ranking_multiplicative
  ranking_koncowy <- rank(suma_rang, ties.method = "first")
  
  wyniki <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    RatioSystem = ratio_system,
    Ranking_Ratio = ranking_ratio,
    ReferencePoint = reference_point,
    Ranking_Reference = ranking_reference,
    FullMultiplicativeForm = full_multiplicative_form,
    Ranking_FullMultiplicative = ranking_multiplicative,
    Suma_Rang = suma_rang,
    Ranking = ranking_koncowy,
    stringsAsFactors = FALSE
  )
  
  wyniki <- wyniki[order(wyniki$Ranking), , drop = FALSE]
  rownames(wyniki) <- NULL
  
  wynik <- list(
    wyniki = wyniki,
    ratio_system = ratio_system,
    reference_point = reference_point,
    full_multiplicative_form = full_multiplicative_form,
    wagi = wagi
  )
  
  class(wynik) <- "rozmyty_multimoora_wynik"
  return(wynik)
}


#' @export
print.rozmyty_multimoora_wynik <- function(x, ...) {
  cat("Wynik metody Rozmyty MULTIMOORA\n")
  print(x$wyniki, row.names = FALSE)
  invisible(x)
  
  
}