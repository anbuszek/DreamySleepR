#' @title Wewnętrzne asercje BWM

#' @description Funkcja pomocnicza do sprawdzania warunków logicznych.

#' @keywords internal

.wymus_bwm <- function(wyrazenie, komunikat) {

  if (!all(wyrazenie)) {

    stop(if (is.null(komunikat)) "Blad" else komunikat)

  }

}


#' @title Wewnętrzna walidacja danych

#' @description Sprawdza, czy wektory porównań mają sens (długość, zakres 1-9).

#' @keywords internal

.waliduj_dane_bwm <- function(najlepsze_do_innych, inne_do_najgorszego, nazwy_kryteriow) {

  .wymus_bwm(length(najlepsze_do_innych) > 1, "Długość wektorów porównań musi być > 1.")

  .wymus_bwm(length(najlepsze_do_innych) == length(inne_do_najgorszego), "Niezgodność długości wektorów.")

  .wymus_bwm(length(najlepsze_do_innych) == length(nazwy_kryteriow), "Liczba kryteriów nie zgadza się z wektorami ocen.")

  .wymus_bwm(1 %in% najlepsze_do_innych, "Wektor 'najlepsze_do_innych' musi zawierać wartość 1 (dla Najlepszego).")

  .wymus_bwm(1 %in% inne_do_najgorszego, "Wektor 'inne_do_najgorszego' musi zawierać wartość 1 (dla Najgorszego).")

  .wymus_bwm(all(najlepsze_do_innych >= 1 & najlepsze_do_innych <= 9), "Oceny muszą być z przedziału 1-9.")


  list(best_to_others = najlepsze_do_innych, others_to_worst = inne_do_najgorszego, criteria_names = nazwy_kryteriow)

}


#' @title Wewnętrzne sprawdzanie spójności

#' @keywords internal

.sprawdz_spojnosc <- function(model) {

  indeks_najgorszego <- match(1, model$others_to_worst)

  najlepszy_nad_najgorszym <- model$best_to_others[indeks_najgorszego]


  # Sprawdzenie idealnej spojnosci: a_bj * a_jw = a_bw

  list(

    jest_spojny = all(model$best_to_others * model$others_to_worst == najlepszy_nad_najgorszym),

    a_bw = najlepszy_nad_najgorszym

  )

}


#' @title Pomocnik budowania ograniczeń

#' @keywords internal

.dodaj_ograniczenie <- function(ograniczenia, nowe_ograniczenie) {

  idx <- length(ograniczenia) + 1

  ograniczenia[[idx]] <- nowe_ograniczenie

  list(ograniczenia = ograniczenia, dodano = TRUE)

}


#' Obliczanie wag metodą BWM

#'

#' @description Wyznacza optymalne wagi kryteriów metodą Best-Worst (BWM) przy użyciu

#' programowania liniowego. Minimalizuje wskaźnik niespójności (ksi).

#'

#' @param nazwy_kryteriow Wektor znakowy z nazwami kryteriów.
#' @param najlepsze_do_innych Wektor numeryczny (1-9). Preferencja Najlepszego kryterium nad innymi.

#' @param inne_do_najgorszego Wektor numeryczny (1-9). Preferencja innych kryteriów nad Najgorszym.

#' @return Lista zawierająca: wagi_kryteriow, wskaznik_spojnosci (CR) oraz wartość ksi.

#' @import Rglpk

#' @export

oblicz_wagi_bwm <- function(nazwy_kryteriow, najlepsze_do_innych, inne_do_najgorszego) {


  # 1. Walidacja i budowa modelu

  dane <- .waliduj_dane_bwm(najlepsze_do_innych, inne_do_najgorszego, nazwy_kryteriow)

  spojnosc <- .sprawdz_spojnosc(dane)


  n_zmiennych <- length(najlepsze_do_innych) + 1 # Wagi 👎 + zmienna ksi (1)

  indeks_ksi <- n_zmiennych


  # --- Budowanie macierzy ograniczen dla Programowania Liniowego ---


  # Ograniczenie 1: Suma wag musi wynosic 1 (w1 + w2 + ... + wn = 1)

  lhs_suma <- c(rep(1, n_zmiennych - 1), 0) # 0 przy ksi, bo ksi nie wchodzi do sumy wag

  ograniczenia <- list(

    list(lhs = lhs_suma, dir = "==", rhs = 1)

  )


  # Ograniczenia wynikajace z porownan: |w_b - a_bj * w_j| <= ksi

  # Przeksztalcamy wartosc bezwzgledna na dwie nierownosci liniowe

  indeks_najlepszego <- match(1, najlepsze_do_innych)


  for (j in seq_along(najlepsze_do_innych)) {

    if (j != indeks_najlepszego) {

      # Rownanie A: w_b - a_bj * w_j - ksi <= 0

      lhs1 <- rep(0, n_zmiennych)

      lhs1[indeks_najlepszego] <- 1

      lhs1[j] <- -najlepsze_do_innych[j]

      lhs1[indeks_ksi] <- -1 # odejmujemy ksi

      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs1, dir = "<=", rhs = 0))$ograniczenia


      # Rownanie B: -w_b + a_bj * w_j - ksi <= 0

      lhs2 <- lhs1 * -1

      lhs2[indeks_ksi] <- -1 # ksi zawsze odejmujemy (zawsze dążymy do minimalizacji bledu)

      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs2, dir = "<=", rhs = 0))$ograniczenia

    }

  }


  # Powtorzenie logiki dla wektora Inne-do-Najgorszego: |w_j - a_jw * w_w| <= ksi

  indeks_najgorszego <- match(1, inne_do_najgorszego)


  for (j in seq_along(inne_do_najgorszego)) {

    if (j != indeks_najgorszego) {

      # Rownanie A: w_j - a_jw * w_w - ksi <= 0

      lhs1 <- rep(0, n_zmiennych)

      lhs1[j] <- 1

      lhs1[indeks_najgorszego] <- -inne_do_najgorszego[j]

      lhs1[indeks_ksi] <- -1

      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs1, dir = "<=", rhs = 0))$ograniczenia


      # Rownanie B: -w_j + a_jw * w_w - ksi <= 0

      lhs2 <- lhs1 * -1

      lhs2[indeks_ksi] <- -1

      ograniczenia <- .dodaj_ograniczenie(ograniczenia, list(lhs = lhs2, dir = "<=", rhs = 0))$ograniczenia

    }

  }


  # 2. Konfiguracja Solvera (Rglpk)

  # Zamiana listy ograniczen na macierz

  macierz_lhs <- t(sapply(ograniczenia, function(x) x$lhs))

  wektor_dir <- sapply(ograniczenia, function(x) x$dir)

  wektor_rhs <- unlist(sapply(ograniczenia, function(x) x$rhs))


  # Funkcja celu: Minimalizujemy tylko ksi (ostatnia zmienna)

  cel <- rep(0, n_zmiennych)

  cel[indeks_ksi] <- 1


  # Rozwiazanie problemu

  wynik <- Rglpk::Rglpk_solve_LP(cel, macierz_lhs, wektor_dir, wektor_rhs, max = FALSE)


  # 3. Przetwarzanie wynikow

  wagi <- wynik$solution[1:(n_zmiennych - 1)]

  wartosc_ksi <- wynik$solution[n_zmiennych]


  # Tabela Indeksu Spójności (Consistency Index) dla skali 1-9 (Rezaei, 2015)

  tabela_ci <- c(0, 0.44, 1.0, 1.63, 2.30, 3.00, 3.73, 4.47, 5.23)


  # Pobieramy wartosc a_bw (Najlepszy do Najgorszego)

  idx_bw <- as.integer(spojnosc$a_bw)

  idx_bw <- ifelse(idx_bw > 9, 9, idx_bw) # Zabezpieczenie


  # Obliczenie Consistency Ratio (CR)

  cr <- wartosc_ksi / tabela_ci[idx_bw]

  if (idx_bw == 1) cr <- 0


  list(

    nazwy_kryteriow = nazwy_kryteriow,

    wagi_kryteriow = wagi,

    wskaznik_spojnosci = cr,

    ksi = wartosc_ksi

  )

}
