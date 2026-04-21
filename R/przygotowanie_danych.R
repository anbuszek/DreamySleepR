#' Parsowanie składni MCDA
#'
#' Funkcja pomocnicza interpretująca model zapisany jako ciąg znaków
#' w postaci np. `"Kryterium =~ zmienna1 + zmienna2"`.
#' Zwraca listę mapującą nazwy kryteriów na odpowiadające im zmienne.
#'
#' @param skladnia Ciąg znaków opisujący składnię modelu MCDA.
#'
#' @return Lista, w której nazwy elementów odpowiadają kryteriom,
#' a wartości są wektorami nazw zmiennych przypisanych do danego kryterium.
#'
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {
  czysta_skladnia <- gsub("\n", "", skladnia)
  linie <- strsplit(czysta_skladnia, ";")[[1]]
  mapowanie <- list()
  
  for (linia in linie) {
    if (trimws(linia) == "") {
      next
    }
    
    czesci <- strsplit(linia, "=~")[[1]]
    
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      mapowanie[[nazwa_kryterium]] <- elementy
    }
  }
  
  return(mapowanie)
}


#' Wewnętrzny skaler Saaty'ego
#'
#' Przekształca dowolną skalę, np. Likert 1-5 lub wartości ciągłe,
#' do skali Saaty'ego 1-9.
#'
#' @param wektor Wektor numeryczny.
#'
#' @return Wektor numeryczny przeskalowany do przedziału 1-9.
#'
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {
  if (any(wektor < 0, na.rm = TRUE)) {
    stop("Wykryto wartości ujemne w danych wejściowych.")
  }
  
  wektor[is.na(wektor) | wektor == 99] <- 0
  
  maska_poprawne <- wektor > 0
  wartosci <- wektor[maska_poprawne]
  
  if (length(wartosci) == 0) {
    return(wektor)
  }
  
  min_v <- min(wartosci)
  max_v <- max(wartosci)
  
  if (min_v == max_v) {
    wektor[maska_poprawne] <- 1
  } else {
    wektor[maska_poprawne] <- 1 + (wartosci - min_v) * (8 / (max_v - min_v))
  }
  
  return(wektor)
}


#' Wewnętrzna funkcja rozmywiająca
#'
#' Zamienia liczbę rzeczywistą na trójkątną liczbę rozmytą (TFN).
#' Dla wartości `x` tworzona jest trójka `(l, m, u)`, gdzie:
#' `l = x - 1`, `m = x`, `u = x + 1`,
#' z ograniczeniem do przedziału 1-9.
#'
#' @param wektor Wektor numeryczny.
#'
#' @return Macierz trójkolumnowa zawierająca wartości `l`, `m`, `u`.
#'
#' @keywords internal
.rozmyj_wektor <- function(wektor) {
  l <- pmax(1, wektor - 1)
  m <- wektor
  u <- pmin(9, wektor + 1)
  
  jest_zerem <- wektor == 0
  l[jest_zerem] <- 0
  m[jest_zerem] <- 0
  u[jest_zerem] <- 0
  
  return(cbind(l, m, u))
}


#' Przygotowanie danych do rozmytej analizy MCDA
#'
#' Funkcja przekształca surowe dane ankietowe w rozmytą macierz decyzyjną.
#' Oblicza wyniki zmiennych kompozytowych na podstawie składni,
#' skaluje je do przedziału 1-9, agreguje odpowiedzi ekspertów
#' i dokonuje fuzzification.
#'
#' @param dane Ramka danych zawierająca surowe zmienne.
#' @param skladnia Ciąg znaków definiujący kryteria,
#' np. `"Koszt =~ k1 + k2; Użyteczność =~ u1 + u2"`.
#' @param kolumna_alternatyw Nazwa kolumny identyfikującej alternatywy.
#' Jeśli `NULL`, każdy wiersz traktowany jest jako osobna alternatywa.
#' @param funkcja_agregacji Funkcja używana do agregacji opinii ekspertów.
#' Domyślnie `mean`.
#'
#' @return Macierz o wymiarach `m x 3n`, gdzie `m` oznacza liczbę alternatyw,
#' a `n` liczbę kryteriów.
#'
#' @export
przygotuj_dane_mcda <- function(dane, skladnia, kolumna_alternatyw = NULL, funkcja_agregacji = mean) {
  if (!is.data.frame(dane)) {
    stop("Argument 'dane' musi być ramką danych (data frame).")
  }
  
  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)
  
  tymczasowe_wyniki <- data.frame(row_id = seq_len(nrow(dane)))
  
  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]
    brakujace <- zmienne[!zmienne %in% names(dane)]
    
    if (length(brakujace) > 0) {
      stop(paste("Brakuje zmiennych w danych:", paste(brakujace, collapse = ", ")))
    }
    
    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_wynik <- dane[[zmienne]]
    }
    
    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)
  }
  
  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) {
      stop("Nie znaleziono kolumny alternatyw w danych.")
    }
    
    tymczasowe_wyniki$ID_Alternatywy <- dane[[kolumna_alternatyw]]
    
    dane_zagregowane <- aggregate(
      . ~ ID_Alternatywy,
      data = tymczasowe_wyniki[, -1, drop = FALSE],
      FUN = funkcja_agregacji
    )
    
    dane_zagregowane <- dane_zagregowane[order(dane_zagregowane$ID_Alternatywy), ]
    nazwy_wierszy <- dane_zagregowane$ID_Alternatywy
    macierz_wynikow <- as.matrix(dane_zagregowane[, nazwy_kryteriow, drop = FALSE])
  } else {
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow, drop = FALSE])
    nazwy_wierszy <- seq_len(nrow(macierz_wynikow))
  }
  
  lista_decyzyjna <- list()
  
  for (i in seq_along(nazwy_kryteriow)) {
    kryt <- nazwy_kryteriow[i]
    lista_decyzyjna[[kryt]] <- .rozmyj_wektor(macierz_wynikow[, i])
  }
  
  finalna_macierz <- do.call(cbind, lista_decyzyjna)
  rownames(finalna_macierz) <- nazwy_wierszy
  attr(finalna_macierz, "nazwy_kryteriow") <- nazwy_kryteriow
  
  return(finalna_macierz)
}