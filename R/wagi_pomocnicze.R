#' @title Wewnętrzny procesor wag
#'
#' @description
#' Decyduje, skąd pobrać wagi kryteriów: z wektora podanego ręcznie
#' albo z metody BWM.
#'
#' @keywords internal
.pobierz_finalne_wagi <- function(macierz,
                                  wagi = NULL,
                                  bwm_kryteria = NULL,
                                  bwm_najlepsze = NULL,
                                  bwm_najgorsze = NULL) {
  if (!is.matrix(macierz)) {
    stop("`macierz` musi być macierzą.")
  }
  
  if (ncol(macierz) %% 3 != 0) {
    stop("Liczba kolumn `macierz` musi być podzielna przez 3.")
  }
  
  n_kryteriow <- ncol(macierz) / 3
  
  if (!is.null(wagi)) {
    if (length(wagi) == n_kryteriow) {
      return(rep(wagi, each = 3))
    }
    
    if (length(wagi) == ncol(macierz)) {
      return(wagi)
    }
    
    stop(
      "Długość `wagi` musi być równa liczbie kryteriów ",
      "albo liczbie kolumn macierzy."
    )
  }
  
  if (!is.null(bwm_najlepsze) && !is.null(bwm_najgorsze)) {
    if (is.null(bwm_kryteria)) {
      if (!is.null(attr(macierz, "nazwy_kryteriow"))) {
        bwm_kryteria <- attr(macierz, "nazwy_kryteriow")
      } else {
        bwm_kryteria <- paste0("C", seq_len(n_kryteriow))
        message(
          "Nie znaleziono nazw kryteriów. ",
          "Używam domyślnych: ",
          paste(bwm_kryteria, collapse = ", ")
        )
      }
    }
    
    message("Obliczanie wag metodą BWM...")
    
    wynik_bwm <- oblicz_wagi_bwm(
      nazwy_kryteriow = bwm_kryteria,
      najlepsze_do_innych = bwm_najlepsze,
      inne_do_najgorszego = bwm_najgorsze
    )
    
    wagi_ostre <- wynik_bwm$wagi_kryteriow
    
    if (length(wagi_ostre) != n_kryteriow) {
      stop("Liczba wag z BWM nie zgadza się z liczbą kryteriów w macierzy.")
    }
    
    return(rep(wagi_ostre, each = 3))
  }
  
  stop("Musisz podać `wagi` albo parametry BWM.")
}