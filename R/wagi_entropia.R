#' Obliczanie wag metodą Entropii Shannona
#'
#' @description
#' Wyznacza obiektywne wagi kryteriów na podstawie danych,
#' mierząc stopień rozproszenia wartości. Im większa zmienność,
#' tym wyższa waga.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna.
#'
#' @return Wektor numeryczny wag sumujący się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna) {
  if (!is.matrix(macierz_decyzyjna)) {
    stop("`macierz_decyzyjna` musi być macierzą.")
  }
  
  if (ncol(macierz_decyzyjna) %% 3 != 0) {
    stop("Liczba kolumn `macierz_decyzyjna` musi być podzielna przez 3.")
  }
  
  n_kolumn <- ncol(macierz_decyzyjna)
  macierz_ostra <- matrix(
    0,
    nrow = nrow(macierz_decyzyjna),
    ncol = n_kolumn / 3
  )
  
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    macierz_ostra[, k] <-
      (macierz_decyzyjna[, j] +
         4 * macierz_decyzyjna[, j + 1] +
         macierz_decyzyjna[, j + 2]) / 6
    k <- k + 1
  }
  
  sumy_kolumn <- colSums(macierz_ostra)
  sumy_kolumn[sumy_kolumn == 0] <- 1
  P <- sweep(macierz_ostra, 2, sumy_kolumn, "/")
  
  if (nrow(macierz_decyzyjna) <= 1) {
    return(rep(1 / ncol(P), ncol(P)))
  }
  
  k_const <- 1 / log(nrow(macierz_decyzyjna))
  E <- numeric(ncol(P))
  
  for (j in seq_len(ncol(P))) {
    p_vals <- P[, j]
    p_vals <- p_vals[p_vals > 0]
    
    if (length(p_vals) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p_vals * log(p_vals))
    }
  }
  
  d <- 1 - E
  
  if (sum(d) == 0) {
    return(rep(1 / length(d), length(d)))
  }
  
  w <- d / sum(d)
  return(w)
}