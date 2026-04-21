#' Personalizacja wag kryteriów według profilu użytkownika
#'
#' Funkcja modyfikuje bazowy wektor wag kryteriów na podstawie
#' wybranego profilu użytkownika, a następnie dokonuje ich normalizacji.
#' Pozwala to uwzględnić preferencje różnych typów użytkowników
#' w procesie wspomagania decyzji.
#'
#' @param wagi Numeryczny wektor wag kryteriów. Wektor musi posiadać nazwy
#' odpowiadające kryteriom decyzyjnym.
#' @param profil Nazwa profilu użytkownika (np. "zestresowany_student",
#' "student_analityczny", "student_oszczedny").
#'
#' @return Numeryczny wektor wag po modyfikacji i normalizacji.
#'
#' @examples
#' wagi <- c(
#'   dokladnosc_faz_snu = 0.2,
#'   analiza_statystyk = 0.15,
#'   ux_latwosc = 0.1,
#'   dane_fizjologiczne = 0.1,
#'   integracja_urzadzenia = 0.1,
#'   funkcje_dodatkowe = 0.1,
#'   zuzycie_baterii = 0.15,
#'   cena_subskrypcja = 0.1
#' )
#'
#' zastosuj_profil_uzytkownika(wagi, "zestresowany_student")
#'
#' @export
zastosuj_profil_uzytkownika <- function(wagi, profil) {
  if (is.null(names(wagi))) {
    stop("Wektor wag musi mieć nazwy kryteriów.")
  }
  
  if (!is.numeric(wagi)) {
    stop("Wektor wag musi być numeryczny.")
  }
  
  if (any(wagi < 0)) {
    stop("Wagi nie mogą być ujemne.")
  }
  
  if (sum(wagi) == 0) {
    stop("Suma wag nie może być równa 0.")
  }
  
  if (!is.character(profil) || length(profil) != 1) {
    stop("Argument 'profil' musi być pojedynczym napisem.")
  }
  
  wymagane_kryteria <- c(
    "dokladnosc_faz_snu",
    "analiza_statystyk",
    "ux_latwosc",
    "dane_fizjologiczne",
    "integracja_urzadzenia",
    "funkcje_dodatkowe",
    "zuzycie_baterii",
    "cena_subskrypcja"
  )
  
  brakujace <- setdiff(wymagane_kryteria, names(wagi))
  if (length(brakujace) > 0) {
    stop(
      paste(
        "Brakuje następujących kryteriów w wektorze wag:",
        paste(brakujace, collapse = ", ")
      )
    )
  }
  
  skorygowane_wagi <- wagi
  
  if (profil == "zestresowany_student") {
    skorygowane_wagi["analiza_statystyk"] <- skorygowane_wagi["analiza_statystyk"] * 1.20
    skorygowane_wagi["ux_latwosc"] <- skorygowane_wagi["ux_latwosc"] * 1.15
    skorygowane_wagi["dokladnosc_faz_snu"] <- skorygowane_wagi["dokladnosc_faz_snu"] * 1.10
    skorygowane_wagi["funkcje_dodatkowe"] <- skorygowane_wagi["funkcje_dodatkowe"] * 0.95
  } else if (profil == "student_analityczny") {
    skorygowane_wagi["dokladnosc_faz_snu"] <- skorygowane_wagi["dokladnosc_faz_snu"] * 1.20
    skorygowane_wagi["analiza_statystyk"] <- skorygowane_wagi["analiza_statystyk"] * 1.25
    skorygowane_wagi["dane_fizjologiczne"] <- skorygowane_wagi["dane_fizjologiczne"] * 1.15
    skorygowane_wagi["ux_latwosc"] <- skorygowane_wagi["ux_latwosc"] * 0.95
  } else if (profil == "student_oszczedny") {
    skorygowane_wagi["cena_subskrypcja"] <- skorygowane_wagi["cena_subskrypcja"] * 1.30
    skorygowane_wagi["zuzycie_baterii"] <- skorygowane_wagi["zuzycie_baterii"] * 1.15
    skorygowane_wagi["funkcje_dodatkowe"] <- skorygowane_wagi["funkcje_dodatkowe"] * 0.95
  } else if (profil == "student_minimalista") {
    skorygowane_wagi["ux_latwosc"] <- skorygowane_wagi["ux_latwosc"] * 1.25
    skorygowane_wagi["integracja_urzadzenia"] <- skorygowane_wagi["integracja_urzadzenia"] * 1.10
    skorygowane_wagi["funkcje_dodatkowe"] <- skorygowane_wagi["funkcje_dodatkowe"] * 0.85
    skorygowane_wagi["analiza_statystyk"] <- skorygowane_wagi["analiza_statystyk"] * 0.95
  } else if (profil == "student_technologiczny") {
    skorygowane_wagi["integracja_urzadzenia"] <- skorygowane_wagi["integracja_urzadzenia"] * 1.25
    skorygowane_wagi["dane_fizjologiczne"] <- skorygowane_wagi["dane_fizjologiczne"] * 1.15
    skorygowane_wagi["funkcje_dodatkowe"] <- skorygowane_wagi["funkcje_dodatkowe"] * 1.10
  } else {
    stop(
      paste0(
        "Nieznany profil. Dostępne profile to: ",
        "zestresowany_student, student_analityczny, student_oszczedny, ",
        "student_minimalista, student_technologiczny."
      )
    )
  }
  
  skorygowane_wagi <- skorygowane_wagi / sum(skorygowane_wagi)
  
  return(skorygowane_wagi)
}