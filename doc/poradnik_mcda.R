## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(DreamySleepR)
data("mcda_dane_surowe")
head(mcda_dane_surowe)

skladnia_modelu <- paste(
  "dokladnosc_faz_snu =~ dokladnosc_faz_snu;",
  "analiza_statystyk =~ analiza_statystyk;",
  "ux_latwosc =~ ux_latwosc;",
  "dane_fizjologiczne =~ dane_fizjologiczne;",
  "integracja_urzadzenia =~ integracja_urzadzenia;",
  "funkcje_dodatkowe =~ funkcje_dodatkowe;",
  "zuzycie_baterii =~ zuzycie_baterii;",
  "cena_subskrypcja =~ cena_subskrypcja"
)
macierz_rozmyta <- przygotuj_dane_mcda(
  dane = mcda_dane_surowe,
  skladnia = skladnia_modelu,
  kolumna_alternatyw = "Aplikacja"
)

print(macierz_rozmyta)
bwm_best <- c(
  1,  # dokladnosc_faz_snu
  3,  # analiza_statystyk
  4,  # ux_latwosc
  2,  # dane_fizjologiczne
  5,  # integracja_urzadzenia
  4,  # funkcje_dodatkowe
  7,  # zuzycie_baterii
  9   # cena_subskrypcja
)

bwm_worst <- c(
  9,  # dokladnosc_faz_snu
  7,  # analiza_statystyk
  6,  # ux_latwosc
  8,  # dane_fizjologiczne
  5,  # integracja_urzadzenia
  4,  # funkcje_dodatkowe
  3,  # zuzycie_baterii
  1   # cena_subskrypcja
)
typy <- c(
  "max", # dokladnosc_faz_snu
  "max", # analiza_statystyk
  "max", # ux_latwosc
  "max", # dane_fizjologiczne
  "max", # integracja_urzadzenia
  "max", # funkcje_dodatkowe
  "min", # zuzycie_baterii
  "min"  # cena_subskrypcja
)
wynik_vikor <- rozmyty_vikor(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy,
  bwm_najlepsze = bwm_best,
  bwm_najgorsze = bwm_worst
)

wynik_vikor$wyniki
wynik_topsis <- rozmyty_topsis(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy,
  bwm_najlepsze = bwm_best,
  bwm_najgorsze = bwm_worst
)

wynik_topsis$wyniki
wynik_waspas <- rozmyty_waspas(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy,
  bwm_najlepsze = bwm_best,
  bwm_najgorsze = bwm_worst
)

wynik_waspas$wyniki


meta_wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy,
  bwm_najlepsze = bwm_best,
  bwm_najgorsze = bwm_worst
)
# 9. Wyniki metod

knitr::kable(wynik_vikor$wyniki, caption = "Wyniki metody Fuzzy VIKOR")
knitr::kable(wynik_topsis$wyniki, caption = "Wyniki metody Fuzzy TOPSIS")
knitr::kable(wynik_waspas$wyniki, caption = "Wyniki metody Fuzzy WASPAS")
knitr::kable(
  meta_wynik$porownanie,
  caption = "Porównanie rankingów oraz wynik końcowego konsensusu"
)

