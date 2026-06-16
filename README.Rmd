# DreamySleepR

<!-- badges: start -->
<!-- badges: end -->

**DreamySleepR** to pakiet języka **R** przeznaczony do oceny i rankingu aplikacji monitorujących sen z wykorzystaniem metod wielokryterialnego wspomagania decyzji w środowisku rozmytym.

Pakiet wspiera cały proces analityczny – od przygotowania danych eksperckich, przez wyznaczanie wag kryteriów, aż po generowanie rankingów i wizualizację wyników. Został opracowany jako narzędzie badawcze do porównywania aplikacji wspomagających monitorowanie jakości snu.

------------------------------------------------------------------------

## Dlaczego DreamySleepR?

Na rynku dostępnych jest wiele aplikacji monitorujących sen, które różnią się funkcjonalnością, dokładnością pomiarów, sposobem prezentacji wyników oraz kosztami użytkowania. DreamySleepR umożliwia ich porównanie z wykorzystaniem metod MCDA, uwzględniających zarówno niepewność ocen ekspertów, jak i różne preferencje użytkowników.

------------------------------------------------------------------------

## Główne możliwości

- przygotowanie danych eksperckich do analiz MCDA,
- transformacja ocen do postaci trójkątnych liczb rozmytych (TFN),
- wyznaczanie wag kryteriów metodą **Best-Worst Method (BWM)**,
- wyznaczanie wag kryteriów metodą **entropii**,
- generowanie rankingów metodami:
  - **Fuzzy TOPSIS**,
  - **Fuzzy VIKOR**,
  - **Fuzzy MULTIMOORA**,
- agregacja wyników wielu metod w postaci meta-rankingu,
- personalizacja wag z wykorzystaniem profili użytkownika,
- wizualizacja wyników analiz,
- tworzenie tabel gotowych do raportowania.

------------------------------------------------------------------------

## Instalacja

Możesz zainstalować wersję deweloperską z GitHub:

```r
# install.packages("devtools")
devtools::install_github("anbuszek/DreamySleepR")
```

------------------------------------------------------------------------

## Dane przykładowe

Pakiet zawiera przykładowy zbiór danych przedstawiający oceny trzech aplikacji monitorujących sen wykonane przez grupę ekspertów.

```r
library(DreamySleepR)

data("mcda_dane_surowe")

head(mcda_dane_surowe)
```

------------------------------------------------------------------------

## Odtworzenie przykładowej analizy

```r
library(DreamySleepR)

data("mcda_dane_surowe")

dane_rozmyte <- przygotuj_dane_mcda(
  dane = mcda_dane_surowe,
  skladnia = "
    Dokladnosc =~ dokladnosc_faz_snu;
    Analiza =~ analiza_statystyk;
    UX =~ ux_latwosc;
    Fizjologia =~ dane_fizjologiczne;
    Integracja =~ integracja_urzadzenia;
    Funkcje =~ funkcje_dodatkowe;
    Bateria =~ zuzycie_baterii;
    Cena =~ cena_subskrypcja
  ",
  kolumna_alternatyw = 'Aplikacja'
)

macierz <- as.matrix(dane_rozmyte)

typy <- c(
  "max", "max", "max", "max",
  "max", "max", "min", "min"
)

wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy,
  bwm_kryteria = c(
    "Dokladnosc", "Analiza", "UX", "Fizjologia",
    "Integracja", "Funkcje", "Bateria", "Cena"
  ),
  bwm_najlepsze = c(3, 4, 2, 3, 2, 3, 5, 1),
  bwm_najgorsze = c(3, 2, 4, 3, 4, 3, 1, 5)
)

wynik$porownanie
```

------------------------------------------------------------------------

## Profile użytkowników

Pakiet umożliwia dostosowanie wag kryteriów do różnych preferencji użytkowników.

Dostępne profile:

- `student_oszczedny`
- `student_analityczny`
- `student_technologiczny`

Przykład:

```r
wagi_profilowe <- zastosuj_profil_uzytkownika(
  wagi = wynik$wagi,
  profil = "student_oszczedny"
)
```

------------------------------------------------------------------------

## Wizualizacja wyników

Wyniki analiz można prezentować za pomocą wbudowanych metod wizualizacji.

```r
plot(wynik)
```

------------------------------------------------------------------------

## Tabele wynikowe

Pakiet umożliwia generowanie tabel zgodnych ze stylem APA.

```r
tabela_apa(wynik)
```

------------------------------------------------------------------------

## Dokumentacja

Pełna dokumentacja pakietu dostępna jest pod adresem:

https://anbuszek.github.io/DreamySleepR/

Dodatkowo można uruchomić poradnik pakietu:

```r
vignette("poradnik_mcda", package = "DreamySleepR")
```

------------------------------------------------------------------------

## Autor

Anna Buszek

------------------------------------------------------------------------

## Licencja

GPL-3