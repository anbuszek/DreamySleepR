# DreamySleepR

<!-- badges: start -->
<!-- badges: end -->

**DreamySleepR** to pakiet w języku R służący do oceny i rankingu aplikacji do monitorowania snu dla studentów z wykorzystaniem metod wielokryterialnych w środowisku rozmytym (Fuzzy MCDA).

Pakiet stanowi narzędzie badawcze opracowane na potrzeby pracy licencjackiej pt.:

**„DreamySleepR: Pakiet R do oceny i rankingu aplikacji do monitorowania snu studentów z wykorzystaniem metod Fuzzy TOPSIS, Fuzzy VIKOR i Fuzzy MULTIMOORA.”**

Umożliwia on pełną ścieżkę analityczną: od danych eksperckich, przez ich agregację w postaci rozmytej, aż po wyznaczenie rankingów metodami **Fuzzy TOPSIS**, **Fuzzy VIKOR**, **Fuzzy MULTIMOORA** oraz ich **meta-rankingu (konsensusu)**.

---

## Funkcje pakietu

Pakiet **DreamySleepR** oferuje:

- przygotowanie i agregację **rozmytych ocen ekspertów**
- obsługę danych symulowanych dla:
  - 3 alternatyw (aplikacje do monitorowania snu)
  - 5 kryteriów oceny
  - 15 ekspertów
- wyznaczanie wag kryteriów metodą **entropii**
- możliwość wykorzystania wag wyznaczonych metodą **BWM**
- **rozmyty_topsis()** – ranking alternatyw metodą Fuzzy TOPSIS
- **rozmyty_vikor()** – ranking alternatyw metodą Fuzzy VIKOR
- **rozmyty_multimoora()** – ranking alternatyw metodą Fuzzy MULTIMOORA
- **rozmyty_meta_ranking()** – agregację wyników wielu metod w jeden ranking konsensusu
- **tabela_apa()** – generowanie tabel gotowych do raportowania
- **wizualizację wyników** rankingów i porównań

---

## Instalacja

Pakiet można zainstalować bezpośrednio z GitHuba:

```r
# install.packages("devtools")
devtools::install_github("anbuszek/DreamySleepR") 
``` 
## Szybki Start

Poniżej znajduje się podstawowy przykład użycia pakietu na danych symulowanych zgodnych z założeniami pracy:

- 3 alternatywy
- 5 kryteriów
- 15 ekspertów

```r
library(DreamySleepR)

# 1. Wczytaj dane
data("mcda_dane_surowe")
head(mcda_dane_surowe, 3)

# 2. Zdefiniuj skladnie danych
skladnia <- list(
  alternative = "App",
  expert = "Expert",
  criteria = c(
    "Accuracy",
    "Usability",
    "Compatibility",
    "Analytics",
    "Sleep_Awareness"
  )
)

# 3. Przygotuj rozmyta macierz decyzyjna
macierz <- przygotuj_dane_mcda(
  dane = mcda_dane_surowe,
  skladnia = skladnia,
  kolumna_alternatyw = "App",
  funkcja_agregacji = mean
)

# 4. Okresl typy kryteriow
typy_kryteriow <- c("max", "max", "max", "max", "max")

# 5. Wyznacz wagi kryteriow metoda entropii
wagi <- oblicz_wagi_entropii(macierz)

# 6. Uruchom metody rankingowe
wynik_topsis <- rozmyty_topsis(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi
)

wynik_vikor <- rozmyty_vikor(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi
)

wynik_multimoora <- rozmyty_multimoora(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi
)

# 7. Meta-ranking
wynik_meta <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi
)

# 8. Wyswietl wyniki
wynik_topsis$wyniki
wynik_vikor$wyniki
wynik_multimoora$wyniki
wynik_meta$porownanie 
```
## Wizualizacja

Pakiet umożliwia graficzną analizę wyników rankingu, na przykład dla metody TOPSIS:
```r
# Wizualizacja wyników (mapa decyzyjna)
plot(wynik_topsis)
```
## Meta-ranking

Wyniki z różnych metod można połączyć w ranking konsensusu, zwiększający stabilność decyzji:
```r
wynik_meta <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  wagi = wagi
)

wynik_meta$porownanie
wynik_meta$korelacje
```
## Raportowanie wyników

Pakiet pozwala przygotować tabele zgodne ze stylem APA:
```r
tabela_apa(wynik_topsis)
tabela_apa(wynik_vikor)
tabela_apa(wynik_multimoora)
tabela_apa(wynik_meta)
```
## Dokumentacja

Więcej informacji i przykładów:

Vignette:
vignette("dreamysleepr_guide", package = "DreamySleepR")

Pomoc dla funkcji:
```r
?rozmyty_topsis
?rozmyty_vikor
?rozmyty_multimoora
?rozmyty_meta_ranking
?oblicz_wagi_entropii
?tabela_apa
```
## Autor

Anna Buszek

## Licencja

GPL-3
