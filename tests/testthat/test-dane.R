test_that("mcda_dane_surowe ma poprawne wymiary, kolumny i zakresy", {
  data("mcda_dane_surowe", package = "DreamySleepR")
  
  # Sprawdzenie wymiarów: 15 ekspertów * 3 alternatywy = 45 wierszy, 10 kolumn
  expect_equal(dim(mcda_dane_surowe), c(45, 10))
  
  # Sprawdzenie kolumn
  oczekiwane_kolumny <- c(
    "EkspertID", "Aplikacja",
    "dokladnosc_faz_snu", "analiza_statystyk", "ux_latwosc",
    "dane_fizjologiczne", "integracja_urzadzenia", "funkcje_dodatkowe",
    "zuzycie_baterii", "cena_subskrypcja"
  )
  expect_equal(colnames(mcda_dane_surowe), oczekiwane_kolumny)
  
  # Sprawdzenie czy wartości kryteriów są w zakresie 1 do 9
  for (kryt in oczekiwane_kolumny[3:10]) {
    expect_true(all(mcda_dane_surowe[[kryt]] >= 1 & mcda_dane_surowe[[kryt]] <= 9))
    expect_true(all(mcda_dane_surowe[[kryt]] == round(mcda_dane_surowe[[kryt]])))
  }
})
