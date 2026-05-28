#!/usr/bin/env Rscript

if (!file.exists("DESCRIPTION")) {
  stop("Run tools/render-site.R from the package root.", call. = FALSE)
}

if (!exists("przygotuj_dane_mcda", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    stop("Package must be loaded with pkgload or devtools before rendering the site.", call. = FALSE)
  }
}

escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  x
}

# --- 1. Load data and run analysis ---

data("mcda_dane_surowe")

skladnia <- "
  Dokladnosc =~ dokladnosc_faz_snu;
  Analiza =~ analiza_statystyk;
  UX =~ ux_latwosc;
  Fizjologia =~ dane_fizjologiczne;
  Integracja =~ integracja_urzadzenia;
  Funkcje =~ funkcje_dodatkowe;
  Bateria =~ zuzycie_baterii;
  Cena =~ cena_subskrypcja
"

kryteria <- c("Dokladnosc", "Analiza", "UX", "Fizjologia", "Integracja", "Funkcje", "Bateria", "Cena")
typy_kryteriow <- c("max", "max", "max", "max", "max", "max", "min", "min")

najlepsze_do_innych <- c(3, 4, 2, 3, 2, 3, 5, 1)
inne_do_najgorszego <- c(3, 2, 4, 3, 4, 3, 1, 5)

dane_rozmyte <- przygotuj_dane_mcda(
  dane = mcda_dane_surowe,
  skladnia = skladnia,
  kolumna_alternatyw = "Aplikacja"
)

macierz <- as.matrix(dane_rozmyte)

wynik_meta <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy_kryteriow,
  bwm_kryteria = kryteria,
  bwm_najlepsze = najlepsze_do_innych,
  bwm_najgorsze = inne_do_najgorszego
)

wynik_bwm <- oblicz_wagi_bwm(
  nazwy_kryteriow = kryteria,
  najlepsze_do_innych = najlepsze_do_innych,
  inne_do_najgorszego = inne_do_najgorszego
)

# --- 2. Generate and save plots to man/figures ---

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

p_tfn <- DreamySleepR:::.wykres_tfn()
ggplot2::ggsave("man/figures/README-tfn-1.png", plot = p_tfn, width = 8, height = 5, dpi = 150)

p_bwm <- DreamySleepR:::.wykres_bwm(wynik_bwm)
ggplot2::ggsave("man/figures/README-bwm-1.png", plot = p_bwm, width = 8, height = 5, dpi = 150)

p_meta <- plot(wynik_meta)
ggplot2::ggsave("man/figures/README-meta-1.png", plot = p_meta, width = 8, height = 5, dpi = 150)

# --- 3. Extract results ---

porownanie <- wynik_meta$porownanie

# Build meta-ranking table sorted by Meta_Agregacja
meta_sorted <- porownanie[order(porownanie$Meta_Agregacja), ]
meta_sorted$Pozycja <- seq_len(nrow(meta_sorted))

top_app <- as.character(meta_sorted$Alternatywa[1])

korelacje <- wynik_meta$korelacje
cor_values <- korelacje[upper.tri(korelacje)]
min_cor <- min(cor_values, na.rm = TRUE)
stabilnosc <- if (is.finite(min_cor) && min_cor >= 0.9) "Tak" else "Do sprawdzenia"

# Data table (first 6 rows of raw data, grouped by app)
dane_preview <- mcda_dane_surowe[order(mcda_dane_surowe$Aplikacja, mcda_dane_surowe$EkspertID), ]

# --- 4. Build HTML ---

render_data_row <- function(row) {
  paste0(
    "<tr>",
    "<td>", escape_html(row$EkspertID), "</td>",
    "<td>", escape_html(row$Aplikacja), "</td>",
    "<td>", escape_html(row$dokladnosc_faz_snu), "</td>",
    "<td>", escape_html(row$analiza_statystyk), "</td>",
    "<td>", escape_html(row$ux_latwosc), "</td>",
    "<td>", escape_html(row$dane_fizjologiczne), "</td>",
    "<td>", escape_html(row$integracja_urzadzenia), "</td>",
    "<td>", escape_html(row$funkcje_dodatkowe), "</td>",
    "<td>", escape_html(row$zuzycie_baterii), "</td>",
    "<td>", escape_html(row$cena_subskrypcja), "</td>",
    "</tr>"
  )
}

data_rows <- lapply(seq_len(nrow(dane_preview)), function(i) {
  render_data_row(dane_preview[i, ])
})
data_rows_html <- paste(unlist(data_rows), collapse = "\n              ")

render_meta_row <- function(row) {
  paste0(
    "<tr>",
    "<td>", escape_html(row$Pozycja), "</td>",
    "<td>", escape_html(row$Alternatywa), "</td>",
    "<td>", escape_html(row$R_TOPSIS), "</td>",
    "<td>", escape_html(row$R_VIKOR), "</td>",
    "<td>", escape_html(row$R_MULTIMOORA), "</td>",
    "</tr>"
  )
}

meta_rows <- lapply(seq_len(nrow(meta_sorted)), function(i) {
  render_meta_row(meta_sorted[i, ])
})
meta_rows_html <- paste(unlist(meta_rows), collapse = "\n            ")

wagi_labels <- paste0(
  '<div class="info-box"><h4>', escape_html(kryteria), '</h4><p>',
  formatC(wynik_bwm$wagi_kryteriow * 100, format = "f", digits = 1), '%</p></div>'
)
wagi_html <- paste(wagi_labels, collapse = "\n            ")

site_html <- paste0(
'<!DOCTYPE html>
<html lang="pl">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>

  <title>DreamySleepR</title>

  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">

  <style>
  
  *{
    margin:0;
    padding:0;
    box-sizing:border-box;
  }
  html{
    scroll-behavior:smooth;
  }
  
  body{
    font-family:\'Inter\', sans-serif;
    background:#f5f3ff;
    color:#1f2937;
    line-height:1.7;
  }
  
  .navbar{
    width:100%;
    height:72px;
    background:white;
    border-bottom:1px solid #d1d5db;
    display:flex;
    align-items:center;
    justify-content:space-between;
    padding:0 42px;
    position:sticky;
    top:0;
    z-index:100;
  }
  
  .logo{
    display:flex;
    align-items:center;
    gap:14px;
  }
  
  .logo-box{
    width:40px;
    height:40px;
    background:linear-gradient(135deg, #7c3aed, #4f46e5);
    border-radius:8px;
    display:flex;
    align-items:center;
    justify-content:center;
    color:white;
    font-weight:800;
    font-size:15px;
  }
  
  .logo-text h1{
    font-size:18px;
    font-weight:800;
    color:#111827;
  }
  
  .logo-text p{
    font-size:12px;
    color:#6b7280;
    margin-top:-3px;
  }
  
  .menu{
    display:flex;
    gap:30px;
    color:#374151;
    font-size:14px;
    font-weight:500;
  }
  .menu a{
    color:#374151;
    text-decoration:none;
    font-size:14px;
    font-weight:500;
    transition:0.2s;
  }
  
  .menu a:hover{
    color:#7c3aed;
  }
  
  .container{
    width:1400px;
    max-width:95%;
    margin:40px auto;
  }
  
  .grid{
    display:grid;
    grid-template-columns:1fr 1fr;
    gap:24px;
    margin-bottom:24px;
    align-items:stretch;
  }
  
  .two-cols{
    display:grid;
    grid-template-columns:1fr 1fr;
    gap:24px;
    margin-bottom:24px;
    align-items:stretch;
  }
  
  .section{
    height:100%;
    background:white;
    border:1px solid #d1d5db;
    padding:34px;
    overflow:hidden;
    border-radius:18px;
    box-shadow: 0 10px 30px rgba(79,70,229,0.06);
  }
  
  .section-label{
    font-size:11px;
    font-weight:700;
    letter-spacing:2px;
    color:#7c3aed;
    margin-bottom:18px;
  }
  .section:hover{
    transform:translateY(-2px);
    transition:0.25s;
    box-shadow: 0 20px 40px rgba(79,70,229,0.12);
  }
  
  h2{
    font-size:52px;
    line-height:1.1;
    font-weight:800;
    margin-bottom:28px;
    color:#111827;
  }
  
  h3{
    font-size:32px;
    font-weight:700;
    margin-bottom:22px;
    color:#111827;
  }
  
  p{
    font-size:16px;
    color:#374151;
    margin-bottom:18px;
  }
  
  .info-boxes{
    display:grid;
    grid-template-columns:repeat(3,1fr);
    gap:18px;
    margin-top:32px;
  }
  
  .info-box{
    border-radius:14px;
    background:#faf5ff;
    padding:18px;
  }
  
  .info-box h4{
    font-size:12px;
    text-transform:uppercase;
    letter-spacing:1px;
    color:#374151;
    margin-bottom:10px;
  }
  
  .info-box p{
    font-size:14px;
    margin-bottom:0;
    border-radius:14px;
    background:#faf5ff;
  }
  
  .questions ol{
    padding-left:20px;
  }
  
  .questions li{
    margin-bottom:14px;
    font-size:15px;
  }
  
  .custom-table-scroll{
    width:100%;
    overflow-x:auto;
    overflow-y:hidden;
    margin-top:24px;
    padding-bottom:8px;
    border-radius:12px;
  }
  
  .sleep-table{
    border-collapse:collapse;
    min-width:1250px;
    width:max-content;
    background:white;
  }
  
  .sleep-table th{
    background:#f3f4f6;
    color:#64748b;
    text-transform:uppercase;
    font-weight:700;
    font-size:11px;
    padding:14px 16px;
    text-align:left;
    border-bottom:2px solid #e5e7eb;
    white-space:nowrap;
  }
  
  .sleep-table td{
    padding:16px;
    border-bottom:1px solid #e5e7eb;
    color:#374151;
    font-size:13px;
    white-space:nowrap;
  }
  
  .meta-table{
    width:100% !important;
    min-width:100% !important;
    max-width:100% !important;
    border-collapse:collapse;
    table-layout:auto;
    display:table;
  }
  
  .meta-table thead{
    width:100%;
  }
  
  .meta-table tbody{
    width:100%;
  }
  
  .meta-table tr{
    width:100%;
  }
  
  .meta-table th{
    background:#f3f4f6;
    color:#64748b;
    text-transform:uppercase;
    font-weight:700;
    padding:18px;
    border-bottom:2px solid #e5e7eb;
    text-align:left;
  }
  
  .meta-table td{
    padding:18px;
    border-bottom:1px solid #e5e7eb;
    color:#374151;
  }
  
  .meta-table th:nth-child(1),
  .meta-table td:nth-child(1){
    width:18%;
  }
  
  .meta-table th:nth-child(2),
  .meta-table td:nth-child(2){
    width:38%;
  }
  
  .meta-table th:nth-child(3),
  .meta-table td:nth-child(3),
  .meta-table th:nth-child(4),
  .meta-table td:nth-child(4),
  .meta-table th:nth-child(5),
  .meta-table td:nth-child(5){
    width:14%;
    text-align:center;
  }
  
  .custom-table-scroll::-webkit-scrollbar{
    height:14px;
  }
  
  .custom-table-scroll::-webkit-scrollbar-track{
    background:#d1d5db;
    border-radius:999px;
  }
  
  .custom-table-scroll::-webkit-scrollbar-thumb{
    background:#8b5cf6;
    border-radius:999px;
  }
  
  .custom-table-scroll::-webkit-scrollbar-thumb:hover{
    background:#6b7280;
  }
  
  .formula-box{
    background:#f9fafb;
    border:1px solid #d1d5db;
    padding:22px;
    margin-top:22px;
    margin-bottom:24px;
  }
  
  .formula{
    font-size:20px;
    font-weight:700;
    color:#111827;
    margin-top:12px;
  }
  
  .result-grid{
    display:grid;
    grid-template-columns:1fr 1fr;
    gap:18px;
    margin-top:26px;
    margin-bottom:28px;
  }
  
  .result-card{
    background:#f5f3ff;
    border:2px solid #ddd6fe;
    padding:22px;
  }
  
  .result-card h4{
    font-size:12px;
    text-transform:uppercase;
    letter-spacing:1px;
    margin-bottom:10px;
    color:#6d28d9;
  }
  
  .result-card p{
    font-size:32px;
    font-weight:800;
    color:#14532d;
    margin:0;
  }
  
  .result-card.secondary{
    background:#eff6ff;
    border-color:#bfdbfe;
  }
  
  .result-card.secondary h4{
    color:#1d4ed8;
  }
  
  .result-card.secondary p{
    color:#1e3a8a;
  }
  
  .chart-img{
    width:100%;
    max-height:420px;
    object-fit:contain;
    margin-top:24px;
    border:1px solid #d1d5db;
    border-radius:10px;
    background:white;
    box-shadow: 0 20px 40px rgba(79,70,229,0.08);
    transition:0.3s;
  }
  
  .chart-img:hover{
    transform:scale(1.01);
  }
  
  pre{
    background:#020617;
    color:#e2e8f0;
    padding:30px;
    border-radius:8px;
    overflow:auto;
    font-size:14px;
    line-height:1.8;
    margin-top:26px;
  }
  
  footer{
    text-align:center;
    color:#6b7280;
    padding:50px 0;
    font-size:14px;
  }
  
  @media(max-width:1200px){
    .two-cols{
      grid-template-columns:1fr;
    }
    h2{
      font-size:40px;
    }
    .info-boxes{
      grid-template-columns:1fr;
    }
    .result-grid{
      grid-template-columns:1fr;
    }
    .navbar{
      padding:0 20px;
    }
    .menu{
      display:none;
    }
  }
  
  </style>
</head>

<body>

  <nav class="navbar">
    <div class="logo">
      <div class="logo-box">DS</div>
      <div class="logo-text">
        <h1>DreamySleepR</h1>
        <p>narzędzie wspomagające ocenę aplikacji monitorujących sen</p>
      </div>
    </div>
    <div class="menu">
      <a href="#problem">Problem</a>
      <a href="#dane">Dane</a>
      <a href="#wyniki">Wyniki</a>
      <a href="#odtworzenie">Odtworzenie</a>
    </div>
  </nav>

  <div class="container">

    <div class="grid">

      <section class="section" id="problem">
        <div class="section-label">SEKCJA 1</div>
        <h2>DreamySleepR - narzędzie wspomagające ocenę i wybór aplikacji do monitorowania snu.</h2>
        <p>
          Coraz więcej studentów korzysta z aplikacji monitorujących sen, aby poprawić jakość odpoczynku,
          kontrolować nawyki związane ze snem oraz lepiej radzić sobie ze stresem i zmęczeniem.
          Duża liczba dostępnych aplikacji oraz różnorodność oferowanych funkcji sprawiają jednak,
          że wybór odpowiedniego rozwiązania często bywa trudny.
        </p>
        <p>
          Pakiet DreamySleepR jest prostym narzędziem analitycznym umożliwiającym ocenę i ranking aplikacji
          do monitorowania snu z wykorzystaniem metod wielokryterialnego wspomagania decyzji.
        </p>
        <div class="info-boxes">
          <div class="info-box">
            <h4>Po co?</h4>
            <p>Pakiet umożliwia ocenę i porównanie aplikacji według wielu kryteriów.</p>
          </div>
          <div class="info-box">
            <h4>Dla kogo?</h4>
            <p>Narzędzie przeznaczone dla studentów oraz użytkowników zainteresowanych poprawą jakości snu.</p>
          </div>
          <div class="info-box">
            <h4>Co jest rezultatem?</h4>
            <p>Końcowym efektem jest ranking aplikacji oraz meta-ranking metod MCDA.</p>
          </div>
        </div>
      </section>

      <section class="section questions">
        <h3>Na jakie pytania poznasz odpowiedzi:</h3>
        <ol>
          <li>Która aplikacja najlepiej monitoruje sen?</li>
          <li>Jak wyglądają dane wejściowe?</li>
          <li>Jak dane ankietowe zamieniane są na oceny rozmyte?</li>
          <li>W jaki sposób ustalane są wagi kryteriów?</li>
          <li>Która alternatywa wygrywa i czy ranking jest stabilny?</li>
          <li>Jak wygląda przykładowy kod wykorzystania pakietu?</li>
        </ol>
      </section>

    </div>

    <div class="two-cols">

      <section class="section" id="dane">
        <div class="section-label">SEKCJA 2</div>
        <h3>Dane wejściowe - ocena aplikacji do monitorowania snu</h3>
        <p>Poniżej zamieszczono przykładowe dane wejściowe wykorzystywane przez pakiet DreamySleepR.</p>
        <p>
          Analizowane alternatywy reprezentują aplikacje do monitorowania snu.
          Uwzględniono kryteria takie jak dokładność pomiaru snu, analiza statystyk,
          łatwość obsługi, integracja z urządzeniami czy zużycie baterii.
        </p>
        <div class="custom-table-scroll">
          <table class="sleep-table">
            <thead>
              <tr>
                <th>Ekspert</th>
                <th>Aplikacja</th>
                <th>Dokładność faz snu</th>
                <th>Analiza statystyk</th>
                <th>Łatwość obsługi</th>
                <th>Dane fizjologiczne</th>
                <th>Integracja z urządzeniami</th>
                <th>Funkcje dodatkowe</th>
                <th>Zużycie baterii</th>
                <th>Cena / subskrypcja</th>
              </tr>
            </thead>
            <tbody>
              ', data_rows_html, '
            </tbody>
          </table>
        </div>
      </section>

      <section class="section">
        <div class="section-label">SEKCJA 3</div>
        <h3>Rozmywanie danych</h3>
        <p>
          W pakiecie DreamySleepR zastosowano trójkątne liczby rozmyte (TFN – Triangular Fuzzy Numbers),
          które umożliwiają uwzględnienie niepewności ocen użytkowników.
        </p>
        <div class="formula-box">
          <p>Jeżeli użytkownik oceni aplikację na 6, rzeczywista ocena może być nieco niższa lub wyższa.</p>
          <div class="formula">TFN = (x - 1, x, x + 1)</div>
          <div class="formula">wynik ostry = (l + 4m + u) / 6</div>
        </div>
        <img src="man/figures/README-tfn-1.png" class="chart-img" alt="Trójkątne liczby rozmyte">
      </section>

    </div>

    <div class="two-cols">

      <section class="section">
        <div class="section-label">SEKCJA 4</div>
        <h3>Wagi kryteriów</h3>
        <p>Wagi kryteriów zostały wyznaczone metodą Best-Worst Method (BWM).</p>
        <p>W przykładowej analizie za najważniejsze kryterium uznano cenę/subskrypcję, a także łatwość obsługi (UX) i integrację z urządzeniami.</p>
        <div class="info-boxes" style="grid-template-columns: repeat(4, 1fr); margin-top: 18px;">
          ', wagi_html, '
        </div>
        <img src="man/figures/README-bwm-1.png" class="chart-img" alt="Wagi BWM">
      </section>

      <section class="section" id="wyniki">
        <div class="section-label">SEKCJA 5</div>
        <h3>Meta-ranking</h3>
        <p>Pakiet wykorzystuje metody Fuzzy TOPSIS, Fuzzy VIKOR oraz Fuzzy MULTIMOORA.</p>
        <p>Meta-ranking agreguje wyniki wszystkich metod i wskazuje najbardziej rekomendowaną aplikację.</p>
        <div class="result-grid">
          <div class="result-card">
            <h4>Najlepsza alternatywa</h4>
            <p>', escape_html(top_app), '</p>
          </div>
          <div class="result-card secondary">
            <h4>Czy ranking jest stabilny?</h4>
            <p>', escape_html(stabilnosc), '</p>
          </div>
          <table class="meta-table">
            <thead>
              <tr>
                <th>Miejsce w meta-rankingu</th>
                <th>Aplikacja</th>
                <th>Metoda TOPSIS</th>
                <th>Metoda VIKOR</th>
                <th>Metoda MULTIMOORA</th>
              </tr>
            </thead>
            <tbody>
              ', meta_rows_html, '
            </tbody>
          </table>
        </div>
        <img src="man/figures/README-meta-1.png" class="chart-img" alt="Wykres meta-rankingu">
      </section>

    </div>

    <section class="section" id="odtworzenie">
      <div class="section-label">SEKCJA 6</div>
      <h3>Jak odtworzyć analizę? - krok po kroku</h3>
      <p>Poniższa instrukcja przeprowadza użytkownika przez pełną analizę z wykorzystaniem demonstracyjnego zbioru danych.</p>
<pre>
data("mcda_dane_surowe")

dane_rozmyte &lt;- przygotuj_dane_mcda(
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
  kolumna_alternatyw = "Aplikacja"
)

macierz &lt;- as.matrix(dane_rozmyte)

typy &lt;- c(
  "max", "max", "max", "max",
  "max", "max", "min", "min"
)

wynik &lt;- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz,
  typy_kryteriow = typy,
  bwm_kryteria = c("Dokladnosc", "Analiza", "UX", "Fizjologia", "Integracja", "Funkcje", "Bateria", "Cena"),
  bwm_najlepsze = c(1, 2, 3, 2, 2, 2, 4, 5),
  bwm_najgorsze = c(5, 4, 3, 4, 4, 4, 2, 1)
)

print(wynik$porownanie)
</pre>
    </section>

    <footer>
      DreamySleepR • 2026
    </footer>

  </div>

</body>
</html>'
)

site_html <- sub("[[:space:]]+$", "", site_html)
writeLines(site_html, "index.html", useBytes = TRUE)
message("Rendered index.html")
