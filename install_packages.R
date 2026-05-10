# Dünya Deprem Haritası — Paket Kurulum Scripti
# RStudio'da bir kez çalıştır: Source butonu veya Ctrl+Shift+S

required <- c(
  "shiny",          # web uygulaması
  "bslib",          # bootstrap tema
  "leaflet",        # interaktif harita
  "leaflet.extras", # ısı haritası, ekstra katmanlar
  "httr2",          # USGS API isteği
  "jsonlite",       # GeoJSON ayrıştırma
  "sf",             # CBS / coğrafi veri yapıları
  "dplyr",          # veri dönüşümü
  "DT",             # interaktif tablo
  "plotly",         # analiz/Türkiye sekmeleri grafikleri
  "scales"          # renk ve ölçek yardımcıları
)

missing <- required[!required %in% installed.packages()[, "Package"]]

if (length(missing) > 0) {
  message("Kurulacak paketler: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
} else {
  message("Tüm paketler zaten kurulu.")
}

# Kurulumu doğrula
invisible(lapply(required, function(p) {
  ok <- requireNamespace(p, quietly = TRUE)
  message(sprintf("  %-16s %s", p, if (ok) "OK" else "EKSİK"))
}))
