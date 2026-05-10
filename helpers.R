# helpers.R — USGS verisi çekme ve harita oluşturma yardımcıları
# encoding: UTF-8

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(sf)
  library(leaflet)
  library(scales)
})

# leaflet.extras opsiyonel: kurulu değilse ısı haritası ve bazı kontroller
# devre dışı kalır. Yine de uygulama çalışır.
HAS_HEATMAP <- requireNamespace("leaflet.extras", quietly = TRUE)
if (HAS_HEATMAP) {
  suppressPackageStartupMessages(library(leaflet.extras))
} else {
  message("leaflet.extras kurulu değil; ısı haritası ve tam ekran ",
          "devre dışı. Kurmak için: install.packages('leaflet.extras')")
}

# ---------------------------------------------------------------------
# USGS GeoJSON akışı
# ---------------------------------------------------------------------

USGS_BASE  <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary"
USGS_QUERY <- "https://earthquake.usgs.gov/fdsnws/event/1/query"

usgs_feed_url <- function(period = c("hour", "day", "week", "month")) {
  period <- match.arg(period)
  # 2.5_*.geojson — M≥2.5 (all_*'in 10-30x daha küçüğü); harita slider'ı
  # zaten min=2'den başlıyor, yani M<2.5 zaten görüntülenmiyor.
  sprintf("%s/2.5_%s.geojson", USGS_BASE, period)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
num_or_na <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)

fetch_usgs <- function(period = "day") {
  url <- usgs_feed_url(period)

  # Ay feed'i ~10MB; yavaş bağlantıda 20sn yetmez. Periyoda göre uyarla.
  timeout_secs <- switch(period, month = 90, week = 60, day = 30, hour = 20, 30)

  resp <- tryCatch(
    request(url) |>
      req_timeout(timeout_secs) |>
      req_user_agent("R-Deprem-Haritasi/1.0") |>
      req_perform(),
    error = function(e) {
      message("USGS isteği başarısız (", period, "): ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(resp) || resp_status(resp) >= 400) {
    return(empty_quake_df())
  }

  raw <- tryCatch(
    fromJSON(resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) {
      message("USGS JSON parse hatası: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw)) return(empty_quake_df())

  feats <- raw$features
  n <- length(feats)
  if (n == 0) return(empty_quake_df())

  # Sütunları önceden ayır — do.call(rbind, lapply(...)) O(n²) idi
  ids      <- character(n); mags     <- numeric(n)
  places   <- character(n); times    <- numeric(n)
  updateds <- numeric(n);   tsunamis <- integer(n)
  sigs     <- numeric(n);   types    <- character(n)
  urls     <- character(n); lons     <- numeric(n)
  lats     <- numeric(n);   depths   <- numeric(n)

  for (i in seq_len(n)) {
    f <- feats[[i]]
    p <- f$properties
    # geometry / coordinates eksikse NA'larla devam et — bozuk feature crash etmesin
    co <- tryCatch(f$geometry$coordinates, error = function(e) NULL)
    if (is.null(co) || length(co) < 2) co <- list(NA, NA, NA)
    ids[i]      <- f$id %||% NA_character_
    mags[i]     <- num_or_na(p$mag)
    places[i]   <- p$place %||% NA_character_
    times[i]    <- num_or_na(p$time) / 1000
    updateds[i] <- num_or_na(p$updated) / 1000
    tsunamis[i] <- as.integer(p$tsunami %||% 0)
    sigs[i]     <- num_or_na(p$sig)
    types[i]    <- p$type %||% "earthquake"
    urls[i]     <- p$url %||% NA_character_
    lons[i]     <- num_or_na(co[[1]])
    lats[i]     <- num_or_na(co[[2]])
    depths[i]   <- if (length(co) >= 3) num_or_na(co[[3]]) else NA_real_
  }

  df <- data.frame(
    id        = ids,
    mag       = mags,
    place     = places,
    time      = as.POSIXct(times,    origin = "1970-01-01", tz = "UTC"),
    updated   = as.POSIXct(updateds, origin = "1970-01-01", tz = "UTC"),
    tsunami   = tsunamis,
    sig       = sigs,
    type      = types,
    url       = urls,
    lon       = lons,
    lat       = lats,
    depth_km  = depths,
    source    = "USGS",
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$lon) & is.finite(df$lat) &
           !is.na(df$time), , drop = FALSE]
  # Windows R bazen JSON'dan gelen Türkçe karakterli stringleri UTF-8 olarak
  # işaretlemez; grep/grepl gibi fonksiyonlar "invalid UTF-8" uyarır. Açıkça işaretle.
  for (col in c("id", "place", "type", "url")) {
    if (col %in% names(df)) Encoding(df[[col]]) <- "UTF-8"
  }
  df$continent <- classify_continent(df$lon, df$lat)
  df
}

# ---------------------------------------------------------------------
# Kandilli Rasathanesi (Türkiye) — opsiyonel ek kaynak
# ---------------------------------------------------------------------

KANDILLI_URL <- "https://api.orhanaydogdu.com.tr/deprem/kandilli/live"

fetch_kandilli <- function(limit = 500) {
  resp <- tryCatch(
    request(KANDILLI_URL) |>
      req_url_query(limit = limit) |>
      req_timeout(20) |>
      req_user_agent("R-Deprem-Haritasi/1.0") |>
      req_perform(),
    error = function(e) {
      message("Kandilli isteği başarısız: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(resp) || resp_status(resp) >= 400) {
    return(empty_quake_df())
  }

  raw <- tryCatch(
    fromJSON(resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) {
      message("Kandilli JSON parse hatası: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw)) return(empty_quake_df())

  recs <- raw$result
  n <- length(recs)
  if (is.null(recs) || n == 0) return(empty_quake_df())

  ids    <- character(n); mags   <- numeric(n)
  places <- character(n); times  <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  lons   <- numeric(n);   lats   <- numeric(n)
  depths <- numeric(n)

  # Geçerli zaman aralığı: 1990–2100 (saniye cinsinden Unix). Bu dışındaki
  # değerler ya yanlış birim (ms) ya da bozuk veri demek.
  unix_min <- as.numeric(as.POSIXct("1990-01-01", tz = "UTC"))   # ~6.3e8
  unix_max <- as.numeric(as.POSIXct("2100-01-01", tz = "UTC"))   # ~4.1e9

  parse_kandilli_time <- function(r) {
    # 1) created_at numerik (saniye veya milisaniye olabilir)
    t_unix <- suppressWarnings(as.numeric(r$created_at))
    if (is.finite(t_unix) && t_unix > 0) {
      # ms tespiti: makul saniye aralığını aşmışsa /1000
      if (t_unix > unix_max) t_unix <- t_unix / 1000
      if (t_unix >= unix_min && t_unix <= unix_max) {
        return(as.POSIXct(t_unix, origin = "1970-01-01", tz = "UTC"))
      }
    }
    # 2) date_time stringi (Türkiye lokal saatiyle gelir)
    s <- r$date_time %||% r$date %||% NA_character_
    if (!is.na(s) && nzchar(s)) {
      tt <- suppressWarnings(
              as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S",
                         tz = "Europe/Istanbul"))
      if (!is.na(tt)) {
        attr(tt, "tzone") <- "UTC"
        return(tt)
      }
    }
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  }

  for (i in seq_len(n)) {
    r <- recs[[i]]
    coords <- r$geojson$coordinates
    times[i]  <- parse_kandilli_time(r)
    ids[i]    <- r$earthquake_id %||% r$`_id` %||% NA_character_
    mags[i]   <- num_or_na(r$mag)
    places[i] <- r$title %||% NA_character_
    lons[i]   <- num_or_na(coords[[1]])
    lats[i]   <- num_or_na(coords[[2]])
    depths[i] <- num_or_na(r$depth)
  }

  df <- data.frame(
    id        = ids,
    mag       = mags,
    place     = places,
    time      = times,
    updated   = times,
    tsunami   = 0L,
    sig       = NA_real_,
    type      = "earthquake",
    url       = "http://www.koeri.boun.edu.tr/scripts/lst9.asp",
    lon       = lons,
    lat       = lats,
    depth_km  = depths,
    source    = "Kandilli",
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$lon) & is.finite(df$lat) &
           !is.na(df$time), , drop = FALSE]
  # Türkçe yer adları için açıkça UTF-8 olarak işaretle (Windows R quirk)
  for (col in c("id", "place", "type", "url")) {
    if (col %in% names(df)) Encoding(df[[col]]) <- "UTF-8"
  }
  df$continent <- classify_continent(df$lon, df$lat)
  df
}

# ---------------------------------------------------------------------
# Tarihsel sorgu — USGS FDSN query API
# Kullanıcı bbox / şehir merkezi+yarıçap + tarih + min büyüklük seçer.
# ---------------------------------------------------------------------

# Türkiye'nin tüm 81 ili (yaklaşık merkez koordinatları)
TR_CITIES <- data.frame(
  name = c("Adana", "Adıyaman", "Afyonkarahisar", "Ağrı", "Aksaray",
           "Amasya", "Ankara", "Antalya", "Ardahan", "Artvin",
           "Aydın", "Balıkesir", "Bartın", "Batman", "Bayburt",
           "Bilecik", "Bingöl", "Bitlis", "Bolu", "Burdur",
           "Bursa", "Çanakkale", "Çankırı", "Çorum", "Denizli",
           "Diyarbakır", "Düzce", "Edirne", "Elazığ", "Erzincan",
           "Erzurum", "Eskişehir", "Gaziantep", "Giresun", "Gümüşhane",
           "Hakkari", "Hatay", "Iğdır", "Isparta", "İstanbul",
           "İzmir", "Kahramanmaraş", "Karabük", "Karaman", "Kars",
           "Kastamonu", "Kayseri", "Kilis", "Kırıkkale", "Kırklareli",
           "Kırşehir", "Kocaeli", "Konya", "Kütahya", "Malatya",
           "Manisa", "Mardin", "Mersin", "Muğla", "Muş",
           "Nevşehir", "Niğde", "Ordu", "Osmaniye", "Rize",
           "Sakarya", "Samsun", "Siirt", "Sinop", "Sivas",
           "Şanlıurfa", "Şırnak", "Tekirdağ", "Tokat", "Trabzon",
           "Tunceli", "Uşak", "Van", "Yalova", "Yozgat", "Zonguldak"),
  lat  = c(37.00, 37.76, 38.76, 39.72, 38.37,
           40.65, 39.93, 36.89, 41.11, 41.18,
           37.85, 39.65, 41.63, 37.88, 40.26,
           40.14, 38.89, 38.40, 40.74, 37.72,
           40.18, 40.16, 40.60, 40.55, 37.78,
           37.92, 40.84, 41.68, 38.67, 39.75,
           39.90, 39.78, 37.07, 40.91, 40.46,
           37.57, 36.20, 39.92, 37.77, 41.01,
           38.42, 37.58, 41.20, 37.18, 40.61,
           41.38, 38.73, 36.72, 39.85, 41.74,
           39.15, 40.85, 37.87, 39.42, 38.35,
           38.62, 37.31, 36.81, 37.21, 38.74,
           38.62, 37.97, 40.99, 37.07, 41.02,
           40.78, 41.29, 37.93, 42.03, 39.75,
           37.17, 37.52, 40.98, 40.32, 41.00,
           39.11, 38.68, 38.49, 40.65, 39.82, 41.46),
  lon  = c(35.32, 38.28, 30.54, 43.05, 34.03,
           35.83, 32.86, 30.71, 42.70, 41.82,
           27.85, 27.89, 32.34, 41.13, 40.22,
           29.98, 40.50, 42.11, 31.61, 30.29,
           29.07, 26.41, 33.61, 34.96, 29.09,
           40.23, 31.16, 26.56, 39.22, 39.49,
           41.27, 30.52, 37.38, 38.39, 39.48,
           43.74, 36.16, 44.04, 30.55, 28.97,
           27.13, 36.93, 32.63, 33.22, 43.10,
           33.78, 35.48, 37.12, 33.51, 27.22,
           34.16, 29.88, 32.49, 29.99, 38.32,
           27.43, 40.74, 34.64, 28.36, 41.50,
           34.71, 34.68, 37.88, 36.25, 40.52,
           30.40, 36.33, 41.94, 35.16, 37.02,
           38.79, 42.46, 27.51, 36.55, 39.72,
           39.55, 29.41, 43.41, 29.27, 34.81, 31.79),
  stringsAsFactors = FALSE
)
TR_CITIES$name <- enc2utf8(TR_CITIES$name)
TR_CITIES <- TR_CITIES[order(TR_CITIES$name), ]

# Büyük illerin önde gelen ilçeleri (yaklaşık merkez koordinatları)
TR_DISTRICTS <- list(
  "İstanbul" = data.frame(
    name = c("Kadıköy", "Üsküdar", "Beşiktaş", "Şişli", "Beyoğlu",
             "Fatih", "Bakırköy", "Kartal", "Maltepe", "Pendik",
             "Ataşehir", "Sancaktepe", "Tuzla", "Avcılar", "Esenyurt"),
    lat  = c(40.9833, 41.0233, 41.0428, 41.0589, 41.0382,
             41.0053, 40.9788, 40.9061, 40.9356, 40.8775,
             40.9923, 41.0028, 40.8158, 40.9794, 41.0294),
    lon  = c(29.0167, 29.0204, 29.0089, 28.9856, 28.9784,
             28.9498, 28.8717, 29.1881, 29.1389, 29.2592,
             29.1244, 29.2342, 29.3008, 28.7214, 28.6720),
    stringsAsFactors = FALSE
  ),
  "Ankara" = data.frame(
    name = c("Çankaya", "Keçiören", "Yenimahalle", "Mamak", "Etimesgut",
             "Sincan", "Altındağ", "Pursaklar", "Gölbaşı"),
    lat  = c(39.9039, 39.9756, 39.9606, 39.9356, 39.9525,
             39.9525, 39.9603, 39.9925, 39.7892),
    lon  = c(32.8617, 32.8639, 32.7997, 32.9089, 32.6700,
             32.5808, 32.8731, 32.9000, 32.8089),
    stringsAsFactors = FALSE
  ),
  "İzmir" = data.frame(
    name = c("Konak", "Bornova", "Karşıyaka", "Buca", "Gaziemir",
             "Bayraklı", "Çiğli", "Balçova", "Narlıdere", "Karabağlar"),
    lat  = c(38.4192, 38.4631, 38.4593, 38.3878, 38.3097,
             38.4628, 38.4994, 38.3833, 38.3942, 38.4006),
    lon  = c(27.1287, 27.2167, 27.1142, 27.1642, 27.1364,
             27.1750, 27.0667, 27.0481, 27.0094, 27.1167),
    stringsAsFactors = FALSE
  ),
  "Bursa" = data.frame(
    name = c("Osmangazi", "Yıldırım", "Nilüfer", "Mudanya", "İnegöl",
             "Gemlik", "Karacabey"),
    lat  = c(40.1786, 40.1839, 40.2167, 40.3711, 40.0789,
             40.4319, 40.2161),
    lon  = c(29.0617, 29.1142, 28.9892, 28.8842, 29.5097,
             29.1500, 28.3603),
    stringsAsFactors = FALSE
  ),
  "Adana" = data.frame(
    name = c("Seyhan", "Yüreğir", "Çukurova", "Sarıçam", "Ceyhan"),
    lat  = c(36.9981, 37.0006, 37.0344, 37.0506, 37.0269),
    lon  = c(35.3006, 35.3633, 35.3219, 35.3925, 35.8228),
    stringsAsFactors = FALSE
  ),
  "Antalya" = data.frame(
    name = c("Muratpaşa", "Kepez", "Konyaaltı", "Aksu", "Döşemealtı",
             "Alanya", "Manavgat", "Serik", "Kemer", "Kaş", "Side"),
    lat  = c(36.8860, 36.9170, 36.8640, 36.9480, 36.9830,
             36.5470, 36.7870, 36.9160, 36.6010, 36.1980, 36.7680),
    lon  = c(30.7130, 30.6810, 30.6280, 30.8460, 30.6310,
             31.9990, 31.4430, 31.0990, 30.5600, 29.6380, 31.3890),
    stringsAsFactors = FALSE
  ),
  "Konya" = data.frame(
    name = c("Selçuklu", "Meram", "Karatay", "Akşehir", "Beyşehir",
             "Ereğli", "Çumra", "Seydişehir", "Ilgın"),
    lat  = c(37.9180, 37.8310, 37.8710, 38.3580, 37.6770,
             37.5140, 37.5720, 37.4260, 38.2790),
    lon  = c(32.4930, 32.4910, 32.5220, 31.4160, 31.7280,
             34.0480, 32.7760, 31.8520, 31.9160),
    stringsAsFactors = FALSE
  ),
  "Gaziantep" = data.frame(
    name = c("Şahinbey", "Şehitkamil", "Nizip", "İslahiye", "Oğuzeli",
             "Araban", "Nurdağı"),
    lat  = c(37.0600, 37.0830, 37.0120, 37.0270, 36.9870, 37.4220, 37.1830),
    lon  = c(37.3780, 37.3800, 37.7950, 36.6330, 37.5180, 37.6900, 36.7330),
    stringsAsFactors = FALSE
  ),
  "Şanlıurfa" = data.frame(
    name = c("Haliliye", "Eyyübiye", "Karaköprü", "Siverek", "Viranşehir",
             "Birecik", "Suruç", "Akçakale", "Harran", "Bozova"),
    lat  = c(37.1660, 37.1400, 37.2130, 37.7550, 37.2320,
             37.0250, 36.9760, 36.7150, 36.8640, 37.3680),
    lon  = c(38.8020, 38.7970, 38.7810, 39.3180, 39.7660,
             37.9780, 38.4210, 38.9460, 39.0380, 38.5160),
    stringsAsFactors = FALSE
  ),
  "Mersin" = data.frame(
    name = c("Akdeniz", "Mezitli", "Toroslar", "Yenişehir", "Tarsus",
             "Erdemli", "Silifke", "Anamur", "Bozyazı"),
    lat  = c(36.8120, 36.7380, 36.8120, 36.7930, 36.9170,
             36.6110, 36.3780, 36.0750, 36.1110),
    lon  = c(34.6430, 34.5270, 34.6540, 34.6010, 34.8920,
             34.3000, 33.9280, 32.8360, 32.9530),
    stringsAsFactors = FALSE
  ),
  "Diyarbakır" = data.frame(
    name = c("Bağlar", "Kayapınar", "Yenişehir", "Sur", "Bismil",
             "Ergani", "Silvan", "Çınar", "Çermik"),
    lat  = c(37.9400, 37.9460, 37.9100, 37.9130, 37.8400,
             38.2700, 38.1420, 37.7280, 38.1370),
    lon  = c(40.1980, 40.1800, 40.2340, 40.2410, 40.6580,
             39.7660, 41.0010, 40.4080, 39.4520),
    stringsAsFactors = FALSE
  ),
  "Kayseri" = data.frame(
    name = c("Melikgazi", "Kocasinan", "Talas", "Hacılar", "Develi",
             "Yahyalı", "Bünyan", "İncesu", "Pınarbaşı"),
    lat  = c(38.7330, 38.7380, 38.6820, 38.6380, 38.3920,
             38.0990, 38.8530, 38.6420, 38.7240),
    lon  = c(35.4940, 35.4730, 35.5550, 35.4530, 35.4910,
             35.3590, 35.8600, 35.1830, 36.3920),
    stringsAsFactors = FALSE
  ),
  "Eskişehir" = data.frame(
    name = c("Tepebaşı", "Odunpazarı", "Sivrihisar", "Çifteler",
             "Mihalıçcık", "Seyitgazi", "Mahmudiye"),
    lat  = c(39.7950, 39.7660, 39.4500, 39.3850, 39.9010, 39.4490, 39.4810),
    lon  = c(30.4900, 30.5270, 31.5390, 31.0440, 31.5100, 30.6920, 30.9710),
    stringsAsFactors = FALSE
  ),
  "Erzurum" = data.frame(
    name = c("Yakutiye", "Palandöken", "Aziziye", "Aşkale", "Horasan",
             "Pasinler", "Oltu", "Tortum", "Hınıs", "Karayazı"),
    lat  = c(39.9100, 39.8800, 39.9510, 39.9210, 40.0430,
             39.9810, 40.5460, 40.2950, 39.3590, 39.6900),
    lon  = c(41.2790, 41.2860, 41.1490, 40.6900, 42.1690,
             41.6730, 41.9840, 41.5450, 41.6920, 42.1320),
    stringsAsFactors = FALSE
  ),
  "Trabzon" = data.frame(
    name = c("Ortahisar", "Akçaabat", "Araklı", "Yomra", "Of",
             "Beşikdüzü", "Vakfıkebir", "Sürmene", "Maçka"),
    lat  = c(41.0050, 41.0260, 40.9370, 40.9520, 40.9450,
             41.0570, 41.0410, 40.9100, 40.8170),
    lon  = c(39.7220, 39.5660, 40.0410, 39.8600, 40.2650,
             39.2350, 39.2900, 40.1830, 39.6090),
    stringsAsFactors = FALSE
  ),
  "Samsun" = data.frame(
    name = c("İlkadım", "Atakum", "Canik", "Bafra", "Vezirköprü",
             "Çarşamba", "Terme", "Havza", "19 Mayıs"),
    lat  = c(41.2860, 41.3300, 41.2900, 41.5670, 41.1400,
             41.1970, 41.2050, 40.9690, 41.5450),
    lon  = c(36.3300, 36.2620, 36.3550, 35.9160, 35.4500,
             36.7280, 36.9740, 35.6660, 36.0190),
    stringsAsFactors = FALSE
  ),
  "Van" = data.frame(
    name = c("İpekyolu", "Tuşba", "Edremit", "Erciş", "Başkale",
             "Gevaş", "Muradiye", "Özalp", "Çatak"),
    lat  = c(38.4950, 38.5260, 38.4500, 39.0300, 38.0440,
             38.2950, 38.9910, 38.6510, 38.0270),
    lon  = c(43.3780, 43.3530, 43.3010, 43.3660, 44.0130,
             43.1080, 43.7690, 44.0400, 43.0670),
    stringsAsFactors = FALSE
  ),
  "Hatay" = data.frame(
    name = c("Antakya", "İskenderun", "Defne", "Arsuz", "Dörtyol",
             "Kırıkhan", "Reyhanlı", "Samandağ", "Belen", "Erzin",
             "Payas", "Altınözü", "Yayladağı"),
    lat  = c(36.2020, 36.5870, 36.1760, 36.4130, 36.8440,
             36.5000, 36.2700, 36.0850, 36.4920, 36.9560,
             36.7510, 36.0900, 35.9020),
    lon  = c(36.1600, 36.1690, 36.1570, 35.8810, 36.2180,
             36.3670, 36.5670, 35.9780, 36.1970, 36.3970,
             36.2310, 36.2470, 36.0530),
    stringsAsFactors = FALSE
  ),
  "Malatya" = data.frame(
    name = c("Battalgazi", "Yeşilyurt", "Doğanşehir", "Akçadağ",
             "Darende", "Hekimhan", "Pütürge", "Arapgir"),
    lat  = c(38.3980, 38.2980, 38.0990, 38.3420, 38.5470,
             38.8160, 38.1980, 39.0400),
    lon  = c(38.3590, 38.2550, 37.8860, 37.9740, 37.5080,
             37.9270, 38.8770, 38.4920),
    stringsAsFactors = FALSE
  ),
  "Kahramanmaraş" = data.frame(
    name = c("Onikişubat", "Dulkadiroğlu", "Elbistan", "Afşin",
             "Türkoğlu", "Pazarcık", "Göksun", "Andırın", "Nurhak"),
    lat  = c(37.5870, 37.5800, 38.2050, 38.2460, 37.3880,
             37.4860, 38.0240, 37.5680, 37.9580),
    lon  = c(36.9370, 36.9500, 37.1930, 36.9180, 36.8440,
             37.2930, 36.4980, 36.3470, 37.4530),
    stringsAsFactors = FALSE
  ),
  "Elazığ" = data.frame(
    name = c("Merkez", "Kovancılar", "Karakoçan", "Palu", "Maden",
             "Sivrice", "Baskil", "Keban", "Arıcak"),
    lat  = c(38.6810, 38.7170, 38.9610, 38.7000, 38.3940,
             38.4500, 38.5680, 38.7950, 38.5240),
    lon  = c(39.2270, 39.8600, 40.0260, 39.9370, 39.6730,
             39.3190, 38.8210, 38.7480, 40.2520),
    stringsAsFactors = FALSE
  ),
  "Denizli" = data.frame(
    name = c("Pamukkale", "Merkezefendi", "Çivril", "Tavas",
             "Acıpayam", "Sarayköy", "Honaz", "Buldan", "Çal"),
    lat  = c(37.7870, 37.7640, 38.3000, 37.5720, 37.4280,
             37.9160, 37.7560, 38.0440, 38.0890),
    lon  = c(29.1180, 29.0870, 29.7410, 29.0840, 29.3550,
             28.9220, 29.2720, 28.8310, 29.4030),
    stringsAsFactors = FALSE
  ),
  "Manisa" = data.frame(
    name = c("Şehzadeler", "Yunusemre", "Akhisar", "Salihli", "Turgutlu",
             "Soma", "Alaşehir", "Demirci", "Kula", "Sarıgöl"),
    lat  = c(38.6170, 38.6180, 38.9190, 38.4830, 38.4950,
             39.1810, 38.3490, 39.0490, 38.5410, 38.2380),
    lon  = c(27.4280, 27.3790, 27.8320, 28.1390, 27.7060,
             27.6110, 28.5240, 28.6610, 28.6480, 28.6930),
    stringsAsFactors = FALSE
  ),
  "Aydın" = data.frame(
    name = c("Efeler", "Söke", "Nazilli", "Kuşadası", "Didim",
             "İncirliova", "Çine", "Germencik", "Karacasu"),
    lat  = c(37.8570, 37.7470, 37.9130, 37.8600, 37.3760,
             37.8510, 37.6120, 37.8720, 37.7270),
    lon  = c(27.8470, 27.4080, 28.3180, 27.2580, 27.2590,
             27.7280, 28.0600, 27.6020, 28.6080),
    stringsAsFactors = FALSE
  ),
  "Muğla" = data.frame(
    name = c("Menteşe", "Bodrum", "Fethiye", "Marmaris", "Milas",
             "Datça", "Ortaca", "Yatağan", "Köyceğiz", "Dalaman", "Ula"),
    lat  = c(37.2160, 37.0340, 36.6210, 36.8530, 37.3170,
             36.7300, 36.8340, 37.3410, 36.9700, 36.7710, 37.1050),
    lon  = c(28.3660, 27.4300, 29.1160, 28.2730, 27.7830,
             27.6850, 28.7680, 28.1390, 28.6810, 28.7960, 28.4150),
    stringsAsFactors = FALSE
  ),
  "Çanakkale" = data.frame(
    name = c("Merkez", "Biga", "Çan", "Ezine", "Lapseki",
             "Gelibolu", "Bayramiç", "Yenice", "Ayvacık", "Eceabat"),
    lat  = c(40.1550, 40.2280, 40.0270, 39.7880, 40.3420,
             40.4120, 39.8040, 39.9290, 39.6020, 40.1890),
    lon  = c(26.4080, 27.2460, 27.0540, 26.3500, 26.6850,
             26.6710, 26.6110, 27.2520, 26.4040, 26.3550),
    stringsAsFactors = FALSE
  ),
  "Balıkesir" = data.frame(
    name = c("Karesi", "Altıeylül", "Bandırma", "Edremit", "Burhaniye",
             "Ayvalık", "Gönen", "Susurluk", "Sındırgı", "Bigadiç"),
    lat  = c(39.6500, 39.6500, 40.3530, 39.5950, 39.4980,
             39.3170, 40.1030, 39.9090, 39.2410, 39.3950),
    lon  = c(27.8900, 27.8700, 27.9780, 27.0240, 26.9740,
             26.6960, 27.6520, 28.1570, 28.1740, 28.1300),
    stringsAsFactors = FALSE
  ),
  "Sakarya" = data.frame(
    name = c("Adapazarı", "Serdivan", "Erenler", "Akyazı", "Hendek",
             "Karasu", "Sapanca", "Geyve", "Pamukova", "Ferizli"),
    lat  = c(40.7810, 40.7790, 40.7600, 40.6870, 40.7980,
             41.1050, 40.6920, 40.5050, 40.5060, 40.9410),
    lon  = c(30.4020, 30.3580, 30.4300, 30.6210, 30.7480,
             30.6890, 30.2600, 30.2960, 30.1700, 30.4870),
    stringsAsFactors = FALSE
  ),
  "Kocaeli" = data.frame(
    name = c("İzmit", "Gebze", "Darıca", "Çayırova", "Körfez",
             "Derince", "Gölcük", "Kartepe", "Karamürsel", "Başiskele"),
    lat  = c(40.7750, 40.8030, 40.7700, 40.8080, 40.7690,
             40.7550, 40.7060, 40.7950, 40.6920, 40.6520),
    lon  = c(29.9460, 29.4310, 29.3780, 29.3850, 29.7830,
             29.8250, 29.8290, 30.1960, 29.6140, 29.9320),
    stringsAsFactors = FALSE
  ),
  "Düzce" = data.frame(
    name = c("Merkez", "Akçakoca", "Çilimli", "Gölyaka",
             "Yığılca", "Kaynaşlı", "Cumayeri", "Gümüşova"),
    lat  = c(40.8380, 41.0920, 40.8440, 40.7600,
             40.9670, 40.7960, 40.8830, 40.8470),
    lon  = c(31.1630, 31.1150, 31.0260, 31.0010,
             31.4520, 31.3140, 30.9450, 30.9510),
    stringsAsFactors = FALSE
  ),
  "Bolu" = data.frame(
    name = c("Merkez", "Gerede", "Mengen", "Mudurnu", "Göynük",
             "Yeniçağa", "Dörtdivan", "Kıbrıscık", "Seben"),
    lat  = c(40.7360, 40.8020, 41.0450, 40.4640, 40.3960,
             40.7780, 40.7400, 40.4140, 40.4150),
    lon  = c(31.6060, 32.1970, 32.0750, 31.1930, 30.7830,
             32.0290, 32.0570, 31.8580, 31.5790),
    stringsAsFactors = FALSE
  ),
  "Tekirdağ" = data.frame(
    name = c("Süleymanpaşa", "Çorlu", "Çerkezköy", "Kapaklı", "Ergene",
             "Malkara", "Saray", "Hayrabolu", "Şarköy", "Marmaraereğlisi"),
    lat  = c(40.9780, 41.1580, 41.2860, 41.3300, 41.2270,
             40.8900, 41.4480, 41.2130, 40.6160, 40.9710),
    lon  = c(27.5110, 27.8040, 28.0000, 27.9800, 27.8500,
             26.9000, 27.9260, 27.1010, 27.1100, 27.9560),
    stringsAsFactors = FALSE
  ),
  "Tokat" = data.frame(
    name = c("Merkez", "Erbaa", "Niksar", "Turhal", "Zile",
             "Reşadiye", "Almus", "Pazar", "Artova", "Yeşilyurt"),
    lat  = c(40.3180, 40.6730, 40.5950, 40.3850, 40.3010,
             40.3940, 40.3790, 40.2790, 40.1430, 40.0940),
    lon  = c(36.5540, 36.5720, 36.9530, 36.0880, 35.8860,
             37.3140, 36.9100, 36.2960, 36.3200, 36.1740),
    stringsAsFactors = FALSE
  ),
  "Sivas" = data.frame(
    name = c("Merkez", "Şarkışla", "Suşehri", "Yıldızeli", "Gemerek",
             "Gürün", "Divriği", "Zara", "Kangal", "Hafik", "Koyulhisar"),
    lat  = c(39.7470, 39.3480, 40.1660, 39.8720, 39.1820,
             38.7170, 39.3710, 39.8920, 39.2360, 39.8580, 40.2830),
    lon  = c(37.0180, 36.4120, 38.0870, 36.6020, 36.0770,
             37.2750, 38.1150, 37.7620, 37.3870, 37.3850, 37.8290),
    stringsAsFactors = FALSE
  ),
  "Bingöl" = data.frame(
    name = c("Merkez", "Solhan", "Karlıova", "Genç", "Adaklı",
             "Kiğı", "Yedisu", "Yayladere"),
    lat  = c(38.8840, 38.9650, 39.2980, 38.7480, 39.2900,
             39.3140, 39.4750, 39.4200),
    lon  = c(40.4990, 41.0520, 41.0180, 40.5570, 40.4830,
             40.3410, 40.5710, 40.0290),
    stringsAsFactors = FALSE
  ),
  "Adıyaman" = data.frame(
    name = c("Merkez", "Kahta", "Besni", "Gölbaşı", "Gerger",
             "Sincik", "Tut", "Çelikhan", "Samsat"),
    lat  = c(37.7660, 37.7810, 37.6920, 37.7850, 38.0260, 37.9730, 37.7880, 38.0290, 37.5810),
    lon  = c(38.2820, 38.6260, 37.8660, 37.6380, 38.8120, 38.6820, 37.9170, 38.2370, 38.4810),
    stringsAsFactors = FALSE
  ),
  "Afyonkarahisar" = data.frame(
    name = c("Merkez", "Sandıklı", "Bolvadin", "Dinar", "Emirdağ",
             "Şuhut", "Sultandağı", "İscehisar", "Çay", "Sincanlı"),
    lat  = c(38.7560, 38.4640, 38.7130, 38.0660, 39.0200, 38.5290, 38.5310, 38.8700, 38.5910, 38.7080),
    lon  = c(30.5390, 30.2730, 31.0460, 30.1660, 31.1500, 30.5460, 31.2210, 30.7680, 31.0250, 30.1790),
    stringsAsFactors = FALSE
  ),
  "Ağrı" = data.frame(
    name = c("Merkez", "Doğubayazıt", "Patnos", "Diyadin", "Eleşkirt",
             "Hamur", "Tutak", "Taşlıçay"),
    lat  = c(39.7200, 39.5470, 39.2360, 39.5410, 39.7980, 39.6040, 39.5460, 39.6770),
    lon  = c(43.0570, 44.0820, 42.8640, 43.6670, 42.6670, 42.9840, 42.7930, 43.4530),
    stringsAsFactors = FALSE
  ),
  "Aksaray" = data.frame(
    name = c("Merkez", "Ortaköy", "Eskil", "Güzelyurt", "Sarıyahşi",
             "Ağaçören", "Sultanhanı", "Gülağaç"),
    lat  = c(38.3710, 38.7380, 38.4010, 38.2700, 38.6540, 38.7700, 38.2490, 38.4600),
    lon  = c(34.0270, 34.0430, 33.4200, 34.3710, 33.5810, 33.8080, 33.5550, 34.2600),
    stringsAsFactors = FALSE
  ),
  "Amasya" = data.frame(
    name = c("Merkez", "Merzifon", "Suluova", "Taşova", "Gümüşhacıköy",
             "Hamamözü", "Göynücek"),
    lat  = c(40.6520, 40.8760, 40.8320, 40.7620, 40.8810, 40.7760, 40.3960),
    lon  = c(35.8330, 35.4610, 35.6510, 36.3180, 35.2070, 35.0560, 35.5290),
    stringsAsFactors = FALSE
  ),
  "Ardahan" = data.frame(
    name = c("Merkez", "Hanak", "Posof", "Damal", "Çıldır", "Göle"),
    lat  = c(41.1100, 41.2310, 41.5150, 41.4130, 41.1160, 40.7880),
    lon  = c(42.7020, 42.8520, 42.7280, 42.8720, 43.1350, 42.6120),
    stringsAsFactors = FALSE
  ),
  "Artvin" = data.frame(
    name = c("Merkez", "Hopa", "Arhavi", "Borçka", "Şavşat",
             "Yusufeli", "Murgul", "Ardanuç"),
    lat  = c(41.1800, 41.4050, 41.3540, 41.3590, 41.2430, 40.8230, 41.2700, 41.1240),
    lon  = c(41.8190, 41.4280, 41.3070, 41.6740, 42.3590, 41.5390, 41.5460, 42.0620),
    stringsAsFactors = FALSE
  ),
  "Bartın" = data.frame(
    name = c("Merkez", "Amasra", "Kurucaşile", "Ulus"),
    lat  = c(41.6340, 41.7490, 41.8380, 41.5830),
    lon  = c(32.3370, 32.3850, 32.7260, 32.6420),
    stringsAsFactors = FALSE
  ),
  "Batman" = data.frame(
    name = c("Merkez", "Kozluk", "Sason", "Beşiri", "Hasankeyf", "Gercüş"),
    lat  = c(37.8810, 38.1930, 38.3180, 37.9170, 37.7110, 37.5750),
    lon  = c(41.1350, 41.4940, 41.4070, 41.2810, 41.4130, 41.4350),
    stringsAsFactors = FALSE
  ),
  "Bayburt" = data.frame(
    name = c("Merkez", "Aydıntepe", "Demirözü"),
    lat  = c(40.2600, 40.3880, 40.1840),
    lon  = c(40.2220, 40.1580, 39.8920),
    stringsAsFactors = FALSE
  ),
  "Bilecik" = data.frame(
    name = c("Merkez", "Bozüyük", "Söğüt", "Osmaneli", "Pazaryeri",
             "Gölpazarı", "İnhisar", "Yenipazar"),
    lat  = c(40.1420, 39.9110, 40.0180, 40.3470, 40.0450, 40.2790, 40.0710, 40.1690),
    lon  = c(29.9790, 30.0360, 30.1900, 30.0270, 29.8760, 30.3190, 30.3370, 30.5260),
    stringsAsFactors = FALSE
  ),
  "Bitlis" = data.frame(
    name = c("Merkez", "Tatvan", "Ahlat", "Adilcevaz", "Hizan",
             "Mutki", "Güroymak"),
    lat  = c(38.4010, 38.5000, 38.7480, 38.7970, 38.2360, 38.4040, 38.5750),
    lon  = c(42.1080, 42.2760, 42.4860, 42.7470, 42.4210, 41.9290, 41.9970),
    stringsAsFactors = FALSE
  ),
  "Burdur" = data.frame(
    name = c("Merkez", "Bucak", "Gölhisar", "Tefenni", "Karamanlı",
             "Yeşilova", "Ağlasun", "Çavdır"),
    lat  = c(37.7200, 37.4600, 37.1390, 37.3170, 37.3430, 37.5150, 37.6430, 37.1690),
    lon  = c(30.2910, 30.6090, 29.5100, 29.7780, 29.8600, 29.7510, 30.5350, 29.6920),
    stringsAsFactors = FALSE
  ),
  "Çankırı" = data.frame(
    name = c("Merkez", "Çerkeş", "Ilgaz", "Şabanözü", "Orta",
             "Eldivan", "Kızılırmak", "Kurşunlu", "Atkaracalar", "Yapraklı"),
    lat  = c(40.6010, 40.8120, 40.9330, 40.4750, 40.6250, 40.5600, 40.3430, 40.8460, 40.8170, 40.7670),
    lon  = c(33.6170, 32.8920, 33.6240, 33.2990, 33.0900, 33.4610, 33.9870, 33.2750, 33.0680, 33.7800),
    stringsAsFactors = FALSE
  ),
  "Çorum" = data.frame(
    name = c("Merkez", "Sungurlu", "Osmancık", "İskilip", "Alaca",
             "Mecitözü", "Bayat", "Kargı", "Ortaköy", "Oğuzlar"),
    lat  = c(40.5500, 40.1690, 40.9710, 40.7380, 40.1690, 40.5220, 40.6770, 41.1420, 40.2440, 40.9370),
    lon  = c(34.9570, 34.3780, 34.8040, 34.4730, 34.8440, 35.2990, 34.2600, 34.5010, 35.1620, 34.6160),
    stringsAsFactors = FALSE
  ),
  "Edirne" = data.frame(
    name = c("Merkez", "Keşan", "Uzunköprü", "İpsala", "Lalapaşa",
             "Havsa", "Süloğlu", "Meriç", "Enez"),
    lat  = c(41.6770, 40.8570, 41.2650, 40.9270, 41.8420, 41.5470, 41.7680, 41.1930, 40.7260),
    lon  = c(26.5570, 26.6280, 26.6900, 26.3870, 26.7520, 26.8210, 26.9170, 26.4210, 26.0830),
    stringsAsFactors = FALSE
  ),
  "Erzincan" = data.frame(
    name = c("Merkez", "Tercan", "Refahiye", "Üzümlü", "Çayırlı",
             "İliç", "Otlukbeli", "Kemah", "Kemaliye"),
    lat  = c(39.7470, 39.7810, 39.8900, 39.7130, 39.8210, 39.4520, 40.0630, 39.6050, 39.2600),
    lon  = c(39.4910, 40.3780, 38.7660, 39.6700, 40.0540, 38.5640, 39.9690, 39.0250, 38.4950),
    stringsAsFactors = FALSE
  ),
  "Giresun" = data.frame(
    name = c("Merkez", "Bulancak", "Görele", "Espiye", "Tirebolu",
             "Eynesil", "Keşap", "Şebinkarahisar", "Alucra", "Yağlıdere"),
    lat  = c(40.9130, 40.9400, 41.0380, 40.9970, 41.0050, 41.0750, 40.9640, 40.2900, 40.3190, 40.7820),
    lon  = c(38.3920, 38.2360, 39.0010, 38.7010, 38.8230, 39.0910, 38.5310, 38.4230, 38.7780, 38.6010),
    stringsAsFactors = FALSE
  ),
  "Gümüşhane" = data.frame(
    name = c("Merkez", "Kelkit", "Şiran", "Torul", "Köse", "Kürtün"),
    lat  = c(40.4600, 40.1370, 40.1900, 40.5600, 40.2150, 40.6900),
    lon  = c(39.4810, 39.4340, 39.1190, 39.2910, 39.6520, 39.0640),
    stringsAsFactors = FALSE
  ),
  "Hakkari" = data.frame(
    name = c("Merkez", "Yüksekova", "Şemdinli", "Çukurca", "Derecik"),
    lat  = c(37.5750, 37.5660, 37.2980, 37.2450, 37.1870),
    lon  = c(43.7410, 44.2820, 44.5750, 43.6170, 44.2550),
    stringsAsFactors = FALSE
  ),
  "Iğdır" = data.frame(
    name = c("Merkez", "Tuzluca", "Karakoyunlu", "Aralık"),
    lat  = c(39.9220, 40.0460, 40.0000, 39.8750),
    lon  = c(44.0450, 43.6580, 43.7620, 44.5190),
    stringsAsFactors = FALSE
  ),
  "Isparta" = data.frame(
    name = c("Merkez", "Yalvaç", "Eğirdir", "Şarkikaraağaç", "Senirkent",
             "Uluborlu", "Sütçüler", "Atabey", "Aksu", "Gönen", "Keçiborlu"),
    lat  = c(37.7660, 38.2980, 37.8710, 38.0800, 38.0970, 38.0820, 37.4950, 37.9510, 37.7950, 37.9570, 37.9430),
    lon  = c(30.5540, 31.1810, 30.8520, 31.3660, 30.5470, 30.4530, 30.9850, 30.6400, 31.0690, 30.5150, 30.3000),
    stringsAsFactors = FALSE
  ),
  "Karabük" = data.frame(
    name = c("Merkez", "Safranbolu", "Eskipazar", "Ovacık", "Yenice", "Eflani"),
    lat  = c(41.2040, 41.2520, 40.9480, 41.0850, 41.1930, 41.4260),
    lon  = c(32.6260, 32.6940, 32.5310, 32.9280, 32.3360, 32.9540),
    stringsAsFactors = FALSE
  ),
  "Karaman" = data.frame(
    name = c("Merkez", "Ermenek", "Sarıveliler", "Ayrancı", "Başyayla", "Kazımkarabekir"),
    lat  = c(37.1810, 36.6380, 36.4830, 37.3570, 36.6590, 37.2200),
    lon  = c(33.2220, 32.8900, 32.6750, 33.6870, 32.7550, 33.0220),
    stringsAsFactors = FALSE
  ),
  "Kars" = data.frame(
    name = c("Merkez", "Sarıkamış", "Kağızman", "Selim", "Susuz",
             "Akyaka", "Arpaçay", "Digor"),
    lat  = c(40.6080, 40.3310, 40.1500, 40.4510, 40.7930, 40.7280, 40.8470, 40.3710),
    lon  = c(43.0970, 42.5750, 43.1350, 42.7780, 43.1140, 43.6160, 43.3140, 43.4180),
    stringsAsFactors = FALSE
  ),
  "Kastamonu" = data.frame(
    name = c("Merkez", "Tosya", "Taşköprü", "İnebolu", "Cide",
             "Daday", "Devrekani", "Araç", "Bozkurt", "Çatalzeytin",
             "Hanönü", "Küre", "Pınarbaşı", "Şenpazar", "Seydiler", "Abana"),
    lat  = c(41.3760, 41.0120, 41.5120, 41.9720, 41.8900,
             41.4840, 41.6170, 41.2410, 41.9610, 41.9500,
             41.5470, 41.7980, 41.6260, 41.8770, 41.4480, 41.9770),
    lon  = c(33.7760, 34.0460, 34.2200, 33.7660, 33.0050,
             33.4640, 33.8440, 33.3190, 34.0140, 34.1970,
             34.4530, 33.7150, 33.0620, 33.4140, 33.6480, 34.0040),
    stringsAsFactors = FALSE
  ),
  "Kilis" = data.frame(
    name = c("Merkez", "Musabeyli", "Polateli", "Elbeyli"),
    lat  = c(36.7170, 36.9270, 36.8760, 36.6770),
    lon  = c(37.1180, 36.9130, 37.0780, 37.4930),
    stringsAsFactors = FALSE
  ),
  "Kırıkkale" = data.frame(
    name = c("Merkez", "Keskin", "Yahşihan", "Delice", "Sulakyurt",
             "Karakeçili", "Bahşili", "Balışeyh", "Çelebi"),
    lat  = c(39.8470, 39.6770, 39.8810, 40.0300, 40.1490, 39.5890, 39.8570, 40.0140, 39.4980),
    lon  = c(33.5150, 33.6120, 33.4790, 34.0260, 33.7060, 33.3850, 33.4110, 33.7010, 33.5870),
    stringsAsFactors = FALSE
  ),
  "Kırklareli" = data.frame(
    name = c("Merkez", "Lüleburgaz", "Babaeski", "Pınarhisar", "Vize",
             "Demirköy", "Pehlivanköy", "Kofçaz"),
    lat  = c(41.7370, 41.4050, 41.4340, 41.6260, 41.5720, 41.8250, 41.3570, 41.9430),
    lon  = c(27.2200, 27.3540, 27.0970, 27.5180, 27.7620, 27.7660, 26.9270, 27.1580),
    stringsAsFactors = FALSE
  ),
  "Kırşehir" = data.frame(
    name = c("Merkez", "Kaman", "Mucur", "Çiçekdağı", "Boztepe",
             "Akpınar", "Akçakent"),
    lat  = c(39.1460, 39.3540, 39.0630, 39.6110, 39.2460, 39.5530, 39.6170),
    lon  = c(34.1610, 33.7260, 34.3820, 34.4080, 34.4880, 33.8350, 34.1240),
    stringsAsFactors = FALSE
  ),
  "Kütahya" = data.frame(
    name = c("Merkez", "Tavşanlı", "Simav", "Gediz", "Emet",
             "Domaniç", "Aslanapa", "Hisarcık", "Şaphane", "Çavdarhisar", "Altıntaş"),
    lat  = c(39.4200, 39.5460, 39.0870, 39.0410, 39.3450, 39.8060, 39.2140, 39.2460, 38.9180, 39.2320, 39.0640),
    lon  = c(29.9840, 29.5010, 28.9780, 29.4080, 29.2650, 29.6040, 29.9290, 29.2340, 29.2300, 29.6250, 30.1310),
    stringsAsFactors = FALSE
  ),
  "Mardin" = data.frame(
    name = c("Artuklu", "Kızıltepe", "Midyat", "Nusaybin", "Mazıdağı",
             "Derik", "Yeşilli", "Savur", "Ömerli", "Dargeçit"),
    lat  = c(37.3120, 37.1900, 37.4180, 37.0750, 37.4670, 37.3670, 37.3390, 37.5300, 37.4010, 37.5470),
    lon  = c(40.7450, 40.5860, 41.3460, 41.2150, 40.4820, 40.2750, 40.7930, 40.8900, 40.9690, 41.7130),
    stringsAsFactors = FALSE
  ),
  "Muş" = data.frame(
    name = c("Merkez", "Bulanık", "Malazgirt", "Varto", "Korkut", "Hasköy"),
    lat  = c(38.7380, 39.0920, 39.1440, 39.1710, 38.7360, 38.6810),
    lon  = c(41.4980, 42.2730, 42.5350, 41.4520, 41.7640, 41.6820),
    stringsAsFactors = FALSE
  ),
  "Nevşehir" = data.frame(
    name = c("Merkez", "Avanos", "Ürgüp", "Hacıbektaş", "Acıgöl",
             "Gülşehir", "Derinkuyu", "Kozaklı"),
    lat  = c(38.6250, 38.7170, 38.6310, 38.9400, 38.5550, 38.7410, 38.3740, 39.2180),
    lon  = c(34.7130, 34.8460, 34.9130, 34.5570, 34.5240, 34.6260, 34.7330, 34.8040),
    stringsAsFactors = FALSE
  ),
  "Niğde" = data.frame(
    name = c("Merkez", "Bor", "Çamardı", "Ulukışla", "Çiftlik", "Altunhisar"),
    lat  = c(37.9660, 37.8930, 37.8290, 37.5460, 38.1140, 37.9870),
    lon  = c(34.6790, 34.5570, 34.9840, 34.4830, 34.4520, 34.3660),
    stringsAsFactors = FALSE
  ),
  "Ordu" = data.frame(
    name = c("Altınordu", "Ünye", "Fatsa", "Perşembe", "Mesudiye",
             "Korgan", "Akkuş", "Kumru", "Aybastı", "Ulubey",
             "Çatalpınar", "Gölköy", "Gürgentepe", "Kabadüz", "Kabataş"),
    lat  = c(40.9850, 41.1300, 41.0290, 41.0690, 40.4690,
             40.8340, 40.7930, 40.8720, 40.6890, 40.8720,
             40.9070, 40.7040, 40.7520, 40.8740, 40.7740),
    lon  = c(37.8790, 37.2880, 37.5000, 37.7760, 37.7760,
             37.3910, 36.9840, 37.2520, 37.3910, 37.7490,
             37.7410, 37.6170, 37.6790, 37.8290, 37.4640),
    stringsAsFactors = FALSE
  ),
  "Osmaniye" = data.frame(
    name = c("Merkez", "Kadirli", "Düziçi", "Bahçe", "Toprakkale",
             "Sumbas", "Hasanbeyli"),
    lat  = c(37.0750, 37.3760, 37.2950, 37.1960, 37.0660, 37.4700, 37.1310),
    lon  = c(36.2470, 36.0990, 36.7030, 36.5720, 36.1390, 36.4300, 36.5670),
    stringsAsFactors = FALSE
  ),
  "Rize" = data.frame(
    name = c("Merkez", "Pazar", "Ardeşen", "Çayeli", "Fındıklı",
             "Kalkandere", "İkizdere", "Çamlıhemşin", "Hemşin",
             "Güneysu", "Derepazarı", "İyidere"),
    lat  = c(41.0240, 41.1760, 41.1930, 41.0850, 41.2820,
             40.9450, 40.7780, 41.0520, 41.0580, 40.9570, 41.0250, 41.0300),
    lon  = c(40.5230, 40.8810, 41.0130, 40.7320, 41.1390,
             40.5080, 40.5550, 41.0290, 40.9170, 40.5910, 40.4200, 40.3800),
    stringsAsFactors = FALSE
  ),
  "Siirt" = data.frame(
    name = c("Merkez", "Kurtalan", "Pervari", "Eruh", "Baykan",
             "Şirvan", "Tillo"),
    lat  = c(37.9290, 37.9290, 37.9290, 37.7510, 38.1660, 38.0620, 37.9530),
    lon  = c(41.9400, 41.6870, 42.5460, 42.1810, 41.7780, 42.0290, 42.0380),
    stringsAsFactors = FALSE
  ),
  "Sinop" = data.frame(
    name = c("Merkez", "Boyabat", "Ayancık", "Gerze", "Türkeli",
             "Erfelek", "Saraydüzü", "Dikmen", "Durağan"),
    lat  = c(42.0300, 41.4690, 41.9430, 41.8190, 41.9430, 41.8900, 41.4800, 41.6540, 41.4220),
    lon  = c(35.1550, 34.7700, 34.5860, 35.1960, 34.3390, 34.9100, 34.6830, 34.9740, 35.0640),
    stringsAsFactors = FALSE
  ),
  "Şırnak" = data.frame(
    name = c("Merkez", "Cizre", "Silopi", "İdil", "Beytüşşebap",
             "Uludere", "Güçlükonak"),
    lat  = c(37.5210, 37.3270, 37.2460, 37.3430, 37.5610, 37.4710, 37.5280),
    lon  = c(42.4610, 42.1900, 42.4700, 41.8860, 43.1560, 42.7870, 41.9720),
    stringsAsFactors = FALSE
  ),
  "Tunceli" = data.frame(
    name = c("Merkez", "Pertek", "Mazgirt", "Pülümür", "Çemişgezek",
             "Hozat", "Ovacık", "Nazımiye"),
    lat  = c(39.1060, 39.1870, 39.0050, 39.4880, 39.0610, 39.1140, 39.3670, 39.1830),
    lon  = c(39.5470, 39.3180, 39.6040, 39.8920, 38.9110, 39.2130, 39.2430, 39.8310),
    stringsAsFactors = FALSE
  ),
  "Uşak" = data.frame(
    name = c("Merkez", "Banaz", "Eşme", "Karahallı", "Sivaslı", "Ulubey"),
    lat  = c(38.6790, 38.7370, 38.4050, 38.3280, 38.4990, 38.4210),
    lon  = c(29.4080, 29.7660, 28.9690, 29.5240, 29.6850, 29.2930),
    stringsAsFactors = FALSE
  ),
  "Yalova" = data.frame(
    name = c("Merkez", "Çiftlikköy", "Çınarcık", "Termal", "Altınova", "Armutlu"),
    lat  = c(40.6540, 40.6670, 40.6430, 40.6170, 40.6940, 40.5150),
    lon  = c(29.2760, 29.3270, 29.1310, 29.1980, 29.5080, 28.8400),
    stringsAsFactors = FALSE
  ),
  "Yozgat" = data.frame(
    name = c("Merkez", "Sorgun", "Yerköy", "Akdağmadeni", "Çekerek",
             "Boğazlıyan", "Şefaatli", "Sarıkaya", "Çayıralan",
             "Aydıncık", "Yenifakılı", "Saraykent", "Kadışehri"),
    lat  = c(39.8210, 39.8150, 39.6280, 39.6680, 40.0690,
             39.1930, 39.5160, 39.4930, 39.2860,
             40.1390, 39.2490, 39.7690, 39.9720),
    lon  = c(34.8080, 35.1810, 34.4750, 35.8820, 35.4990,
             35.2550, 34.6870, 35.4020, 35.6330,
             35.2810, 35.1610, 35.5200, 35.7980),
    stringsAsFactors = FALSE
  ),
  "Zonguldak" = data.frame(
    name = c("Merkez", "Ereğli", "Çaycuma", "Devrek", "Alaplı", "Gökçebey"),
    lat  = c(41.4560, 41.2820, 41.4300, 41.2150, 41.1840, 41.3190),
    lon  = c(31.7880, 31.4180, 32.0830, 31.9570, 31.3920, 31.9460),
    stringsAsFactors = FALSE
  )
)

# Türkçe karakterli anahtar/isimleri açıkça UTF-8 olarak işaretle
# (Windows R quirk: kaynak dosya UTF-8 olsa bile karşılaştırmalar
# bazen Latin1 olarak değerlendirilip eşleşme başarısız olur).
names(TR_DISTRICTS) <- enc2utf8(names(TR_DISTRICTS))
for (.k in names(TR_DISTRICTS)) {
  TR_DISTRICTS[[.k]]$name <- enc2utf8(TR_DISTRICTS[[.k]]$name)
}
rm(.k)

# Bölge bbox'ları (lon: minlon..maxlon, lat: minlat..maxlat)
WORLD_REGIONS <- list(
  "Tüm Dünya"  = NULL,
  "Türkiye"    = list(minlon = 25.5, maxlon = 45,  minlat = 35.5, maxlat = 43),
  "Avrupa"     = list(minlon = -25,  maxlon = 60,  minlat = 35,   maxlat = 72),
  "Asya"       = list(minlon = 25,   maxlon = 180, minlat = -10,  maxlat = 80),
  "K. Amerika" = list(minlon = -170, maxlon = -50, minlat = 7,    maxlat = 84),
  "G. Amerika" = list(minlon = -85,  maxlon = -33, minlat = -56,  maxlat = 13),
  "Afrika"     = list(minlon = -25,  maxlon = 60,  minlat = -35,  maxlat = 37),
  "Okyanusya"  = list(minlon = 110,  maxlon = 180, minlat = -50,  maxlat = -10)
)
names(WORLD_REGIONS) <- enc2utf8(names(WORLD_REGIONS))

# USGS FDSN query API ile tarihsel deprem listesi.
# bbox = list(minlon, maxlon, minlat, maxlat) ya da NULL (tüm dünya)
# center = list(lat, lon) + radius_km   — bbox ile birlikte verilmemeli
fetch_usgs_history <- function(starttime, endtime,
                                minmagnitude = 4,
                                bbox = NULL,
                                center = NULL, radius_km = NULL,
                                limit = 5000) {

  # KRİTİK: OutDec=',' (Türkçe konvansiyon) URL'deki sayısal parametreleri
  # "25,5" gibi yazar — USGS query API "25.5" bekler. Bu fonksiyon süresince
  # geçici olarak OutDec='.'; çıkışta eski değere dön.
  old_outdec <- getOption("OutDec")
  on.exit(options(OutDec = old_outdec), add = TRUE)
  options(OutDec = ".")

  # Direkt çağrı zinciri — do.call yerine; bazı httr2 sürümlerinde
  # do.call/list ile parametre splat'i sorun çıkarabiliyor.
  req <- request(USGS_QUERY) |>
    req_timeout(90) |>
    req_user_agent("R-Deprem-Haritasi/1.0") |>
    req_url_query(
      format       = "geojson",
      starttime    = format(as.Date(starttime), "%Y-%m-%d"),
      endtime      = format(as.Date(endtime),   "%Y-%m-%d"),
      minmagnitude = minmagnitude,
      limit        = limit,
      orderby      = "magnitude"
    )

  if (!is.null(bbox)) {
    req <- req |> req_url_query(
      minlongitude = bbox$minlon,
      maxlongitude = bbox$maxlon,
      minlatitude  = bbox$minlat,
      maxlatitude  = bbox$maxlat
    )
  }
  if (!is.null(center) && !is.null(radius_km)) {
    req <- req |> req_url_query(
      latitude    = center$lat,
      longitude   = center$lon,
      maxradiuskm = radius_km
    )
  }

  message("USGS sorgu URL: ", req$url)  # konsola sorgu URL'i (tanı için)

  resp <- tryCatch(req_perform(req),
    error = function(e) {
      message("USGS sorgu hatası: ", conditionMessage(e)); NULL
    })

  if (is.null(resp)) {
    message("USGS sorgu: yanıt alınamadı (network/timeout)")
    out <- empty_quake_df()
    attr(out, "fetch_status") <- "fail"
    return(out)
  }
  if (resp_status(resp) >= 400) {
    message("USGS sorgu HTTP hatası: ", resp_status(resp))
    out <- empty_quake_df()
    attr(out, "fetch_status") <- "fail"
    attr(out, "http_status") <- resp_status(resp)
    return(out)
  }

  raw <- tryCatch(
    fromJSON(resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) {
      message("USGS JSON parse hatası: ", conditionMessage(e)); NULL
    }
  )
  if (is.null(raw)) {
    out <- empty_quake_df()
    attr(out, "fetch_status") <- "fail"
    return(out)
  }

  feats <- raw$features
  n <- length(feats)
  if (n == 0) {
    out <- empty_quake_df()
    attr(out, "fetch_status") <- "empty"  # API yanıt verdi, sonuç yok
    return(out)
  }

  ids      <- character(n); mags     <- numeric(n)
  places   <- character(n); times    <- numeric(n)
  tsunamis <- integer(n);   sigs     <- numeric(n)
  urls     <- character(n); lons     <- numeric(n)
  lats     <- numeric(n);   depths   <- numeric(n)

  for (i in seq_len(n)) {
    f <- feats[[i]]
    p <- f$properties
    co <- tryCatch(f$geometry$coordinates, error = function(e) NULL)
    if (is.null(co) || length(co) < 2) co <- list(NA, NA, NA)
    ids[i]      <- f$id %||% NA_character_
    mags[i]     <- num_or_na(p$mag)
    places[i]   <- p$place %||% NA_character_
    times[i]    <- num_or_na(p$time) / 1000
    tsunamis[i] <- as.integer(p$tsunami %||% 0)
    sigs[i]     <- num_or_na(p$sig)
    urls[i]     <- p$url %||% NA_character_
    lons[i]     <- num_or_na(co[[1]])
    lats[i]     <- num_or_na(co[[2]])
    depths[i]   <- if (length(co) >= 3) num_or_na(co[[3]]) else NA_real_
  }

  df <- data.frame(
    id        = ids,
    mag       = mags,
    place     = places,
    time      = as.POSIXct(times, origin = "1970-01-01", tz = "UTC"),
    updated   = as.POSIXct(times, origin = "1970-01-01", tz = "UTC"),
    tsunami   = tsunamis,
    sig       = sigs,
    type      = "earthquake",
    url       = urls,
    lon       = lons,
    lat       = lats,
    depth_km  = depths,
    source    = "USGS",
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$lon) & is.finite(df$lat) &
           !is.na(df$time), , drop = FALSE]
  # UTF-8 işaretleme (Windows R quirk)
  for (col in c("id", "place", "type", "url")) {
    if (col %in% names(df)) Encoding(df[[col]]) <- "UTF-8"
  }
  df$continent <- classify_continent(df$lon, df$lat)
  df
}

# Geçmiş sorgu sekmesi için sade harita (overlays olmadan).
hist_base_map <- function() {
  tile_opts <- providerTileOptions(noWrap = TRUE)
  m <- leaflet(options = leafletOptions(
        worldCopyJump = FALSE, minZoom = 2)) |>
    setView(lng = 35, lat = 39, zoom = 4) |>
    addProviderTiles("Esri.WorldStreetMap", group = "Sokak",
                     options = tile_opts) |>
    addProviderTiles("Esri.WorldImagery",   group = "Uydu",
                     options = tile_opts) |>
    # Uydu için Esri'nin hibrit etiket katmanı (beyaz halo, okunaklı)
    addTiles(
      urlTemplate = paste0("https://server.arcgisonline.com/ArcGIS/rest/",
                           "services/Reference/World_Boundaries_and_Places/",
                           "MapServer/tile/{z}/{y}/{x}"),
      attribution = "Tiles &copy; Esri",
      group       = "Uydu",
      options     = tile_opts
    ) |>
    addProviderTiles("CartoDB.Voyager",     group = "Voyager",
                     options = tile_opts) |>
    addLayersControl(
      baseGroups = c("Sokak", "Voyager", "Uydu"),
      options = layersControlOptions(collapsed = TRUE)
    ) |>
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(imperial = FALSE))
  if (HAS_HEATMAP) {
    m <- m |>
      leaflet.extras::addFullscreenControl(position = "topleft") |>
      leaflet.extras::addResetMapButton()
  }
  m
}

# Birden çok kaynağı birleştir, ID üzerinden tekilleştir
merge_quake_sources <- function(...) {
  parts <- list(...)
  parts <- parts[!sapply(parts, function(x) is.null(x) || nrow(x) == 0)]
  if (length(parts) == 0) return(empty_quake_df())
  df <- do.call(rbind, parts)
  # Yakın zaman + yakın koordinat = aynı deprem; basit tekilleştirme
  df <- df[!duplicated(df[, c("id", "source")]), , drop = FALSE]
  df[order(df$time, decreasing = TRUE), , drop = FALSE]
}

empty_quake_df <- function() {
  data.frame(
    id = character(), mag = numeric(), place = character(),
    time = as.POSIXct(character(), tz = "UTC"),
    updated = as.POSIXct(character(), tz = "UTC"),
    tsunami = integer(), sig = numeric(), type = character(),
    url = character(), lon = numeric(), lat = numeric(),
    depth_km = numeric(), source = character(), continent = character(),
    stringsAsFactors = FALSE
  )
}

# Yaklaşık kıta sınıflandırması (lat/lon kutuları) — doğrulama paneli için
classify_continent <- function(lon, lat) {
  out <- rep("Okyanus / Diğer", length(lon))
  out[lat <= -60] <- "Antarktika"
  out[lon >= -170 & lon <= -50 & lat >=  7 & lat <= 84] <- "K. Amerika"
  out[lon >=  -85 & lon <= -33 & lat >= -56 & lat <= 13] <- "G. Amerika"
  out[lon >=  -25 & lon <=  60 & lat >= 36 & lat <= 72] <- "Avrupa"
  out[lon >=  -25 & lon <=  65 & lat >= -35 & lat <= 37] <- "Afrika"
  out[lon >=   25 & lon <= 180 & lat >= -10 & lat <= 80] <- "Asya"
  out[lon >=  110 & lon <= 180 & lat >= -50 & lat <= -10] <- "Okyanusya"
  out
}

filter_quakes <- function(df, min_mag = 0, max_depth = 700,
                          tsunami_only = FALSE) {
  if (nrow(df) == 0) return(df)
  out <- df[is.finite(df$mag) & df$mag >= min_mag, , drop = FALSE]
  out <- out[is.finite(out$depth_km) & out$depth_km <= max_depth, ,
             drop = FALSE]
  if (tsunami_only) out <- out[out$tsunami == 1, , drop = FALSE]
  out
}

# Türkçe göreli zaman: "3 dk önce", "5 sa önce" vb.
time_ago_tr <- function(t) {
  if (length(t) == 0) return(character(0))
  diff <- as.numeric(difftime(Sys.time(), t, units = "secs"))
  vapply(diff, function(d) {
    if (is.na(d) || !is.finite(d)) return("—")
    if (d < -60)       sprintf("%d sn sonra", as.integer(abs(d)))  # gerçekten gelecek
    else if (d < 10)   "şimdi"
    else if (d < 60)   sprintf("%d sn önce", as.integer(d))
    else if (d < 3600) sprintf("%d dk önce", as.integer(d / 60))
    else if (d < 86400) sprintf("%d sa önce", as.integer(d / 3600))
    else               sprintf("%d gün önce", as.integer(d / 86400))
  }, character(1))
}

# En yeni N depremi getir (zamana göre azalan)
latest_quakes <- function(df, n = 5) {
  if (nrow(df) == 0) return(df)
  out <- df[order(df$time, decreasing = TRUE), , drop = FALSE]
  out[seq_len(min(n, nrow(out))), , drop = FALSE]
}

# ---------------------------------------------------------------------
# Türkiye geçmiş depremleri (USGS query API, 1990 sonrası, M ≥ 4)
# Veri: data/turkey_history.csv — bir kez indirilmiş, yerel cache
# ---------------------------------------------------------------------

# CSV path resolution — çalışma dizini app klasörü olmayabilir (RStudio bazen
# proje köküne döner). Birkaç olası konumu sırayla dene.
resolve_turkey_history_path <- function() {
  candidates <- c(
    file.path("data", "turkey_history.csv"),
    file.path(getwd(), "data", "turkey_history.csv"),
    file.path(dirname(getwd()), "data", "turkey_history.csv"),
    # helpers.R yanındaki data/ klasörü (en güvenilir)
    tryCatch({
      h <- normalizePath(sys.frames()[[1]]$ofile %||% "", mustWork = FALSE)
      if (nzchar(h)) file.path(dirname(h), "data", "turkey_history.csv") else NA
    }, error = function(e) NA)
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) hit[1] else file.path("data", "turkey_history.csv")
}
TURKEY_HISTORY_PATH <- resolve_turkey_history_path()

turkey_history <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    # Path her çağrıda yeniden çöz — kullanıcı setwd() yapmış olabilir
    path <- if (file.exists(TURKEY_HISTORY_PATH)) {
      TURKEY_HISTORY_PATH
    } else {
      resolve_turkey_history_path()
    }
    if (!file.exists(path)) {
      message("turkey_history.csv bulunamadı. Aranan: ", path,
              " | Çalışma dizini: ", getwd())
      return(NULL)
    }
    df <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8"),
      error = function(e) {
        message("CSV okuma hatası: ", conditionMessage(e)); NULL
      }
    )
    if (is.null(df)) return(NULL)

    required_cols <- c("time_ms", "mag", "depth_km")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      message("CSV eksik sütunlar: ", paste(missing_cols, collapse = ", "),
              " — beklenen: time_ms, mag, place, depth_km, lat, lon")
      return(NULL)
    }
    if (nrow(df) == 0) {
      message("turkey_history.csv boş")
      return(NULL)
    }

    df$time <- as.POSIXct(df$time_ms / 1000,
                          origin = "1970-01-01", tz = "UTC")
    df$year <- as.integer(format(df$time, "%Y"))
    cached <<- df
    df
  }
})

# Yer adından şehir/ülke çıkar — USGS place stringi: "X km Y of <yer>, <ülke>"
extract_city <- function(place) {
  if (is.na(place) || !nzchar(place)) return("—")
  # Virgülden sonrasını al (genelde ülke)
  parts <- strsplit(place, ", ", fixed = TRUE)[[1]]
  if (length(parts) >= 2) {
    last <- parts[length(parts)]
    # "Türkiye" benzerlerini Türkçeleştir
    if (grepl("^Turkey$|^Türkiye$", last)) {
      first <- parts[1]
      first <- sub("^[0-9]+\\s*km\\s+\\S+\\s+of\\s+", "", first)
      return(first)
    }
    return(parts[1])
  }
  place
}

# Türkiye genel istatistikleri — 24h, M≥5 toplam, en büyük, ort derinlik
turkey_stats <- function(live_df = NULL) {
  hist <- turkey_history()
  if (is.null(hist)) {
    return(list(last24 = 0, total_m5 = 0, biggest = NULL,
                avg_depth = NA_real_, by_year = NULL))
  }

  # Son 24 saat — Kandilli (Türkiye odaklı) öncelikli; Kandilli yoksa
  # USGS verisinden Türkiye bbox'ı içindekileri say.
  last24 <- 0
  if (!is.null(live_df) && nrow(live_df) > 0) {
    cutoff <- Sys.time() - 24 * 3600
    in_24h <- !is.na(live_df$time) & live_df$time >= cutoff
    kand   <- in_24h & live_df$source == "Kandilli"
    if (any(kand)) {
      last24 <- sum(kand)
    } else {
      # Türkiye yaklaşık bbox: lon 25.5–45, lat 35.5–43
      tr_bbox <- in_24h &
                 !is.na(live_df$lon) & !is.na(live_df$lat) &
                 live_df$lon >= 25.5 & live_df$lon <= 45 &
                 live_df$lat >= 35.5 & live_df$lat <= 43
      last24 <- sum(tr_bbox)
    }
  }

  # M ≥ 5 toplam (Türkiye geçmiş 1990+)
  total_m5 <- sum(hist$mag >= 5.0, na.rm = TRUE)

  # En büyük deprem
  big_idx  <- which.max(hist$mag)
  biggest  <- if (length(big_idx) > 0) hist[big_idx, ] else NULL

  # Ortalama derinlik (M ≥ 4 verisinin tamamı)
  avg_depth <- mean(hist$depth_km, na.rm = TRUE)

  # Yıllık deprem sayıları
  by_year <- as.data.frame(table(hist$year))
  names(by_year) <- c("year", "n")
  by_year$year <- as.integer(as.character(by_year$year))

  list(last24 = last24, total_m5 = total_m5,
       biggest = biggest, avg_depth = avg_depth,
       by_year = by_year, history = hist)
}

# ---------------------------------------------------------------------
# Tarihi büyük depremler — küresel + Türkiye odaklı
# Kaynak: USGS, AFAD, akademik literatür (yaklaşık epicentre & rakamlar)
# ---------------------------------------------------------------------

HISTORIC_QUAKES <- data.frame(
  date     = as.Date(c(
    # Türkiye
    "1939-12-27", "1999-08-17", "1999-11-12", "2011-10-23",
    "2020-10-30", "2023-02-06", "2023-02-06",
    # Dünya — 20. yy
    "1906-04-18", "1908-12-28", "1923-09-01", "1960-05-22",
    "1964-03-27", "1976-07-28", "1985-09-19",
    # Dünya — 21. yy
    "2004-12-26", "2005-10-08", "2008-05-12", "2010-01-12",
    "2011-03-11", "2015-04-25", "2017-09-19"
  )),
  name = c(
    "Erzincan Depremi", "Marmara (İzmit) Depremi", "Düzce Depremi",
    "Van Depremi", "İzmir/Sisam Depremi",
    "Kahramanmaraş Pazarcık Depremi", "Kahramanmaraş Elbistan Depremi",
    "San Francisco Depremi", "Messina Depremi", "Büyük Kantō Depremi",
    "Valdivia (Şili) Depremi", "Alaska Depremi",
    "Tangshan (Çin) Depremi", "Mexico City Depremi",
    "Sumatra (Hint Okyanusu) Depremi", "Keşmir Depremi",
    "Siçuan (Çin) Depremi", "Haiti Depremi",
    "Tōhoku (Japonya) Depremi", "Gorkha (Nepal) Depremi",
    "Puebla (Meksika) Depremi"
  ),
  region = c(
    rep("Türkiye", 7),
    rep("Dünya", 14)
  ),
  lat = c(
    39.770, 40.748, 40.806, 38.628, 37.918, 37.166, 38.024,
    37.766, 38.150, 35.405, -38.143, 60.908, 39.605, 18.226,
    3.295, 34.539, 31.002, 18.443, 38.297, 28.230, 18.550
  ),
  lon = c(
    39.530, 29.864, 31.187, 43.486, 26.794, 37.042, 37.203,
    -122.481, 15.687, 139.080, -73.407, -147.339, 117.890, -102.573,
    95.982, 73.588, 103.322, -72.571, 142.373, 84.731, -98.498
  ),
  mag = c(
    7.8, 7.6, 7.2, 7.1, 7.0, 7.8, 7.5,
    7.9, 7.1, 7.9, 9.5, 9.2, 7.5, 8.0,
    9.1, 7.6, 7.9, 7.0, 9.1, 7.8, 7.1
  ),
  deaths = c(
    33000, 17480, 845, 604, 117, 53000, NA,
    3000, 75000, 142800, 5700, 131, 242769, 9500,
    227898, 87351, 87587, 222570, 19759, 8964, 370
  ),
  note = c(
    "Cumhuriyet tarihinin en yıkıcı depremlerinden biri",
    "Kuzey Anadolu Fay Zonu, modern Türkiye'nin dönüm noktası",
    "İzmit'in artçısı; KAFZ'da batıya ilerleme",
    "Doğu Anadolu Fay Zonu yakını",
    "Ege denizi, Sisam adası açıkları",
    "Doğu Anadolu Fay Zonu — 11 ilde büyük yıkım",
    "Aynı gün, Kahramanmaraş'ın ikincisi",
    "Modern sismolojinin başlangıcı",
    "20. yy'ın en yıkıcı Avrupa depremi",
    "Tokyo metropolünü neredeyse silen deprem",
    "Kayıtlı tarihte en büyük deprem (M9.5)",
    "Pasifik Halkası, mega-thrust",
    "Kayıt altındaki en ölümcül depremlerden",
    "Mexico City'de yapısal çöküşler",
    "Hint Okyanusu Tsunamisi — 14 ülkede yıkım",
    "Hindistan-Pakistan sınırı yakını",
    "Wenchuan ilçesi, Longmenshan Fayı",
    "Port-au-Prince başkenti yıkıldı",
    "Fukuşima nükleer kazasıyla bağlantılı",
    "Everest çığları, Katmandu vadisi",
    "1985 Mexico'nun aynı tarih yıldönümünde"
  ),
  stringsAsFactors = FALSE
)
# Tarih sıralı (yeniden eskiye)
HISTORIC_QUAKES <- HISTORIC_QUAKES[order(HISTORIC_QUAKES$date,
                                          decreasing = TRUE), ]
rownames(HISTORIC_QUAKES) <- NULL

# Lokalden bağımsız Türkçe tarih biçimi: "27 Aralık 1939"
TR_AYLAR <- c("Ocak", "Şubat", "Mart", "Nisan", "Mayıs", "Haziran",
              "Temmuz", "Ağustos", "Eylül", "Ekim", "Kasım", "Aralık")
format_date_tr <- function(d) {
  if (length(d) == 0) return(character(0))
  m <- as.integer(format(d, "%m"))
  sprintf("%s %s %s",
          format(d, "%d"),
          TR_AYLAR[m],
          format(d, "%Y"))
}

# Tarihi deprem popup'ı
historic_popup <- function(df) {
  sprintf(
    paste0(
      "<div style='font-family:Inter,Segoe UI,sans-serif;min-width:240px'>",
      "<div style='font-size:11px;color:#888;letter-spacing:.5px'>%s</div>",
      "<div style='font-size:16px;font-weight:700;color:#1f2937;",
      "margin:2px 0 6px 0'>%s</div>",
      "<div style='display:flex;gap:8px;align-items:center;margin:6px 0'>",
      "<span style='background:#fbbf24;color:#0f172a;padding:2px 8px;",
      "border-radius:6px;font-weight:700;font-size:13px'>M %.1f</span>",
      "<span style='background:#f3f4f6;color:#374151;padding:2px 8px;",
      "border-radius:6px;font-size:11px'>%s</span>",
      "</div>",
      "%s",
      "<div style='font-size:12px;color:#475569;margin-top:6px;",
      "line-height:1.4'>%s</div>",
      "</div>"
    ),
    format_date_tr(df$date),
    df$name,
    df$mag,
    df$region,
    ifelse(is.na(df$deaths),
           "",
           sprintf(
             "<div style='font-size:12px;color:#dc2626'><b>Kayıp:</b> %s</div>",
             format(df$deaths, big.mark = ".", decimal.mark = ","))),
    df$note
  )
}

# ---------------------------------------------------------------------
# Global tektonik plaka sınırları (Bird 2003 / Mueller PB2002 modeli)
# Verisi: data/plate_boundaries.geojson — Fraxen/tectonicplates GitHub deposundan
# ---------------------------------------------------------------------
plate_boundaries_sf <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- file.path("data", "plate_boundaries.geojson")
    if (!file.exists(path)) {
      message("plate_boundaries.geojson yok — global plaka katmanı atlanır.")
      return(NULL)
    }
    res <- tryCatch(
      sf::st_read(path, quiet = TRUE),
      error = function(e) { message("Plaka veri okuma hatası: ", e$message); NULL }
    )
    cached <<- res
    res
  }
})

# Her deprem için en yakın global plaka sınırı (=fay) ve mesafe (km)
nearest_fault <- function(df, faults = NULL) {
  if (nrow(df) == 0) {
    return(transform(df, fault_name = character(0),
                         fault_dist_km = numeric(0)))
  }
  if (is.null(faults)) faults <- plate_boundaries_sf()
  if (is.null(faults)) {
    df$fault_name    <- NA_character_
    df$fault_dist_km <- NA_real_
    return(df)
  }

  pts <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  d_mat <- suppressWarnings(st_distance(pts, faults))
  d_km <- as.numeric(d_mat) / 1000
  d_km <- matrix(d_km, nrow = nrow(pts))
  idx  <- max.col(-d_km, ties.method = "first")
  # PB2002 GeoJSON'unda PlateA/PlateB sütunları var; insan-okur isim üret.
  fname <- if (all(c("PlateA", "PlateB") %in% names(faults))) {
    paste0(faults$PlateA, " ↔ ", faults$PlateB)
  } else if ("Name" %in% names(faults)) {
    as.character(faults$Name)
  } else if ("name" %in% names(faults)) {
    as.character(faults$name)
  } else {
    rep("Plaka sınırı", nrow(faults))
  }
  df$fault_name    <- fname[idx]
  df$fault_dist_km <- d_km[cbind(seq_len(nrow(df)), idx)]
  df
}

# ---------------------------------------------------------------------
# Görselleştirme
# ---------------------------------------------------------------------

# Büyüklük kategorisi → renk (USGS sismik renk skalası — standart)
QUAKE_COLORS <- c(
  "#22c55e",   # < 2.5    yeşil       — mikro/zayıf
  "#84cc16",   # 2.5-4    açık yeşil  — hafif
  "#facc15",   # 4-5      sarı        — orta
  "#f97316",   # 5-6      turuncu     — güçlü
  "#dc2626",   # 6-7      kırmızı     — şiddetli
  "#7f1d1d"    # 7+       koyu kırmızı— yıkıcı
)
QUAKE_BINS <- c(0, 2.5, 4, 5, 6, 7, 10)

mag_color <- function(mag) {
  # 6 renk → 6 aralık: -Inf,2.5 / 2.5,4 / 4,5 / 5,6 / 6,7 / 7,Inf
  idx <- as.integer(cut(mag,
                        breaks = c(-Inf, 2.5, 4, 5, 6, 7, Inf),
                        include.lowest = TRUE, right = FALSE))
  out <- QUAKE_COLORS[idx]
  out[is.na(out)] <- "#999999"
  out
}

quake_palette <- function() {
  colorBin(
    palette = QUAKE_COLORS,
    bins    = QUAKE_BINS,
    domain  = c(0, 10),
    na.color = "#999999"
  )
}

# Logaritmik yarıçap (büyük depremler belirgin görünsün)
mag_radius <- function(mag) {
  m <- ifelse(is.na(mag) | mag < 0, 0, mag)
  4 + (1.6 ^ m)
}

# Büyüklüğe göre kategori metni (Türkçe)
mag_category <- function(mag) {
  vapply(mag, function(m) {
    if (is.na(m))         "—"
    else if (m < 2.5)     "Mikro"
    else if (m < 4)       "Hafif"
    else if (m < 5)       "Orta"
    else if (m < 6)       "Güçlü"
    else if (m < 7)       "Şiddetli"
    else                  "Yıkıcı"
  }, character(1))
}

# Tahmini enerji (Joule) — sismolojik formül: log10(E) = 1.5*M + 4.8
mag_energy_text <- function(mag) {
  vapply(mag, function(m) {
    if (is.na(m) || m < 0) return("—")
    e <- 10 ^ (1.5 * m + 4.8)
    if (e < 1e9)        sprintf("%.1f MJ", e / 1e6)
    else if (e < 1e12)  sprintf("%.1f GJ", e / 1e9)
    else if (e < 1e15)  sprintf("%.1f TJ", e / 1e12)
    else                sprintf("%.1f PJ", e / 1e15)
  }, character(1))
}

quake_popup <- function(df) {
  if (nrow(df) == 0) return(character(0))

  cols   <- mag_color(df$mag)
  cats   <- mag_category(df$mag)
  ago    <- time_ago_tr(df$time)
  utc    <- format(df$time, "%H:%M:%S",   tz = "UTC")
  utcdt  <- format(df$time, "%d.%m.%Y", tz = "UTC")
  trtime <- format(df$time, "%H:%M:%S",  tz = "Europe/Istanbul")
  energy <- mag_energy_text(df$mag)

  src_badge <- ifelse(df$source == "Kandilli",
    "<span class='qp-badge qp-badge-kandilli'>KANDİLLİ</span>",
    "<span class='qp-badge qp-badge-usgs'>USGS</span>")

  place_html <- ifelse(is.na(df$place), "Bilinmeyen konum", df$place)

  has_fault <- !is.na(df$fault_name) & !is.na(df$fault_dist_km)
  fault_html <- ifelse(has_fault,
    sprintf("<b>%s</b> &middot; %s km uzakta",
            df$fault_name,
            formatC(round(df$fault_dist_km, 0),
                    big.mark = ".", decimal.mark = ",",
                    format = "d")),
    "<span class='qp-muted'>Hesaplanamadı</span>")

  tsunami_html <- ifelse(df$tsunami == 1,
    "<div class='qp-warning'><span>&#9888;</span> Tsunami uyarısı aktif</div>",
    "")

  link_text <- ifelse(df$source == "Kandilli",
                      "Kandilli detay sayfası", "USGS detay sayfası")

  sprintf(paste0(
    "<div class='qp'>",
      # ---- Header: mag rozet + meta ----
      "<div class='qp-header'>",
        "<div class='qp-mag' style='background:%s;box-shadow:0 4px 14px %s55'>",
          "<div class='qp-mag-label'>BÜYÜKLÜK</div>",
          "<div class='qp-mag-value'>%.1f</div>",
        "</div>",
        "<div class='qp-meta'>",
          "<div class='qp-meta-row'>",
            "<span class='qp-cat' style='color:%s'>%s</span>",
            "<span class='qp-dot'>&middot;</span>",
            "<span class='qp-ago'>%s</span>",
          "</div>",
          "<div class='qp-place'>%s</div>",
          "<div class='qp-src'>%s</div>",
        "</div>",
      "</div>",

      # ---- Grid: 4 hücre ----
      "<div class='qp-grid'>",
        "<div class='qp-cell'>",
          "<div class='qp-key'>DERİNLİK</div>",
          "<div class='qp-val'>%.1f <span class='qp-unit'>km</span></div>",
        "</div>",
        "<div class='qp-cell'>",
          "<div class='qp-key'>ENERJİ</div>",
          "<div class='qp-val'>%s</div>",
        "</div>",
        "<div class='qp-cell'>",
          "<div class='qp-key'>YEREL (TR)</div>",
          "<div class='qp-val qp-mono'>%s</div>",
          "<div class='qp-sub'>%s UTC</div>",
        "</div>",
        "<div class='qp-cell'>",
          "<div class='qp-key'>KOORDİNAT</div>",
          "<div class='qp-val qp-mono qp-small'>%.3f, %.3f</div>",
          "<div class='qp-sub'>%s</div>",
        "</div>",
      "</div>",

      # ---- Fay satırı ----
      "<div class='qp-fault'>",
        "<div class='qp-key'>EN YAKIN PLAKA SINIRI</div>",
        "<div class='qp-val-sm'>%s</div>",
      "</div>",

      # ---- Tsunami uyarısı (varsa) ----
      "%s",

      # ---- Footer link ----
      "<div class='qp-footer'>",
        "<a href='%s' target='_blank' class='qp-link' style='color:%s'>%s &rarr;</a>",
      "</div>",
    "</div>"
  ),
  cols, cols,                          # mag rozet bg + glow
  df$mag,                              # mag value
  cols, cats,                          # category text + color
  ago,                                 # time ago
  place_html,                          # place name
  src_badge,                           # source badge
  df$depth_km,                         # depth
  energy,                              # energy
  trtime, utc,                         # local time + utc
  df$lat, df$lon, utcdt,               # coords + utc date
  fault_html,                          # nearest fault
  tsunami_html,                        # tsunami warning
  df$url, cols, link_text              # link
  )
}

# Temel harita iskeleti — renkli ve canlı tasarım
base_map <- function() {
  # Tile sınırları: sadece dünya içinde tile iste (boş alanlar için
  # Esri'nin "Map data not yet available" yanıtını engelle)
  tile_opts <- providerTileOptions(
    noWrap = TRUE,
    bounds = list(c(-85.05, -180), c(85.05, 180))
  )

  m <- leaflet(options = leafletOptions(
        worldCopyJump = FALSE, minZoom = 2,
        zoomControl = TRUE, dragging = TRUE,
        maxBoundsViscosity = 1.0)) |>
    setView(lng = 20, lat = 25, zoom = 2.5) |>
    setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>

    # Altlıklar (Esri Sokak varsayılan — derste kullanılan)
    addProviderTiles("Esri.WorldStreetMap",  group = "Sokak (Esri)",
                     options = tile_opts) |>
    addProviderTiles("Esri.NatGeoWorldMap",  group = "National Geographic",
                     options = tile_opts) |>
    addProviderTiles("CartoDB.Voyager",      group = "Voyager",
                     options = tile_opts) |>
    addProviderTiles("OpenStreetMap.Mapnik", group = "OSM",
                     options = tile_opts) |>
    addProviderTiles("Esri.WorldImagery",    group = "Uydu",
                     options = tile_opts) |>
    # Uydu için Esri'nin hibrit etiket katmanı — beyaz halo, satellite
    # üzerinde okunaklı, ülke/şehir/yer adları
    addTiles(
      urlTemplate = paste0("https://server.arcgisonline.com/ArcGIS/rest/",
                           "services/Reference/World_Boundaries_and_Places/",
                           "MapServer/tile/{z}/{y}/{x}"),
      attribution = "Tiles &copy; Esri",
      group       = "Uydu",
      options     = tile_opts
    ) |>

    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(imperial = FALSE)) |>
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      toggleDisplay = TRUE, minimized = FALSE,
      position = "bottomright", width = 140, height = 100
    )

  # Tarihi büyük depremler (varsayılan kapalı; kullanıcı checkbox ile açar)
  hist_radius <- 9 + (HISTORIC_QUAKES$mag - 6) * 3
  hist_color  <- mag_color(HISTORIC_QUAKES$mag)
  m <- m |> addCircleMarkers(
    lng         = HISTORIC_QUAKES$lon,
    lat         = HISTORIC_QUAKES$lat,
    radius      = hist_radius,
    color       = "#ffffff",
    weight      = 2,
    fillColor   = hist_color,
    fillOpacity = 0.92,
    popup       = historic_popup(HISTORIC_QUAKES),
    label       = sprintf("%.1f", HISTORIC_QUAKES$mag),
    labelOptions = labelOptions(
      noHide = TRUE, direction = "center", textOnly = TRUE,
      style = list("color" = "#ffffff",
                   "font-weight" = "800",
                   "font-size" = "11px",
                   "text-shadow" = "0 0 3px rgba(0,0,0,0.85), 0 1px 1px rgba(0,0,0,0.6)")
    ),
    options     = pathOptions(className = "historic-marker"),
    group       = "Tarihi Depremler"
  )

  # Dünya fay hatları (global tektonik plaka sınırları)
  pb <- plate_boundaries_sf()
  if (!is.null(pb)) {
    m <- m |> addPolylines(
      data        = pb,
      color       = "#d32f2f",
      weight      = 2.5,
      opacity     = 0.85,
      label       = ~paste0("Plaka sınırı: ", PlateA, " ↔ ", PlateB),
      labelOptions = labelOptions(
        style = list("font-weight" = "700", "color" = "#d32f2f")),
      popup       = ~sprintf(
        "<b>Plaka sınırı</b><br/>%s ↔ %s<br/><small>%s</small>",
        PlateA, PlateB, ifelse(is.na(Type) | Type == "", "—", Type)),
      group       = "Dünya Fay Hatları"
    )
  }

  if (HAS_HEATMAP) {
    m <- m |>
      leaflet.extras::addFullscreenControl(position = "topleft") |>
      leaflet.extras::addResetMapButton()
  }

  overlays <- c("Depremler", "Tarihi Depremler", "Dünya Fay Hatları")
  if (HAS_HEATMAP) overlays <- c(overlays, "Isı Haritası")

  # collapsed = TRUE: küçük ekranlarda (≤15") katman paneli sürekli açık
  # kaldığında deprem etiketleri/büyüklük baloncuklarıyla çakışıyordu.
  # Artık küçük ikon — üzerine gelince / tıklayınca açılır.
  m <- addLayersControl(
    m,
    baseGroups = c("Sokak (Esri)", "National Geographic", "Voyager",
                   "OSM", "Uydu"),
    overlayGroups = overlays,
    options = layersControlOptions(collapsed = TRUE)
  )
  # Tüm opsiyonel overlay katmanlar varsayılan kapalı (kullanıcı seçer)
  m <- hideGroup(m, "Dünya Fay Hatları")
  m <- hideGroup(m, "Tarihi Depremler")
  if (HAS_HEATMAP) m <- hideGroup(m, "Isı Haritası")
  m
}

# Depremleri haritaya ekle (performans optimize edilmiş)
# - 200+ kayıt: kümeleme zorla aktif (büyük performans kazancı)
# - 600+ kayıt: halo katmanı atlanır
# - Pulse animasyonu max 30 noktayla sınırlı (büyüklük sıralı)
draw_quakes <- function(map_proxy, df, cluster = FALSE,
                        pulse_threshold = 4.5) {
  pal <- quake_palette()
  n   <- nrow(df)

  # NOT: clearControls() katman kontrolü, ölçek ve mini haritayı da silerdi.
  # Sadece legend'i layerId üzerinden değiştir.
  m <- map_proxy |>
    clearGroup("Depremler") |>
    clearGroup("Pulse") |>
    clearGroup("Isı Haritası") |>
    removeControl("mag_legend")

  if (n == 0) return(m)

  # Yoğun veride otomatik kümeleme (kullanıcı seçimini geçersiz kılar)
  auto_cluster <- cluster || n > 200
  draw_halo    <- n <= 600 && !auto_cluster

  cluster_opts <- if (auto_cluster) markerClusterOptions(
    showCoverageOnHover = FALSE,
    spiderfyOnMaxZoom   = TRUE,
    maxClusterRadius    = 50,
    chunkedLoading      = TRUE
  ) else NULL

  # Animasyonlu halka — büyüklüğe göre sıralı, en fazla 30 nokta
  pulse_df <- df[is.finite(df$mag) & df$mag >= pulse_threshold, ,
                 drop = FALSE]
  pulse_df <- pulse_df[order(-pulse_df$mag), , drop = FALSE]
  if (nrow(pulse_df) > 30) pulse_df <- pulse_df[seq_len(30), ]

  if (nrow(pulse_df) > 0 && !auto_cluster) {
    m <- m |>
      addCircleMarkers(
        data        = pulse_df,
        lng         = ~lon, lat = ~lat,
        radius      = ~mag_radius(mag) * 1.4,
        color       = ~pal(mag),
        weight      = 2,
        opacity     = 0.9,
        fill        = FALSE,
        options     = pathOptions(className = "pulse-ring"),
        group       = "Pulse"
      )
  }

  # Halo katmanı (yalnızca veri seyrekken)
  if (draw_halo) {
    m <- m |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon, lat = ~lat,
        radius      = ~mag_radius(mag) * 1.8,
        color       = ~pal(mag),
        stroke      = TRUE, weight = 1.5,
        opacity     = 0.40,
        fillOpacity = 0.10,
        options     = pathOptions(className = "quake-halo"),
        group       = "Depremler"
      )
  }

  # Tüm depremler tek katmanda — kümeleme açıkken etiket gizli
  # (Cluster modunda her noktanın etiketi anlamsız olur)
  if (auto_cluster) {
    m <- m |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon, lat = ~lat,
        radius      = ~mag_radius(mag),
        color       = "#ffffff",
        weight      = 1.2,
        fillColor   = ~pal(mag),
        fillOpacity = 0.92,
        options     = pathOptions(className = "quake-glow"),
        popup       = quake_popup(df),
        popupOptions = popupOptions(closeButton = TRUE,
                              autoPan = TRUE,
                              autoPanPadding = c(20, 20),
                              keepInView = TRUE,
                              maxWidth = 300, maxHeight = 420),
        label       = ~sprintf("M%.1f — %s", mag, place),
        labelOptions = labelOptions(
          style = list("font-weight" = "600",
                       "background-color" = "rgba(255,255,255,0.95)")),
        clusterOptions = cluster_opts,
        group       = "Depremler"
      )
  } else {
    m <- m |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon, lat = ~lat,
        radius      = ~mag_radius(mag),
        color       = "#ffffff",
        weight      = 1.2,
        fillColor   = ~pal(mag),
        fillOpacity = 0.92,
        options     = pathOptions(className = "quake-glow"),
        popup       = quake_popup(df),
        popupOptions = popupOptions(closeButton = TRUE,
                              autoPan = TRUE,
                              autoPanPadding = c(20, 20),
                              keepInView = TRUE,
                              maxWidth = 300, maxHeight = 420),
        label       = ~sprintf("%.1f", mag),
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list("color" = "#ffffff",
                       "font-weight" = "800",
                       "font-size" = "10px",
                       "text-shadow" = "0 0 3px rgba(0,0,0,0.95), 0 1px 1px rgba(0,0,0,0.7)")
        ),
        group       = "Depremler"
      )
  }

  if (HAS_HEATMAP) {
    # Magnitude değerlerini 0-1 aralığına normalize et (heatmap için)
    intensity_vals <- pmax(df$mag, 0, na.rm = TRUE)
    intensity_norm <- intensity_vals / max(c(intensity_vals, 8), na.rm = TRUE)
    m <- leaflet.extras::addHeatmap(
      m,
      lng       = df$lon,
      lat       = df$lat,
      intensity = intensity_norm,
      blur      = 18,
      max       = 1.0,
      radius    = 22,
      minOpacity = 0.4,
      group     = "Isı Haritası"
    )
  }

  addLegend(
    m,
    position  = "bottomright",
    pal       = pal,
    values    = c(0, 2.5, 4, 5, 6, 7, 10),
    title     = "Büyüklük (M)",
    opacity   = 0.95,
    layerId   = "mag_legend",
    # big.mark='.' (varsayılan ',' OutDec=',' ile çakışır → uyarı verir)
    labFormat = labelFormat(digits = 1, big.mark = ".")
  )
}
