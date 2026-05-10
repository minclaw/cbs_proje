# app.R — Dünya Deprem Haritası
# Çalıştırma: RStudio'da bu dosyayı aç ve sağ üstteki "Run App" butonuna bas.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(DT)
  library(dplyr)
  library(plotly)
  library(jsonlite)
  library(sf)
})

# Türkçe konvansiyon: ondalık ayracı ',' — R'nin varsayılan format() davranışı
# big.mark='.' ile çakışmasın diye genel ayar. Sprintf etkilenmez (%f hep '.').
options(OutDec = ",")

# helpers.R'ı app.R'ın bulunduğu klasörden bul (çalışma dizininden bağımsız)
# Bir kullanıcı RStudio'dan, başkası terminal R'den çalıştırabiliyor; çoklu
# fallback ile script konumu güvenilir şekilde bulunsun.
local({
  candidates <- character(0)

  # 1) RStudio editör bağlamı
  rs_dir <- tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      ctx <- rstudioapi::getSourceEditorContext()
      if (!is.null(ctx) && nzchar(ctx$path)) dirname(ctx$path) else NA_character_
    } else NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(rs_dir)) candidates <- c(candidates, rs_dir)

  # 2) source() ile çağrıldıysa sys.frames içinde $ofile bulunur
  for (f in rev(sys.frames())) {
    of <- tryCatch(f$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) {
      candidates <- c(candidates, dirname(normalizePath(of, mustWork = FALSE)))
      break
    }
  }

  # 3) Rscript ile çalıştırıldıysa --file=... parametresi
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) {
    candidates <- c(candidates,
                    dirname(normalizePath(sub("^--file=", "", fa[1]),
                                          mustWork = FALSE)))
  }

  # 4) Son çare: çalışma dizini
  candidates <- c(candidates, getwd())

  helpers <- NULL
  for (d in candidates) {
    p <- file.path(d, "helpers.R")
    if (file.exists(p)) { helpers <- p; break }
  }

  if (is.null(helpers)) {
    stop("\n\n[Dünya Deprem Haritası] 'helpers.R' bulunamadı.\n",
         "Beklenen yer: app.R ile aynı klasör. Aranan dizinler:\n  - ",
         paste(candidates, collapse = "\n  - "),
         "\n\nÇözüm:\n",
         "  1) helpers.R dosyasının app.R ile AYNI klasörde olduğunu doğrula.\n",
         "  2) RStudio'da Session > Set Working Directory > To Source File Location.\n",
         "  3) Proje arşivini eksiksiz indirip yeniden çıkar.\n",
         call. = FALSE)
  }

  source(helpers, chdir = TRUE)
})

# ---- Tema --------------------------------------------------------------

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#0891b2",   # buzul cyan (glacier)
  success    = "#10b981",   # emerald
  warning    = "#f59e0b",   # amber
  danger     = "#e11d48",   # rose
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

# Özel CSS (kart kenarları, gölge, başlık şeridi, harita pulse animasyonu)
app_css <- HTML(
  "/* === Animasyonlu yumuşak gradient arka plan === */
   body {
     background: linear-gradient(135deg,
       #dbeafe 0%, #e0e7ff 20%, #ede9fe 40%,
       #fce7f3 60%, #e0e7ff 80%, #dbeafe 100%);
     background-size: 400% 400%;
     animation: bgShift 32s ease infinite;
     min-height: 100vh;
   }
   @keyframes bgShift {
     0%   { background-position:   0% 50%; }
     50%  { background-position: 100% 50%; }
     100% { background-position:   0% 50%; }
   }

   /* === Navbar: saydam buzul cyan + frosted glass === */
   .navbar {
     background: linear-gradient(120deg,
       rgba(8, 145, 178, 0.82)   0%,
       rgba(14, 165, 233, 0.78)  50%,
       rgba(56, 189, 248, 0.72) 100%) !important;
     backdrop-filter: blur(18px) saturate(180%);
     -webkit-backdrop-filter: blur(18px) saturate(180%);
     box-shadow: 0 4px 24px rgba(8, 145, 178, 0.18),
                 inset 0 -1px 0 rgba(255, 255, 255, 0.25);
     border-bottom: 1px solid rgba(255, 255, 255, 0.35);
   }
   .navbar-brand {
     color: #fff !important;
     font-weight: 700;
     letter-spacing: .3px;
     text-shadow: 0 2px 8px rgba(8, 47, 73, 0.35);
   }

   /* === Sekme görünürlüğü (Anasayfa / Harita / Tablo / vs.) === */
   /* Pasif sekme: hafif soluk beyaz */
   .navbar .nav-link {
     color: rgba(255, 255, 255, 0.78) !important;
     font-weight: 500;
     padding: 8px 14px !important;
     border-radius: 8px;
     margin: 0 3px;
     transition: background .2s ease, color .2s ease,
                 transform .15s ease;
   }
   .navbar .nav-link:hover {
     color: #ffffff !important;
     background: rgba(255, 255, 255, 0.15) !important;
     transform: translateY(-1px);
   }
   /* Aktif sekme: belirgin pill, parlak beyaz yazı */
   .navbar .nav-link.active,
   .navbar .nav-link[aria-selected='true'],
   .navbar .nav-link[aria-current='page'] {
     color: #ffffff !important;
     background: rgba(255, 255, 255, 0.28) !important;
     font-weight: 700 !important;
     box-shadow: 0 2px 10px rgba(255, 255, 255, 0.18),
                 inset 0 1px 0 rgba(255, 255, 255, 0.4);
     text-shadow: 0 1px 3px rgba(8, 47, 73, 0.3);
   }
   /* Sekme ikonları */
   .navbar .nav-link i, .navbar .nav-link svg {
     color: inherit !important;
     -webkit-text-fill-color: inherit !important;
     opacity: .9;
   }
   .navbar .nav-link.active i,
   .navbar .nav-link.active svg {
     opacity: 1;
   }

   /* === Modernleştirilmiş butonlar === */
   .btn-primary {
     background: linear-gradient(135deg, #0891b2 0%, #06b6d4 50%, #22d3ee 100%) !important;
     border: none !important;
     box-shadow: 0 4px 14px rgba(8, 145, 178, 0.32),
                 inset 0 1px 0 rgba(255, 255, 255, 0.25);
     transition: transform .18s cubic-bezier(.34,1.56,.64,1),
                 box-shadow .25s ease;
     font-weight: 600;
     letter-spacing: .2px;
   }
   .btn-primary:hover, .btn-primary:focus {
     background: linear-gradient(135deg, #0e7490 0%, #06b6d4 50%, #67e8f9 100%) !important;
     box-shadow: 0 8px 24px rgba(8, 145, 178, 0.45),
                 inset 0 1px 0 rgba(255, 255, 255, 0.3);
     transform: translateY(-1px);
   }
   .btn-primary:active {
     transform: translateY(0);
     box-shadow: 0 2px 8px rgba(8, 145, 178, 0.4) !important;
   }

   /* Açık ton (hızlı bölge) butonları — cam tarz */
   .btn-light {
     background: rgba(255, 255, 255, 0.7) !important;
     backdrop-filter: blur(8px);
     -webkit-backdrop-filter: blur(8px);
     border: 1px solid rgba(8, 145, 178, 0.18) !important;
     color: #0e7490 !important;
     font-weight: 600;
     transition: all .2s ease;
   }
   .btn-light:hover {
     background: rgba(207, 250, 254, 0.85) !important;
     border-color: rgba(8, 145, 178, 0.35) !important;
     color: #0c4a6e !important;
     transform: translateY(-1px);
     box-shadow: 0 4px 14px rgba(8, 145, 178, 0.18);
   }

   /* Outline butonlar (preset tarih) */
   .btn-outline-secondary {
     background: rgba(255, 255, 255, 0.55) !important;
     backdrop-filter: blur(6px);
     border: 1px solid rgba(8, 145, 178, 0.22) !important;
     color: #0e7490 !important;
     font-weight: 600;
     transition: all .2s ease;
   }
   .btn-outline-secondary:hover {
     background: linear-gradient(135deg, #0891b2, #06b6d4) !important;
     border-color: transparent !important;
     color: #fff !important;
     transform: translateY(-1px);
     box-shadow: 0 4px 14px rgba(8, 145, 178, 0.32);
   }

   /* === Cam efekti (glassmorphism) kartlar === */
   .card {
     border: 1px solid rgba(255, 255, 255, 0.5) !important;
     background: rgba(255, 255, 255, 0.62) !important;
     backdrop-filter: blur(14px) saturate(180%);
     -webkit-backdrop-filter: blur(14px) saturate(180%);
     box-shadow: 0 8px 32px rgba(15, 23, 42, 0.08);
     border-radius: 14px !important;
     transition: transform .25s ease, box-shadow .25s ease;
   }
   .card:hover {
     box-shadow: 0 14px 40px rgba(15, 23, 42, 0.12);
   }
   .card-header {
     background: rgba(255, 255, 255, 0.35) !important;
     border-bottom: 1px solid rgba(255, 255, 255, 0.5) !important;
   }
   .leaflet-container {
     border-radius: 12px;
     background: #eef2f6 !important;
   }
   /* Tile image hatası ya da alt-text görünür kalmasın (boş alanlar saydam) */
   .leaflet-tile-pane img.leaflet-tile-loaded { opacity: 1 !important; }
   .leaflet-tile-pane img:not(.leaflet-tile-loaded) {
     opacity: 0 !important;
     visibility: hidden !important;
   }
   .leaflet-tile-error { display: none !important; }
   /* Tile container üzerindeki boş overlay görüntüleri gizle */
   .leaflet-pane > img:not([src]) {
     display: none !important;
   }
   .info-banner {
     background: rgba(207, 250, 254, 0.65);
     backdrop-filter: blur(10px) saturate(180%);
     -webkit-backdrop-filter: blur(10px) saturate(180%);
     color: #155e75;
     padding: 10px 14px;
     border-left: 4px solid #06b6d4;
     border-radius: 10px;
     font-size: 13px;
     box-shadow: 0 2px 12px rgba(8, 145, 178, 0.08);
   }

   /* Yan paneldeki yanıp sönen canlı durum noktası (emerald) */
   .live-dot { display:inline-block; width:10px; height:10px;
               border-radius:50%; background:#10b981; margin-right:6px;
               box-shadow: 0 0 0 0 rgba(16, 185, 129, .7);
               animation: livePulse 1.6s infinite; }
   @keyframes livePulse {
     0%   { box-shadow: 0 0 0 0   rgba(16, 185, 129, .7); }
     70%  { box-shadow: 0 0 0 10px rgba(16, 185, 129, 0); }
     100% { box-shadow: 0 0 0 0   rgba(16, 185, 129, 0); }
   }

   /* Harita üzerinde büyük depremler için yayılan dalga animasyonu */
   .pulse-ring {
     fill: transparent !important;
     animation: ringPulse 1.8s cubic-bezier(.4,.0,.6,1) infinite;
     transform-origin: center;
   }
   @keyframes ringPulse {
     0%   { stroke-opacity: 0.95; stroke-width: 1.5; }
     70%  { stroke-opacity: 0;    stroke-width: 14;  }
     100% { stroke-opacity: 0;    stroke-width: 14;  }
   }

   /* Marker parıltı efekti — hafif glow (performans dostu) */
   .quake-glow path {
     filter: drop-shadow(0 0 3px currentColor);
   }
   /* Tarihi deprem markerları — altın glow */
   .historic-marker {
     filter: drop-shadow(0 0 4px rgba(251,191,36,.8));
   }
   /* Etiket (label) tasarımı */
   .leaflet-tooltip {
     border-radius: 6px !important;
     border: 0 !important;
     box-shadow: 0 2px 8px rgba(0,0,0,.15) !important;
     font-family: Inter, 'Segoe UI', sans-serif !important;
   }
   /* Katman kontrolü ve mini harita stili */
   .leaflet-control-layers, .leaflet-control-attribution {
     border-radius: 8px !important;
     box-shadow: 0 2px 10px rgba(0,0,0,.1) !important;
   }

   /* Kaynak rozetleri */
   .src-badge { display:inline-block; padding:2px 8px; border-radius:10px;
                font-size:11px; font-weight:600; margin-left:4px; }
   .src-usgs    { background:#e3f2fd; color:#1565c0; }
   .src-kandilli{ background:#e8f5e9; color:#2e7d32; }

   /* layout_sidebar main content gap'ini sıkıştır */
   .bslib-sidebar-layout > .main,
   .bslib-sidebar-layout main {
     gap: 8px !important;
   }

   /* Son Dakika şeridi */
   .breaking-bar {
     display:flex; align-items:center; gap:8px;
     background:linear-gradient(90deg,#b71c1c 0%,#d32f2f 100%);
     color:#fff; padding:8px 12px; border-radius:10px;
     box-shadow:0 2px 12px rgba(183,28,28,.25);
     overflow-x:auto; white-space:nowrap;
     font-family:Inter,'Segoe UI',sans-serif;
   }
   .breaking-label {
     display:inline-flex; align-items:center; gap:6px;
     background:rgba(0,0,0,.25); padding:4px 10px; border-radius:6px;
     font-weight:700; letter-spacing:.5px; font-size:12px;
     flex-shrink:0;
   }
   .breaking-label::before {
     content:''; display:inline-block; width:8px; height:8px;
     border-radius:50%; background:#fff;
     box-shadow:0 0 0 0 rgba(255,255,255,.7);
     animation: livePulse 1.4s infinite;
   }
   .breaking-item {
     display:inline-flex; align-items:center; gap:6px;
     background:rgba(255,255,255,.12); padding:4px 10px;
     border-radius:6px; font-size:13px; cursor:pointer;
     transition: background .15s ease;
     border:0; color:#fff !important;
   }
   .breaking-item:hover { background:rgba(255,255,255,.25); }
   .breaking-item .mag {
     background:#fff; color:#b71c1c; padding:1px 6px;
     border-radius:4px; font-weight:700; font-size:11px;
   }
   .breaking-item .place {
     max-width:200px; overflow:hidden; text-overflow:ellipsis;
   }
   .breaking-item .ago { opacity:.85; font-size:11px; }
   .breaking-item.fresh { background:rgba(255,235,59,.35); }
   .breaking-empty {
     padding:8px 12px; opacity:.85; font-style:italic;
   }

   /* === Modern Deprem Popup'ı === */
   .leaflet-popup-content { margin: 0 !important; }
   .leaflet-popup-content-wrapper {
     padding: 0 !important;
     border-radius: 14px !important;
     box-shadow: 0 12px 32px rgba(15,23,42,.18) !important;
     border: 1px solid rgba(15,23,42,.06) !important;
     overflow: hidden;
   }
   .qp {
     font-family: Inter, 'Segoe UI', sans-serif;
     min-width: 240px; max-width: 280px;
     color: #0f172a;
     padding: 11px 12px;
   }
   /* Header */
   .qp-header {
     display: flex; gap: 9px; align-items: stretch;
     padding-bottom: 9px;
     border-bottom: 1px solid #e2e8f0;
   }
   .qp-mag {
     flex-shrink: 0;
     width: 50px; height: 50px;
     border-radius: 11px;
     color: #fff;
     display: flex; flex-direction: column;
     align-items: center; justify-content: center;
     font-weight: 700;
   }
   .qp-mag-label {
     font-size: 7.5px; letter-spacing: .7px;
     opacity: .85; margin-bottom: 2px;
   }
   .qp-mag-value {
     font-size: 21px; line-height: 1;
     font-variant-numeric: tabular-nums;
   }
   .qp-meta { flex: 1; min-width: 0; padding-top: 2px; }
   .qp-meta-row {
     display: flex; gap: 6px; align-items: center;
     font-size: 11px; margin-bottom: 4px;
   }
   .qp-cat {
     font-weight: 700; letter-spacing: .3px;
     text-transform: uppercase; font-size: 11px;
   }
   .qp-dot { color: #cbd5e1; font-weight: 700; }
   .qp-ago { color: #64748b; font-size: 11px; }
   .qp-place {
     font-size: 12.5px; font-weight: 600; color: #0f172a;
     line-height: 1.3; margin-bottom: 5px;
     display: -webkit-box; -webkit-line-clamp: 2;
     -webkit-box-orient: vertical; overflow: hidden;
   }
   .qp-src { line-height: 1; }
   .qp-badge {
     display: inline-block; padding: 3px 9px;
     border-radius: 8px; font-size: 10px; font-weight: 700;
     letter-spacing: .4px;
   }
   .qp-badge-usgs     { background: #dbeafe; color: #1d4ed8; }
   .qp-badge-kandilli { background: #dcfce7; color: #15803d; }
   /* Grid */
   .qp-grid {
     display: grid; grid-template-columns: 1fr 1fr;
     gap: 7px 10px; padding: 9px 0;
     border-bottom: 1px solid #e2e8f0;
   }
   .qp-cell { min-width: 0; }
   .qp-key {
     font-size: 9.5px; color: #94a3b8;
     letter-spacing: .6px; font-weight: 700;
     text-transform: uppercase; margin-bottom: 3px;
   }
   .qp-val {
     font-size: 12.5px; font-weight: 700; color: #0f172a;
     line-height: 1.2;
   }
   .qp-val-sm { font-size: 11.5px; color: #0f172a; }
   .qp-val.qp-small { font-size: 11px; }
   .qp-mono {
     font-family: 'JetBrains Mono', Consolas, monospace;
     font-variant-numeric: tabular-nums;
   }
   .qp-unit { color: #94a3b8; font-weight: 500; font-size: 11px; }
   .qp-sub { font-size: 10.5px; color: #94a3b8; margin-top: 2px; }
   .qp-muted { color: #94a3b8; font-style: italic; }
   /* Fault */
   .qp-fault {
     padding: 7px 0;
     border-bottom: 1px solid #e2e8f0;
   }
   /* Tsunami uyarısı */
   .qp-warning {
     display: flex; align-items: center; gap: 6px;
     background: #fef2f2; color: #b91c1c;
     padding: 6px 10px; margin: 7px 0 0 0;
     border-radius: 7px; font-size: 11px; font-weight: 600;
     border-left: 3px solid #dc2626;
   }
   .qp-warning span:first-child { font-size: 14px; }
   /* Footer */
   .qp-footer {
     text-align: right; padding-top: 7px;
   }
   .qp-link {
     font-size: 12px; font-weight: 600;
     text-decoration: none;
     transition: opacity .15s ease;
   }
   .qp-link:hover { opacity: .75; }

   /* === Cam efekti istatistik kartları === */
   .stat-card {
     display:flex; align-items:center; gap:12px;
     background: rgba(255, 255, 255, 0.55);
     backdrop-filter: blur(12px) saturate(180%);
     -webkit-backdrop-filter: blur(12px) saturate(180%);
     border: 1px solid rgba(255, 255, 255, 0.6);
     border-left: 4px solid var(--accent, #0891b2);
     border-radius: 12px;
     padding: 12px 16px; min-height: 68px;
     box-shadow: 0 6px 24px rgba(15, 23, 42, 0.06);
     transition: transform .25s cubic-bezier(.34,1.56,.64,1),
                 box-shadow .25s ease,
                 background .25s ease;
     animation: cardSlideIn .55s cubic-bezier(.21,1.02,.73,1) backwards;
     position: relative;
     overflow: hidden;
   }
   .stat-card::before {
     content: '';
     position: absolute;
     inset: 0;
     background: linear-gradient(120deg,
       transparent 30%,
       rgba(255,255,255,0.35) 50%,
       transparent 70%);
     transform: translateX(-100%);
     transition: transform .8s ease;
     pointer-events: none;
   }
   .stat-card:hover {
     transform: translateY(-4px) scale(1.015);
     box-shadow: 0 14px 36px rgba(15, 23, 42, 0.14),
                 0 0 0 1px var(--accent, #0891b2);
     background: rgba(255, 255, 255, 0.75);
   }
   .stat-card:hover::before { transform: translateX(100%); }

   /* Kart sıralı giriş — her kart 80ms gecikme */
   @keyframes cardSlideIn {
     from { opacity: 0; transform: translateY(14px); }
     to   { opacity: 1; transform: translateY(0); }
   }
   .stat-card:nth-of-type(1) { animation-delay: 0.00s; }
   .stat-card:nth-of-type(2) { animation-delay: 0.08s; }
   .stat-card:nth-of-type(3) { animation-delay: 0.16s; }
   .stat-card:nth-of-type(4) { animation-delay: 0.24s; }

   .stat-icon {
     width: 42px; height: 42px; flex-shrink: 0;
     border-radius: 10px;
     background: linear-gradient(135deg,
                  var(--accent, #0891b2) 0%,
                  color-mix(in srgb, var(--accent, #0891b2) 70%, white) 100%);
     color: #fff;
     display: flex; align-items: center; justify-content: center;
     font-size: 18px;
     box-shadow: 0 4px 14px color-mix(in srgb, var(--accent, #0891b2) 35%, transparent);
     transition: transform .25s ease;
   }
   .stat-card:hover .stat-icon { transform: rotate(-6deg) scale(1.08); }
   .stat-icon i { font-size: 18px; line-height: 1; }

   .stat-body { min-width: 0; flex: 1; }
   .stat-title {
     font-size: 10.5px; font-weight: 700; letter-spacing: .5px;
     text-transform: uppercase; color: #64748b;
     margin: 0; line-height: 1.1;
     white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
   }
   .stat-value {
     font-size: 22px; font-weight: 800; color: #0f172a;
     line-height: 1.2; margin-top: 3px;
     font-variant-numeric: tabular-nums;
     animation: valuePop .45s cubic-bezier(.34,1.56,.64,1);
   }
   @keyframes valuePop {
     0%   { opacity: 0; transform: scale(0.82); }
     60%  { opacity: 1; transform: scale(1.05); }
     100% { opacity: 1; transform: scale(1); }
   }

   /* Yan panel cam efekti */
   .bslib-sidebar-layout > .sidebar,
   section.sidebar {
     background: rgba(255, 255, 255, 0.72) !important;
     backdrop-filter: blur(14px) saturate(180%);
     -webkit-backdrop-filter: blur(14px) saturate(180%);
     border-right: 1px solid rgba(255, 255, 255, 0.6);
   }

   /* === Anasayfa === */
   .home-hero {
     text-align: center;
     padding: 36px 20px 28px;
     margin-bottom: 14px;
   }
   .home-title {
     font-weight: 800;
     font-size: 2.4em;
     margin: 0 0 10px;
     background: linear-gradient(135deg, #0e7490 0%, #06b6d4 50%, #22d3ee 100%);
     -webkit-background-clip: text;
     -webkit-text-fill-color: transparent;
     background-clip: text;
     animation: titleFadeIn .9s ease-out;
   }
   @keyframes titleFadeIn {
     from { opacity: 0; transform: translateY(-8px); }
     to   { opacity: 1; transform: translateY(0); }
   }
   .home-title i, .home-title svg { color: #0891b2; -webkit-text-fill-color: #0891b2; }
   .home-subtitle {
     color: #475569;
     font-size: 15px;
     line-height: 1.6;
     max-width: 680px;
     margin: 0 auto 18px;
   }
   .home-live-pill {
     display: inline-flex;
     align-items: center;
     gap: 8px;
     background: rgba(16, 185, 129, 0.13);
     padding: 8px 18px;
     border-radius: 100px;
     border: 1px solid rgba(16, 185, 129, 0.3);
     color: #047857;
     font-size: 13px;
     font-weight: 600;
   }
   .home-section-title {
     font-weight: 700;
     color: #0f172a;
     margin: 22px 4px 10px;
     letter-spacing: .2px;
     font-size: 16px;
   }
   .home-section-title i, .home-section-title svg { color: #0891b2; }

   /* Vitrin kartı */
   .featured-quake-card { margin: 14px 0 4px; }
   .featured-content {
     display: flex;
     gap: 22px;
     align-items: center;
     padding: 8px 4px;
   }
   .featured-mag {
     width: 110px; height: 110px;
     border-radius: 18px;
     color: #fff;
     display: flex; flex-direction: column;
     align-items: center; justify-content: center;
     flex-shrink: 0;
     font-family: Inter, sans-serif;
     font-weight: 700;
   }
   .featured-mag small {
     font-size: 9.5px;
     letter-spacing: .8px;
     opacity: .9;
   }
   .featured-mag h2 {
     font-size: 38px;
     font-weight: 800;
     margin: 2px 0;
     line-height: 1;
     font-variant-numeric: tabular-nums;
   }
   .featured-info { flex: 1; min-width: 0; }
   .featured-place {
     font-size: 18px;
     font-weight: 700;
     color: #0f172a;
     margin-bottom: 8px;
   }
   .featured-meta {
     font-size: 13px;
     color: #475569;
     line-height: 1.7;
   }

   /* Hızlı erişim buton-kartları */
   .btn-nav-card {
     width: 100%;
     padding: 26px 14px !important;
     background: rgba(255, 255, 255, 0.62) !important;
     backdrop-filter: blur(12px) saturate(180%);
     -webkit-backdrop-filter: blur(12px) saturate(180%);
     border: 1px solid rgba(8, 145, 178, 0.18) !important;
     border-radius: 14px !important;
     color: #0e7490 !important;
     text-align: center;
     transition: transform .25s cubic-bezier(.34,1.56,.64,1),
                 box-shadow .25s ease,
                 background .25s ease,
                 color .25s ease;
     box-shadow: 0 4px 18px rgba(8, 145, 178, 0.06);
   }
   .btn-nav-card .nav-card-label {
     font-weight: 700;
     font-size: 15px;
     letter-spacing: .2px;
     margin-bottom: 4px;
   }
   .btn-nav-card .nav-card-desc {
     font-size: 11.5px;
     opacity: .75;
     font-weight: 500;
     line-height: 1.3;
   }
   .btn-nav-card:hover {
     background: linear-gradient(135deg, #0891b2 0%, #06b6d4 50%, #22d3ee 100%) !important;
     color: #fff !important;
     transform: translateY(-4px) scale(1.02);
     box-shadow: 0 14px 36px rgba(8, 145, 178, 0.32);
     border-color: transparent !important;
   }
   .btn-nav-card:hover i,
   .btn-nav-card:hover svg { color: #fff; }

   /* === Geçmiş Depremler intro (sorgu öncesi) === */
   .hist-intro {
     text-align: center;
     padding: 32px 24px;
     margin: 18px auto;
     max-width: 620px;
     background: rgba(255, 255, 255, 0.6);
     backdrop-filter: blur(14px) saturate(180%);
     -webkit-backdrop-filter: blur(14px) saturate(180%);
     border: 1px solid rgba(255, 255, 255, 0.55);
     border-radius: 18px;
     box-shadow: 0 12px 40px rgba(15, 23, 42, 0.08);
     animation: cardSlideIn .5s cubic-bezier(.21,1.02,.73,1);
   }
   .hist-intro-icon {
     font-size: 38px;
     color: #0891b2;
     margin-bottom: 12px;
     display: inline-block;
     padding: 16px;
     background: linear-gradient(135deg,
       rgba(8, 145, 178, 0.14),
       rgba(34, 211, 238, 0.14));
     border-radius: 50%;
     animation: floatGently 4s ease-in-out infinite;
   }
   @keyframes floatGently {
     0%, 100% { transform: translateY(0); }
     50%      { transform: translateY(-5px); }
   }
   .hist-intro-title {
     font-size: 22px; font-weight: 800;
     color: #0f172a;
     margin: 0 0 8px;
   }
   .hist-intro-text {
     color: #475569;
     font-size: 13.5px;
     max-width: 520px;
     margin: 0 auto 22px;
     line-height: 1.6;
   }
   /* Form card içeriği (intro içinde) */
   .hist-form-card {
     background: rgba(255, 255, 255, 0.78);
     border: 1px solid rgba(8, 145, 178, 0.14);
     border-radius: 12px;
     padding: 18px 20px;
     text-align: left;
     box-shadow: 0 4px 16px rgba(15, 23, 42, 0.04);
     margin-bottom: 14px;
   }
   .hist-form-card .form-group { margin-bottom: 12px; }
   .hist-form-card .control-label,
   .hist-form-card label {
     font-weight: 600;
     color: #0f172a;
     font-size: 13px;
     margin-bottom: 4px;
   }
   .hist-form-card hr { margin: 12px 0; border-color: rgba(8,145,178,0.12); }
   .hist-intro-help {
     display: inline-flex; align-items: center; gap: 4px;
     color: #64748b;
     font-size: 11.5px;
     padding: 8px 14px;
     background: rgba(207, 250, 254, 0.55);
     border-radius: 100px;
     border: 1px solid rgba(8, 145, 178, 0.16);
   }
   .hist-intro-help i, .hist-intro-help svg { color: #0891b2; }

   /* Sidebar boş durum (sorgu öncesi) */
   .sidebar-empty {
     text-align: center;
     padding: 28px 14px;
     animation: cardSlideIn .5s ease-out;
   }
   .sidebar-empty-icon {
     font-size: 30px;
     color: #0891b2;
     margin-bottom: 12px;
     opacity: .7;
   }
   .sidebar-empty-title {
     font-size: 14px; font-weight: 700;
     color: #334155;
     margin: 0 0 8px;
   }
   .sidebar-empty-text {
     color: #64748b;
     font-size: 12px;
     line-height: 1.55;
     margin: 0;
   }

   /* === Geçmiş — yatay (horizontal) intro form === */
   .hist-intro-h {
     max-width: 1100px;
     margin: 16px auto;
     padding: 8px;
     animation: cardSlideIn .5s cubic-bezier(.21,1.02,.73,1);
   }
   .hist-intro-h-header {
     text-align: center;
     margin-bottom: 16px;
   }
   .hist-intro-h-header .hist-intro-h-icon {
     font-size: 30px; color: #0891b2;
     padding: 14px;
     background: linear-gradient(135deg, rgba(8,145,178,.14), rgba(34,211,238,.14));
     border-radius: 50%;
     display: inline-block; margin-bottom: 8px;
   }
   .hist-intro-h-header h3 {
     font-size: 20px; font-weight: 800; color: #0f172a;
     margin: 4px 0;
   }
   .hist-intro-h-header p {
     color: #64748b; font-size: 13px; max-width: 600px;
     margin: 4px auto 0; line-height: 1.5;
   }
   .hist-intro-h-card {
     background: rgba(255,255,255,.78);
     border: 1px solid rgba(8,145,178,.14);
     border-radius: 14px;
     padding: 20px 22px;
     box-shadow: 0 6px 22px rgba(15,23,42,.06);
   }
   .hist-intro-h-card .form-group { margin-bottom: 10px; }
   .hist-intro-h-card label,
   .hist-intro-h-card .control-label {
     font-weight: 600; color: #0f172a; font-size: 12.5px;
     margin-bottom: 4px;
   }
   .hist-presets {
     margin-top: 10px;
   }
   .hist-presets-row {
     display: flex; gap: 6px; margin-top: 4px;
   }
   .hist-presets-row .btn { flex: 1; font-size: 12px; padding: 4px 8px; }

   /* === Geçmiş — sorgu sonrası kompakt grid === */
   .hist-result-card {
     overflow: hidden;
   }
   .hist-result-card .card-header {
     padding: 8px 14px; font-size: 13px;
   }"
)

# Türkçe tam sayı biçimi: 12345 → "12.345"
# (big.mark='.' ile decimal.mark='.' çakışmasından kaynaklı R uyarılarını önler)
fmt_tr_int <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Kompakt istatistik kartı oluşturucu
stat_card <- function(title, value_id, icon_name, accent = "#0891b2") {
  div(class = "stat-card",
      style = paste0("--accent:", accent, ";"),
      div(class = "stat-icon", icon(icon_name)),
      div(class = "stat-body",
          div(class = "stat-title", title),
          div(class = "stat-value", textOutput(value_id, inline = TRUE))
      )
  )
}

# Geçmiş Depremler için filtre kontrolleri — hem intro form'da hem sidebar'da
# kullanılır. Defaults parametreleri kullanıcının önceki seçimini taşımak için.
build_filter_inputs <- function(scope    = "region",
                                region   = "Türkiye",
                                city     = "İstanbul",
                                district = "_all_",
                                dates    = NULL,
                                minmag   = 5,
                                radius   = 150) {
  if (is.null(dates) || length(dates) < 2 || any(is.na(dates))) {
    dates <- c(Sys.Date() - 5 * 365, Sys.Date())
  }
  # İlçe seçeneklerini şehre göre önceden doldur — aksi halde form yeniden
  # render edildiğinde (intro → sidebar geçişi) önceki ilçe seçimi kaybolur.
  district_choices <- if (!is.null(city) && city %in% names(TR_DISTRICTS)) {
    ds <- TR_DISTRICTS[[city]]
    c("Tüm şehir" = "_all_", setNames(ds$name, ds$name))
  } else {
    c("Tüm şehir" = "_all_")
  }
  if (!(district %in% as.character(district_choices))) district <- "_all_"
  tagList(
    radioButtons("hist_scope", "Kapsam",
                 choices = c("Bölge" = "region", "Şehir" = "city"),
                 selected = scope),
    conditionalPanel(
      condition = "input.hist_scope == 'region'",
      selectInput("hist_region", "Bölge",
                  choices  = names(WORLD_REGIONS),
                  selected = region)
    ),
    conditionalPanel(
      condition = "input.hist_scope == 'city'",
      selectInput("hist_city", "Şehir",
                  choices  = TR_CITIES$name,
                  selected = city,
                  selectize = FALSE),
      selectInput("hist_district", "İlçe (opsiyonel)",
                  choices  = district_choices,
                  selected = district,
                  selectize = FALSE),
      sliderInput("hist_radius", "Yarıçap (km)",
                  min = 20, max = 500, value = radius, step = 10)
    ),
    dateRangeInput("hist_dates", "Tarih aralığı",
                   start    = dates[1],
                   end      = dates[2],
                   min      = "1900-01-01",
                   max      = Sys.Date(),
                   format   = "dd.mm.yyyy",
                   language = "tr",
                   separator = " — "),
    tags$div(
      style = "margin-top:-6px;",
      tags$small(class = "text-muted", "Hızlı seçim:"),
      layout_column_wrap(
        width = 1/4, gap = "4px",
        actionButton("hist_preset_5y",  "5 yıl",
                     class = "btn-outline-secondary btn-sm"),
        actionButton("hist_preset_25y", "25 yıl",
                     class = "btn-outline-secondary btn-sm"),
        actionButton("hist_preset_50y", "50 yıl",
                     class = "btn-outline-secondary btn-sm"),
        actionButton("hist_preset_all", "Tümü",
                     class = "btn-outline-secondary btn-sm")
      )
    ),
    sliderInput("hist_minmag", "Minimum büyüklük",
                min = 4, max = 9,
                value = max(4, min(9, round(as.numeric(minmag) %||% 5))),
                step = 1),
    actionButton("hist_query", "Sorgula",
                 icon = icon("magnifying-glass"),
                 class = "btn-primary w-100 mt-2")
  )
}

# Anasayfa hızlı erişim buton-kartı
nav_card_btn <- function(id, icon_name, label, desc) {
  actionButton(
    id,
    label = HTML(paste0(
      "<i class='fa fa-", icon_name,
      "' style='font-size:2em;display:block;margin-bottom:10px'></i>",
      "<div class='nav-card-label'>", label, "</div>",
      "<div class='nav-card-desc'>", desc, "</div>"
    )),
    class = "btn-nav-card"
  )
}

# ---- UI ----------------------------------------------------------------

ui <- page_navbar(
  id = "main_nav",      # programatik sekme geçişi (nav_select) için
  title = span(icon("globe"), " Dünya Deprem Haritası"),
  theme = app_theme,
  fillable = "Harita",  # sadece Harita full-screen; diğerleri scroll
  navbar_options = navbar_options(bg = "#0891b2", theme = "dark"),

  header = tags$head(
    tags$style(app_css)
  ),

  nav_panel(
    title = "Anasayfa",
    icon  = icon("house"),

    # Hero bölümü
    div(class = "home-hero",
        h1(class = "home-title",
           icon("globe"), " Dünya Deprem İzleme Sistemi"),
        p(class = "home-subtitle",
          "USGS ve Kandilli Rasathanesi'nin canlı verileriyle dünya ",
          "geneli ve Türkiye'deki deprem aktivitesini gerçek zamanlı ",
          "takip et, geçmiş depremleri sorgula, istatistikleri incele."),
        div(class = "home-live-pill",
            span(class = "live-dot"),
            textOutput("home_last_update", inline = TRUE))
    ),

    # 24 saat özet kartları
    h5(class = "home-section-title",
       icon("chart-simple"), " Son 24 Saat — Dünya Geneli"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      stat_card("Toplam Deprem (24s)",   "home_24h",         "wave-square",         "#0891b2"),
      stat_card("En Büyük (24s)",        "home_biggest_24h", "triangle-exclamation","#e11d48"),
      stat_card("Türkiye Aktivitesi",    "home_tr_24h",      "flag",                "#10b981"),
      stat_card("Tsunami Uyarısı",       "home_tsunami",     "water",               "#f59e0b")
    ),

    # Vitrin: en büyük deprem
    uiOutput("home_featured_quake"),

    # Son 5 aktivite
    card(
      card_header(
        class = "d-flex align-items-center",
        tagList(icon("bolt"), " Son 5 Aktivite"),
        tags$small(class = "text-muted ms-2",
                   "(Son Dakika canlı feed)")
      ),
      DTOutput("home_recent_table")
    ),

    # Hızlı erişim navigasyon kartları
    h5(class = "home-section-title",
       icon("compass"), " Hızlı Erişim"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      nav_card_btn("nav_to_harita",  "map-location-dot",
                   "Canlı Harita",
                   "Tüm depremleri haritada gör"),
      nav_card_btn("nav_to_tablo",   "table",
                   "Tablo",
                   "Filtrelenebilir liste"),
      nav_card_btn("nav_to_history", "magnifying-glass-chart",
                   "Geçmiş Depremler",
                   "1900'a kadar sorgula"),
      nav_card_btn("nav_to_turkey",  "chart-pie",
                   "Türkiye İstatistikleri",
                   "1900+ tarihsel veriler")
    )
  ),

  nav_panel(
    title = "Harita",
    icon  = icon("map-location-dot"),

    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = span(icon("sliders"), " Filtreler"),
        bg = "#ffffff",

        selectInput(
          "period", "Zaman aralığı",
          choices = c(
            "Son 1 saat"  = "hour",
            "Son 1 gün"   = "day",
            "Son 7 gün"   = "week",
            "Son 30 gün"  = "month"
          ),
          selected = "day"
        ),

        sliderInput("min_mag", "Minimum büyüklük",
                    min = 2, max = 9, value = 4, step = 1),

        sliderInput("max_depth", "Maksimum derinlik (km)",
                    min = 0, max = 700, value = 700, step = 10),

        checkboxInput("tsunami_only", "Sadece tsunami uyarılı", FALSE),
        checkboxInput("cluster", "İşaretçileri kümele", FALSE),

        sliderInput("pulse_threshold",
                    "Animasyon eşiği (M ≥)",
                    min = 2, max = 7, value = 4, step = 1),

        hr(),

        checkboxInput("auto_refresh",
                      tagList(span(class = "live-dot"),
                              "Otomatik yenile (60 sn)"),
                      TRUE),
        actionButton("refresh", "Şimdi yenile",
                     icon = icon("rotate"),
                     class = "btn-primary w-100"),

        hr(),

        h6("Hızlı bölge"),
        layout_column_wrap(
          width = 1/2, gap = "6px",
          actionButton("zoom_world",   "Dünya",    class = "btn-light btn-sm"),
          actionButton("zoom_turkey",  "Türkiye",  class = "btn-light btn-sm"),
          actionButton("zoom_pacific", "Pasifik",  class = "btn-light btn-sm"),
          actionButton("zoom_europe",  "Avrupa",   class = "btn-light btn-sm")
        ),

        hr(),
        helpText(HTML(paste0(
          "<b>Veri kaynakları</b><br>",
          "<span class='src-badge src-usgs'>USGS</span> ",
          "<a href='https://earthquake.usgs.gov/' target='_blank'>",
          "Earthquake Hazards Program</a> — dünya geneli<br>",
          "<span class='src-badge src-kandilli'>KANDİLLİ</span> ",
          "Boğaziçi Üniv. Rasathanesi — Türkiye"
        )))
      ),

      # Son Dakika şeridi + harita (stat kartları Anasayfa'ya taşındı)
      uiOutput("breaking_bar"),

      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tagList(icon("earth-americas"), " Anlık Harita"),
          tags$small(class = "text-muted",
                     textOutput("last_update", inline = TRUE))
        ),
        leafletOutput("map", height = "calc(100vh - 220px)")
      )
    )
  ),

  nav_panel(
    title = "Tablo",
    icon  = icon("table"),
    card(
      card_header("Filtrelenmiş Deprem Listesi"),
      DTOutput("table")
    )
  ),

  nav_panel(
    title = "Geçmiş Depremler",
    icon  = icon("magnifying-glass-chart"),

    layout_sidebar(
      sidebar = sidebar(
        id = "hist_sidebar",
        width = 300,
        open = "closed",  # Başlangıçta kapalı — ilk sorgudan sonra otomatik açılır
        title = span(icon("filter"), " Sorgu Filtreleri"),
        bg = "#ffffff",
        uiOutput("hist_sidebar_content")
      ),
      # Sorgu yapılana kadar yatay intro form, sonra harita+tablo yan yana
      uiOutput("hist_results")
    )
  ),

  nav_panel(
    title = "Türkiye İstatistikleri",
    icon  = icon("chart-pie"),

    # Veri durumu uyarısı (CSV yoksa görünür)
    uiOutput("tr_data_status"),

    # Üstte 4 kompakt stat kart (yan yana)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      stat_card("Son 24 Saat (Türkiye)",  "tr_last24",     "clock-rotate-left", "#0891b2"),
      stat_card("M ≥ 5.0 Toplam (1900+)", "tr_total_m5",   "wave-square",       "#e74c3c"),
      stat_card("Ortalama Derinlik",      "tr_avg_depth",  "ruler-vertical",    "#f1c40f"),
      stat_card("Veri Aralığı",           "tr_year_range", "calendar",          "#1abc9c")
    ),

    # En büyük deprem detayı
    card(
      card_header(icon("trophy"), " En Büyük Deprem (Türkiye, 1900+)"),
      uiOutput("tr_biggest_box")
    ),

    # Yıllık deprem sayıları grafiği — geniş ve yüksek
    card(
      card_header(icon("chart-column"),
                  " Yıllık Deprem Sayıları (M ≥ 4.0)"),
      card_body(plotlyOutput("tr_yearly_plot", height = "420px"))
    ),

    # Büyüklük dağılımı — tek başına, geniş
    card(
      card_header(icon("chart-bar"), " Büyüklük Kategorileri"),
      card_body(plotlyOutput("tr_mag_plot", height = "340px"))
    ),

    # Derinlik dağılımı — tek başına, geniş
    card(
      card_header(icon("arrow-down"), " Derinlik Dağılımı (0-200 km)"),
      card_body(plotlyOutput("tr_depth_plot", height = "340px"))
    ),

    # En büyük 10 deprem listesi
    card(
      card_header(icon("list-ol"),
                  " Türkiye'deki En Büyük 10 Deprem (1900+)"),
      DTOutput("tr_top10")
    )
  ),

  nav_panel(
    title = "Hakkında",
    icon  = icon("circle-info"),
    card(
      card_header("Proje Hakkında"),
      card_body(
        # NOT: Eskiden shiny::markdown() kullanılıyordu; commonmark paketi
        # eksik olan kurulumlarda sayfa render edilemiyor ve ekran boş /
        # koyu görünüyordu. Doğrudan HTML ile daha güvenilir.
        tags$p(
          "Bu uygulama, ", tags$b("USGS Earthquake Hazards Program"),
          " tarafından yayınlanan GeoJSON akışlarını kullanarak dünya ",
          "genelindeki son depremleri interaktif bir harita üzerinde gösterir."
        ),
        tags$p(tags$b("Kullanılan teknolojiler")),
        tags$ul(
          tags$li("R + Shiny — web uygulama çatısı"),
          tags$li("bslib (Bootstrap 5, Flatly) — modern arayüz teması"),
          tags$li("leaflet + leaflet.extras — interaktif harita"),
          tags$li("sf — coğrafi veri (CBS)"),
          tags$li("httr2 / jsonlite — USGS + Kandilli API erişimi"),
          tags$li("DT — interaktif tablo")
        ),
        tags$p(
          tags$b("Veri güncelleme:"),
          " USGS verisi ~1 dakika gecikmeyle yayımlanır. ",
          "Otomatik yenileme açıkken uygulama 60 saniyede bir veriyi yeniden ",
          "çeker. Yenileme başarısız olursa önceki veri korunur."
        ),
        tags$p(tags$b("Renk kodlaması (büyüklüğe göre — USGS sismik standart)")),
        tags$table(
          class = "table table-sm",
          style = "max-width: 480px;",
          tags$thead(tags$tr(
            tags$th("Aralık"), tags$th("Renk"), tags$th("Anlam")
          )),
          tags$tbody(
            tags$tr(tags$td("< 2,5"),     tags$td("Yeşil"),       tags$td("Mikro / Zayıf")),
            tags$tr(tags$td("2,5 – 4,0"), tags$td("Açık yeşil"),  tags$td("Hafif")),
            tags$tr(tags$td("4,0 – 5,0"), tags$td("Sarı"),        tags$td("Orta")),
            tags$tr(tags$td("5,0 – 6,0"), tags$td("Turuncu"),     tags$td("Güçlü")),
            tags$tr(tags$td("6,0 – 7,0"), tags$td("Kırmızı"),     tags$td("Şiddetli")),
            tags$tr(tags$td("≥ 7,0"),     tags$td("Koyu Kırmızı"),tags$td("Yıkıcı"))
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(href = "https://earthquake.usgs.gov/", target = "_blank",
           class = "nav-link text-white",
           icon("up-right-from-square"), " USGS")
  )
)

# ---- Server ------------------------------------------------------------

server <- function(input, output, session) {

  refresh_tick <- reactive({
    input$refresh
    if (isTRUE(input$auto_refresh)) {
      invalidateLater(60 * 1000, session)
    }
    Sys.time()
  })

  # ---- Veri Cache --------------------------------------------------------
  # Çekme başarısızsa veya kısmen başarısızsa eski veriyi koru.
  # nearest_fault burada bir kez hesaplanır; filtered() tekrar etmez.
  raw_cache <- reactiveValues(
    data        = empty_quake_df(),
    period      = NA_character_,
    fetched_at  = as.POSIXct(NA),
    last_status = "init"   # "ok", "partial", "fail", "init"
  )

  raw_data <- reactive({
    refresh_tick()
    period <- input$period

    same_params <- identical(raw_cache$period, period)

    res <- withProgress(
      message = "Veriler çekiliyor...", value = 0.1, {
        incProgress(0.3, detail = "USGS")
        usgs_df <- fetch_usgs(period)

        incProgress(0.3, detail = "Kandilli (Türkiye)")
        kandilli_df <- fetch_kandilli(limit = 500)

        incProgress(0.2, detail = "Birleştiriliyor")
        merged <- merge_quake_sources(usgs_df, kandilli_df)

        list(
          merged       = merged,
          usgs_ok      = nrow(usgs_df)     > 0,
          kandilli_ok  = nrow(kandilli_df) > 0,
          n_usgs       = nrow(usgs_df),
          n_kandilli   = nrow(kandilli_df)
        )
      }
    )

    merged      <- res$merged
    all_ok      <- res$usgs_ok && res$kandilli_ok
    any_data    <- nrow(merged) > 0
    has_cache   <- nrow(raw_cache$data) > 0 && same_params

    # Tüm istekler çuvalladıysa ve cache varsa: cache'i koru
    if (!any_data && has_cache) {
      raw_cache$last_status <- "fail"
      showNotification(
        "Veri çekilemedi — önceki veri korunuyor.",
        type = "warning", duration = 6
      )
      return(raw_cache$data)
    }

    # Kısmi başarı: yeni veri var ama bir kaynak çuvalladı
    if (any_data && !all_ok) {
      kaynaklar <- c(
        if (!res$usgs_ok)     "USGS" else NULL,
        if (!res$kandilli_ok) "Kandilli" else NULL
      )
      if (length(kaynaklar) > 0) {
        showNotification(
          paste("Veri kısmi:", paste(kaynaklar, collapse = ", "),
                "yanıt vermedi."),
          type = "warning", duration = 5
        )
      }
      raw_cache$last_status <- "partial"
    } else if (any_data) {
      raw_cache$last_status <- "ok"
    } else {
      # Hiç veri yok ve cache de yok — kullanıcıya net hata
      raw_cache$last_status <- "fail"
      showNotification(
        "Hiçbir kaynaktan veri alınamadı. İnternet bağlantısını kontrol edin.",
        type = "error", duration = 8
      )
    }

    # Pahalı: en yakın plaka sınırı — burada bir kez hesaplansın,
    # filtered() tekrar etmesin (slider hareketinde performans)
    if (any_data) {
      withProgress(message = "Fay mesafeleri hesaplanıyor...", value = 0.1, {
        merged <- nearest_fault(merged)
      })
    }

    raw_cache$data       <- merged
    raw_cache$period     <- period
    raw_cache$fetched_at <- Sys.time()

    merged
  })

  # Slider'lar hareket ederken çizimi geciktir (performans)
  min_mag_d   <- reactive(input$min_mag)   |> debounce(300)
  max_depth_d <- reactive(input$max_depth) |> debounce(300)
  pulse_d     <- reactive(input$pulse_threshold) |> debounce(300)

  # Tek deprem odak modu (Son Dakika tıklamasıyla)
  focused_id <- reactiveVal(NULL)

  filtered <- reactive({
    raw <- raw_data()
    # NOT: nearest_fault artık raw_data()'da bir kez hesaplanıyor —
    # slider değişiminde st_distance tekrar tekrar çalıştırılmaz.

    # Odak modu — Son Dakika kartından tıklanmışsa sadece o depremi göster
    fid <- focused_id()
    if (!is.null(fid) && nrow(raw) > 0) {
      match_idx <- which(!is.na(raw$id) & raw$id == fid)
      if (length(match_idx) > 0) {
        return(raw[match_idx, , drop = FALSE])
      }
      # Odaklı deprem mevcut veride yok — sessizce odağı temizle
      focused_id(NULL)
    }

    df <- filter_quakes(
      raw,
      min_mag      = min_mag_d(),
      max_depth    = max_depth_d(),
      tsunami_only = input$tsunami_only
    )
    df
  })

  # ---- Harita ---------------------------------------------------------
  output$map <- renderLeaflet({ base_map() })
  # Anasayfa'da raw_data tetiklenmiyor (recent_quick kullanılıyor); kullanıcı
  # Harita'ya geçtiğinde observe ve renderLeaflet yarışa giriyor. suspendWhenHidden
  # = FALSE ile map output'u her zaman canlı kalsın, observe boşa düşmesin.
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  observe({
    df <- filtered()
    # Map element DOM'da olana kadar bekle — yoksa leafletProxy boşa düşer
    req(input$map_zoom)
    proxy <- leafletProxy("map")
    draw_quakes(proxy, df,
                cluster = isTRUE(input$cluster),
                pulse_threshold = pulse_d())
  })

  # Hızlı bölge butonları
  observeEvent(input$zoom_world, {
    leafletProxy("map") |> setView(20, 25, 2.5)
  })
  observeEvent(input$zoom_turkey, {
    leafletProxy("map") |> setView(35, 39, 6)
  })
  observeEvent(input$zoom_pacific, {
    leafletProxy("map") |> setView(-150, 10, 3)
  })
  observeEvent(input$zoom_europe, {
    leafletProxy("map") |> setView(15, 50, 4)
  })

  output$last_update <- renderText({
    raw_data()  # son fetch'i tetikle
    ts <- raw_cache$fetched_at
    if (is.na(ts)) return("Veri henüz alınmadı")
    status_lbl <- switch(raw_cache$last_status %||% "ok",
                         ok      = "",
                         partial = " (kısmi)",
                         fail    = " (önceki veri)",
                         "")
    sprintf("Son güncelleme: %s%s — %d kayıt",
            format(ts, "%H:%M:%S"), status_lbl, nrow(raw_cache$data))
  })

  # ---- Son Dakika Şeridi ----------------------------------------------
  # Hafif/hızlı feed — sadece USGS son 1 saat, nearest_fault YOK, Kandilli YOK.
  # Son 5 Aktivite & Son Dakika için kullanılır; raw_data()'nın 10-30sn'lik
  # ilk yükleme süresini beklememesi için ayrı reactive.
  recent_quick <- reactive({
    refresh_tick()
    df <- fetch_usgs("hour")
    if (is.null(df) || nrow(df) == 0) {
      # Son 1 saatte hiç deprem yoksa son 1 güne geç
      df <- fetch_usgs("day")
    }
    if (is.null(df)) df <- empty_quake_df()
    df
  })

  recent_quakes <- reactive({
    latest_quakes(recent_quick(), n = 5)
  })

  output$breaking_bar <- renderUI({
    # Auto-refresh kapalı olsa bile "X dk önce" etiketleri canlı kalsın.
    invalidateLater(60 * 1000, session)
    df <- recent_quakes()
    if (nrow(df) == 0) {
      return(div(class = "breaking-bar",
                 div(class = "breaking-label", "SON DAKİKA"),
                 div(class = "breaking-empty",
                     "Veri bekleniyor...")))
    }
    ago_secs <- as.numeric(difftime(Sys.time(), df$time, units = "secs"))
    items <- lapply(seq_len(nrow(df)), function(i) {
      fresh_cls <- if (!is.na(ago_secs[i]) && ago_secs[i] < 3600)
                     "breaking-item fresh" else "breaking-item"
      tags$button(
        id        = paste0("recent_", i),
        class     = fresh_cls,
        onclick   = sprintf(
          "Shiny.setInputValue('recent_click', %d, {priority:'event'});", i),
        type      = "button",
        span(class = "mag", sprintf("M %.1f", df$mag[i])),
        span(class = "place",
             ifelse(is.na(df$place[i]), "Bilinmeyen konum", df$place[i])),
        span(class = "ago", time_ago_tr(df$time[i]))
      )
    })
    div(class = "breaking-bar",
        div(class = "breaking-label", "SON DAKİKA"),
        items)
  })

  # Haritada boş bir noktaya tıklanınca odak modu kapansın
  observeEvent(input$map_click, {
    if (!is.null(focused_id())) focused_id(NULL)
  })

  # Son Dakika kartına tıklayınca: ODAK modu — sadece o deprem haritada
  observeEvent(input$recent_click, {
    df <- recent_quakes()
    i  <- as.integer(input$recent_click)
    if (is.na(i) || i < 1 || i > nrow(df)) return()

    # ID NA ise odak modu çalışmaz, yalnızca uç
    if (!is.na(df$id[i]) && nzchar(df$id[i])) {
      focused_id(df$id[i])
    }

    leafletProxy("map") |>
      flyTo(lng = df$lon[i], lat = df$lat[i], zoom = 7)
  })


  # ---- Tablo ----------------------------------------------------------
  # Stabil sütun şemasıyla tablo verisi — boşken bile aynı sütunlar
  # Sütun adları ASCII (DataTables Türkçe karakteri ID olarak bozuyor);
  # gösterimde Türkçe başlık `colnames` ile eklenir.
  TABLE_COLS <- c("Zaman", "Büyüklük", "Derinlik", "Yer", "Kıta",
                  "En yakın fay", "Mesafe (km)", "Kaynak", "Tsunami", "Detay")

  build_table_df <- function(df) {
    if (nrow(df) == 0) {
      return(data.frame(
        time       = character(0),
        magnitude  = numeric(0),
        depth      = numeric(0),
        place      = character(0),
        continent  = character(0),
        fault      = character(0),
        fault_dist = numeric(0),
        source     = character(0),
        tsunami    = character(0),
        url        = character(0),
        stringsAsFactors = FALSE
      ))
    }
    df |>
      arrange(desc(time)) |>
      transmute(
        time       = format(time, "%Y-%m-%d %H:%M:%S"),
        magnitude  = round(mag, 1),
        depth      = round(depth_km, 1),
        place      = ifelse(is.na(place) | !nzchar(place),
                            "Bilinmeyen konum", place),
        continent  = continent,
        fault      = ifelse(is.na(fault_name), "—", fault_name),
        fault_dist = round(fault_dist_km, 1),
        source     = ifelse(source == "Kandilli",
                            "<span class='src-badge src-kandilli'>KANDİLLİ</span>",
                            "<span class='src-badge src-usgs'>USGS</span>"),
        tsunami    = ifelse(tsunami == 1, "Evet", "Hayır"),
        url        = ifelse(is.na(url) | !nzchar(url),
                            "—",
                            sprintf("<a href='%s' target='_blank'>Aç</a>",
                                    url))
      )
  }

  table_data <- reactive({ build_table_df(filtered()) })

  # İlk render — sonra observer ile replaceData (state korunur)
  output$table <- renderDT({
    isolate({
      datatable(
        table_data(),
        escape = FALSE, rownames = FALSE,
        colnames = TABLE_COLS,
        options = list(
          pageLength = 25, order = list(list(0, "desc")),
          dom = "ftip",
          language = list(
            emptyTable   = "Filtreye uyan deprem yok.",
            zeroRecords  = "Aramayla eşleşen kayıt yok",
            info         = "_TOTAL_ kayıt: _START_ – _END_",
            infoEmpty    = "0 kayıt",
            infoFiltered = "(_MAX_ kayıttan filtrelendi)",
            lengthMenu   = "Sayfa başına _MENU_ kayıt",
            search       = "Ara:",
            paginate     = list(previous = "Önceki", `next` = "Sonraki")
          )
        )
      ) |>
        formatStyle("magnitude",
          background = styleColorBar(c(0, 9), "#fcd9d6"),
          backgroundSize = "98% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
  }, server = TRUE)

  # Veri yenilendikçe tabloyu in-place güncelle
  observe({
    d <- table_data()
    DT::dataTableProxy("table") |>
      DT::replaceData(d, resetPaging = FALSE, clearSelection = "none",
                      rownames = FALSE)
  })

  # ---- Anasayfa --------------------------------------------------------
  # Türkiye yaklaşık bbox — home_tr_24h kartında kullanılır
  TR_BBOX_HOME <- list(min_lon = 25.5, max_lon = 45,
                       min_lat = 35.5, max_lat = 43)

  output$home_last_update <- renderText({
    raw_data()  # son fetch'i tetikle
    invalidateLater(60 * 1000, session)  # her dakika tazele
    ts <- raw_cache$fetched_at
    if (is.na(ts)) return("Veri yükleniyor...")
    sprintf("Son güncelleme: %s", format(ts, "%H:%M:%S"))
  })

  home_24h_df <- reactive({
    df <- raw_data()
    if (nrow(df) == 0) return(df)
    cutoff <- Sys.time() - 24 * 3600
    df[!is.na(df$time) & df$time >= cutoff, , drop = FALSE]
  })

  output$home_24h <- renderText({
    df <- home_24h_df()
    if (nrow(df) == 0) "—" else fmt_tr_int(nrow(df))
  })

  output$home_biggest_24h <- renderText({
    df <- home_24h_df()
    if (nrow(df) == 0) return("—")
    mx <- suppressWarnings(max(df$mag, na.rm = TRUE))
    if (!is.finite(mx)) return("—")
    sprintf("M %.1f", mx)
  })

  output$home_tr_24h <- renderText({
    df <- home_24h_df()
    if (nrow(df) == 0) return("—")
    tr <- df[!is.na(df$lon) & !is.na(df$lat) &
             df$lon >= TR_BBOX_HOME$min_lon &
             df$lon <= TR_BBOX_HOME$max_lon &
             df$lat >= TR_BBOX_HOME$min_lat &
             df$lat <= TR_BBOX_HOME$max_lat, , drop = FALSE]
    fmt_tr_int(nrow(tr))
  })

  output$home_tsunami <- renderText({
    df <- raw_data()
    if (nrow(df) == 0) return("0")
    fmt_tr_int(sum(df$tsunami == 1, na.rm = TRUE))
  })

  output$home_featured_quake <- renderUI({
    df <- raw_data()
    if (nrow(df) == 0) {
      return(div(class = "info-banner",
        style = "margin: 14px 0;",
        "Veri yükleniyor — birkaç saniye içinde dolacak."))
    }
    valid <- df[!is.na(df$mag), , drop = FALSE]
    if (nrow(valid) == 0) {
      return(div(class = "info-banner",
        style = "margin: 14px 0;",
        "Görüntülenecek deprem bulunamadı."))
    }
    big <- valid[which.max(valid$mag), , drop = FALSE]

    place_txt  <- if (is.na(big$place) || !nzchar(big$place))
                    "Bilinmeyen konum" else big$place
    depth_txt  <- if (is.finite(big$depth_km))
                    sprintf("%.1f km", big$depth_km) else "—"
    mag_col    <- mag_color(big$mag)
    src_badge  <- if (big$source == "Kandilli")
                    "<span class='src-badge src-kandilli'>KANDİLLİ</span>"
                  else
                    "<span class='src-badge src-usgs'>USGS</span>"
    ago_txt    <- time_ago_tr(big$time)

    card(
      class = "featured-quake-card",
      card_header(
        class = "d-flex align-items-center",
        tagList(icon("triangle-exclamation"), " Mevcut Veride En Büyük Deprem"),
        tags$small(class = "text-muted ms-2", ago_txt)
      ),
      div(class = "featured-content",
        div(class = "featured-mag",
            style = sprintf(
              "background: linear-gradient(135deg, %s 0%%, %s 100%%);
               box-shadow: 0 8px 28px %s55;",
              mag_col, mag_col, mag_col),
            tags$small("BÜYÜKLÜK"),
            tags$h2(sprintf("%.1f", big$mag)),
            tags$small(mag_category(big$mag))
        ),
        div(class = "featured-info",
          div(class = "featured-place", place_txt),
          div(class = "featured-meta",
            HTML(sprintf(
              "%s &nbsp;&middot;&nbsp; <b>Tarih:</b> %s &nbsp;&middot;&nbsp; ",
              src_badge,
              format(big$time, "%d.%m.%Y %H:%M UTC"))),
            HTML(sprintf(
              "<b>Derinlik:</b> %s &nbsp;&middot;&nbsp; <b>Konum:</b> %.3f, %.3f",
              depth_txt, big$lat, big$lon))
          ),
          actionButton("nav_to_harita_from_card", "Haritada Göster",
                       icon = icon("map-location-dot"),
                       class = "btn-primary mt-3")
        )
      )
    )
  })

  output$home_recent_table <- renderDT({
    # Hızlı feed kullan — raw_data()'yı (10-30sn) bekleme
    recent <- recent_quakes()
    if (nrow(recent) == 0) {
      return(datatable(
        data.frame(Mesaj = "Veri bekleniyor..."),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE),
        class = "compact stripe"
      ))
    }
    # NOT: Sütun adları ASCII (DataTables Türkçe karakterleri ID olarak
    # kullanırken bozuyor); başlıklar `colnames` ile Türkçe görünür.
    show <- data.frame(
      magnitude = sprintf("M %.1f", recent$mag),
      place     = ifelse(is.na(recent$place) | !nzchar(recent$place),
                         "Bilinmeyen konum", recent$place),
      ago       = time_ago_tr(recent$time),
      utc       = format(recent$time, "%d.%m %H:%M"),
      source    = ifelse(recent$source == "Kandilli",
                         "<span class='src-badge src-kandilli'>KANDİLLİ</span>",
                         "<span class='src-badge src-usgs'>USGS</span>"),
      stringsAsFactors = FALSE
    )
    datatable(
      show, rownames = FALSE, escape = FALSE,
      colnames = c("Büyüklük", "Yer", "Zaman", "UTC", "Kaynak"),
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      class = "compact stripe hover"
    )
  })

  # Sekme geçişi observer'ları (Anasayfa hızlı erişim + featured kart)
  observeEvent(input$nav_to_harita, {
    bslib::nav_select("main_nav", "Harita", session = session)
  })
  observeEvent(input$nav_to_tablo, {
    bslib::nav_select("main_nav", "Tablo", session = session)
  })
  observeEvent(input$nav_to_history, {
    bslib::nav_select("main_nav", "Geçmiş Depremler", session = session)
  })
  observeEvent(input$nav_to_turkey, {
    bslib::nav_select("main_nav", "Türkiye İstatistikleri", session = session)
  })

  # Vitrin kartı "Haritada Göster": en büyük depreme odak + lokasyona uç
  # Pending focus pattern — harita henüz açılmamışsa sekme değişince uygula
  pending_focus <- reactiveVal(NULL)

  observeEvent(input$nav_to_harita_from_card, {
    df <- raw_data()
    if (nrow(df) == 0) {
      bslib::nav_select("main_nav", "Harita", session = session)
      return()
    }
    valid <- df[!is.na(df$mag), , drop = FALSE]
    if (nrow(valid) == 0) {
      bslib::nav_select("main_nav", "Harita", session = session)
      return()
    }
    big <- valid[which.max(valid$mag), , drop = FALSE]
    pending_focus(list(
      id  = if (!is.na(big$id) && nzchar(big$id)) big$id else NULL,
      lon = big$lon, lat = big$lat
    ))
    bslib::nav_select("main_nav", "Harita", session = session)
  })

  # Harita sekmesine geçilince bekleyen odağı uygula (focus + flyTo)
  observeEvent(input$main_nav, {
    if (!isTRUE(input$main_nav == "Harita")) return()
    pf <- pending_focus()
    if (is.null(pf)) return()
    if (!is.null(pf$id)) focused_id(pf$id)
    if (is.finite(pf$lon) && is.finite(pf$lat)) {
      leafletProxy("map") |>
        flyTo(lng = pf$lon, lat = pf$lat, zoom = 6)
    }
    pending_focus(NULL)
  }, ignoreInit = TRUE)

  # ---- Geçmiş Sorgulama Sekmesi --------------------------------------
  HIST_LIMIT <- 20000   # USGS FDSN query API'nin teorik üst sınırı

  hist_data         <- reactiveVal(NULL)
  hist_query_made   <- reactiveVal(FALSE)
  hist_form_snapshot <- reactiveVal(NULL)  # intro form değerlerini sidebar'a taşır

  # İlçe seçeneklerini şehre göre üret (yardımcı fonksiyon)
  district_choices_for <- function(city) {
    # Browser'dan gelen string'i UTF-8'e zorla — Windows R'de karşılaştırma
    # encoding mismatch yüzünden başarısız olabiliyor.
    if (!is.null(city)) city <- enc2utf8(city)
    if (!is.null(city) && city %in% names(TR_DISTRICTS)) {
      ds <- TR_DISTRICTS[[city]]
      c("Tüm şehir" = "_all_", setNames(ds$name, ds$name))
    } else {
      c("Tüm şehir" = "_all_")
    }
  }

  # Şehir değiştiğinde ilçe seçeneklerini güncelle (mevcut seçimi koru)
  observeEvent(input$hist_city, {
    choices <- district_choices_for(input$hist_city)
    current <- isolate(input$hist_district)
    selected_val <- if (!is.null(current) && current %in% as.character(choices)) {
      current
    } else {
      "_all_"
    }
    updateSelectInput(session, "hist_district",
                      choices = choices, selected = selected_val)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  # Kapsam "Şehir"e geçince ilçe listesini taze tut (selectize=FALSE olsa bile
  # bazen DOM güncellemesi gecikebiliyor — defensive update).
  observeEvent(input$hist_scope, {
    if (isTRUE(input$hist_scope == "city")) {
      choices <- district_choices_for(input$hist_city)
      current <- isolate(input$hist_district)
      selected_val <- if (!is.null(current) && current %in% as.character(choices)) {
        current
      } else {
        "_all_"
      }
      updateSelectInput(session, "hist_district",
                        choices = choices, selected = selected_val)
    }
  })

  # Sidebar içeriği — sorgu öncesi boş mesaj, sonrası filtreler
  output$hist_sidebar_content <- renderUI({
    # Sorgu yapılmadıysa sidebar zaten kapalı (open="closed") — boş içerik yeter.
    if (!hist_query_made()) return(NULL)
    # Sorgu sonrası — snapshot'tan değerleri al (re-render güvenli)
    snap <- hist_form_snapshot() %||% list()
    tagList(
      build_filter_inputs(
        scope    = snap$scope    %||% "region",
        region   = snap$region   %||% "Türkiye",
        city     = snap$city     %||% "İstanbul",
        district = snap$district %||% "_all_",
        dates    = snap$dates,
        minmag   = snap$minmag   %||% 5,
        radius   = snap$radius   %||% 150
      ),
      hr(),
      div(class = "info-banner",
          style = "font-size:11.5px; padding:8px 10px;
                   line-height:1.45;",
          HTML(paste0(
            "<b>USGS veri kapsamı</b><br>",
            "• 1900&ndash;1973: M&nbsp;&geq;&nbsp;6<br>",
            "• 1973&ndash;1990: M&nbsp;&geq;&nbsp;4-5<br>",
            "• 1990&ndash;günümüz: M&nbsp;&geq;&nbsp;4"
          )))
    )
  })

  # Ana sonuç alanı — sorgu öncesi YATAY intro form, sonrası yan yana harita+tablo
  output$hist_results <- renderUI({
    if (!hist_query_made()) {
      # Varsayılan ilçe seçenekleri (İstanbul) — JS observer'ı şehir değişince
      # günceller, ama ilk render'da listenin dolu gelmesi için.
      ist_districts <- TR_DISTRICTS[["İstanbul"]]$name
      district_choices_init <- c(
        "Tüm şehir" = "_all_",
        setNames(ist_districts, ist_districts)
      )
      return(div(
        class = "hist-intro-h",
        div(class = "hist-intro-h-header",
          div(icon("magnifying-glass-chart", class = "hist-intro-h-icon")),
          h3("Geçmiş Deprem Sorgusu"),
          p("Filtreleri seç ve Sorgula'ya bas. Sonuçlar geldikten sonra ",
            "sol panelden filtreleri değiştirebilirsin.")
        ),
        div(class = "hist-intro-h-card",
          layout_columns(
            col_widths = c(4, 4, 4),
            # Kolon 1 — Kapsam + Bölge/Şehir+İlçe
            div(
              radioButtons("hist_scope", "Kapsam",
                           choices = c("Bölge" = "region", "Şehir" = "city"),
                           selected = "region", inline = TRUE),
              conditionalPanel(
                condition = "input.hist_scope == 'region'",
                selectInput("hist_region", "Bölge",
                            choices  = names(WORLD_REGIONS),
                            selected = "Türkiye", width = "100%")
              ),
              conditionalPanel(
                condition = "input.hist_scope == 'city'",
                selectInput("hist_city", "Şehir",
                            choices  = TR_CITIES$name,
                            selected = "İstanbul", width = "100%",
                            selectize = FALSE),
                selectInput("hist_district", "İlçe (opsiyonel)",
                            choices  = district_choices_init,
                            selected = "_all_", width = "100%",
                            selectize = FALSE)
              )
            ),
            # Kolon 2 — Tarih + Hızlı seçim
            div(
              dateRangeInput("hist_dates", "Tarih aralığı",
                             start    = Sys.Date() - 5 * 365,
                             end      = Sys.Date(),
                             min      = "1900-01-01",
                             max      = Sys.Date(),
                             format   = "dd.mm.yyyy",
                             language = "tr",
                             separator = " — ",
                             width = "100%"),
              div(class = "hist-presets",
                tags$small(class = "text-muted", "Hızlı tarih:"),
                div(class = "hist-presets-row",
                  actionButton("hist_preset_5y",  "5 yıl",  class = "btn-outline-secondary btn-sm"),
                  actionButton("hist_preset_25y", "25 yıl", class = "btn-outline-secondary btn-sm"),
                  actionButton("hist_preset_50y", "50 yıl", class = "btn-outline-secondary btn-sm"),
                  actionButton("hist_preset_all", "Tümü",   class = "btn-outline-secondary btn-sm")
                )
              )
            ),
            # Kolon 3 — Min büyüklük + (şehir modunda) Yarıçap
            div(
              sliderInput("hist_minmag", "Minimum büyüklük",
                          min = 4, max = 9, value = 5, step = 1,
                          width = "100%"),
              conditionalPanel(
                condition = "input.hist_scope == 'city'",
                sliderInput("hist_radius", "Yarıçap (km)",
                            min = 20, max = 500, value = 150, step = 10,
                            width = "100%")
              )
            )
          ),
          actionButton("hist_query", "Sorgula",
                       icon = icon("magnifying-glass"),
                       class = "btn-primary btn-lg w-100",
                       style = "margin-top: 14px; padding: 10px;"),
          div(class = "hist-intro-help",
              style = "display:block; text-align:center; margin-top:14px;",
              icon("circle-info"),
              HTML(paste0(
                "&nbsp;<b>USGS veri kapsamı:</b> ",
                "1900&ndash;1973 sadece M&nbsp;&geq;&nbsp;6, ",
                "1990+ kapsamlı M&nbsp;&geq;&nbsp;4."
              )))
        )
      ))
    }
    # Sorgu sonrası: kompakt stat kartları + yan yana harita/tablo
    tagList(
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        stat_card("Sonuç Sayısı",      "hist_count",  "list-ol",            "#0891b2"),
        stat_card("Maksimum Büyüklük", "hist_maxmag", "triangle-exclamation","#e11d48"),
        stat_card("Ortalama Derinlik", "hist_depth",  "ruler-vertical",      "#f59e0b"),
        stat_card("Tarih Aralığı",     "hist_range",  "calendar",            "#10b981")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          class = "hist-result-card",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tagList(icon("map-location-dot"), " Sonuç Haritası"),
            tags$small(class = "text-muted",
                       textOutput("hist_summary", inline = TRUE))
          ),
          leafletOutput("hist_map", height = "calc(100vh - 320px)")
        ),
        card(
          class = "hist-result-card",
          card_header(icon("table"), " Sonuç Tablosu"),
          div(style = "max-height: calc(100vh - 320px); overflow-y: auto;",
              DTOutput("hist_table"))
        )
      )
    )
  })

  output$hist_map <- renderLeaflet({
    m  <- hist_base_map()
    df <- hist_data()
    if (is.null(df) || nrow(df) == 0) return(m)
    # Markerları doğrudan haritaya ekle (proxy timing sorunlarını önler:
    # renderUI ile leafletOutput yeniden oluştuğunda proxy komutları
    # eski/asılmamış elemana gidip kayboluyordu).
    m <- draw_quakes(m, df,
                     cluster = nrow(df) > 200,
                     pulse_threshold = 6.5)
    lon_rng <- range(df$lon, na.rm = TRUE)
    lat_rng <- range(df$lat, na.rm = TRUE)
    if (all(is.finite(c(lon_rng, lat_rng))) &&
        length(lon_rng) == 2 && length(lat_rng) == 2) {
      m <- m |> fitBounds(lng1 = lon_rng[1], lat1 = lat_rng[1],
                          lng2 = lon_rng[2], lat2 = lat_rng[2])
    }
    m
  })

  # Tarih aralığı hızlı preset butonları
  observeEvent(input$hist_preset_5y, {
    updateDateRangeInput(session, "hist_dates",
                         start = Sys.Date() - 5 * 365,
                         end   = Sys.Date())
  })
  observeEvent(input$hist_preset_25y, {
    updateDateRangeInput(session, "hist_dates",
                         start = Sys.Date() - 25 * 365,
                         end   = Sys.Date())
  })
  observeEvent(input$hist_preset_50y, {
    updateDateRangeInput(session, "hist_dates",
                         start = Sys.Date() - 50 * 365,
                         end   = Sys.Date())
  })
  observeEvent(input$hist_preset_all, {
    updateDateRangeInput(session, "hist_dates",
                         start = as.Date("1900-01-01"),
                         end   = Sys.Date())
  })

  observeEvent(input$hist_query, {
    # Tarih aralığı kontrolü — kullanıcı tarih alanını silebilir, NULL/NA olabilir
    if (is.null(input$hist_dates) || length(input$hist_dates) < 2 ||
        anyNA(input$hist_dates)) {
      showNotification("Lütfen geçerli bir tarih aralığı seç.",
                       type = "error", duration = 5)
      return()
    }
    if (input$hist_dates[1] > input$hist_dates[2]) {
      showNotification("Başlangıç tarihi bitiş tarihinden sonra olamaz.",
                       type = "error", duration = 5)
      return()
    }

    # Form değerlerini snapshot'la — re-render sırasında input'lar
    # kaybolabilir, sidebar bu snapshot'tan dolacak.
    hist_form_snapshot(list(
      scope    = input$hist_scope,
      region   = input$hist_region,
      city     = input$hist_city,
      district = input$hist_district,
      dates    = input$hist_dates,
      minmag   = input$hist_minmag,
      radius   = input$hist_radius
    ))

    # İlk sorgu — intro yerine sonuç paneli açılsın
    first_query <- !hist_query_made()
    hist_query_made(TRUE)
    # İlk sorguda sol sidebar'ı otomatik aç (filtreler için)
    if (first_query) {
      tryCatch(
        bslib::sidebar_toggle("hist_sidebar", open = TRUE, session = session),
        error = function(e) NULL  # eski bslib sürümü için sessiz fallback
      )
    }

    df <- withProgress(
      message = "USGS sorgusu yapılıyor...", value = 0.3,
      detail = "Bu birkaç saniye sürebilir", {
        if (input$hist_scope == "region") {
          bbox <- WORLD_REGIONS[[input$hist_region]]
          fetch_usgs_history(
            starttime    = input$hist_dates[1],
            endtime      = input$hist_dates[2],
            minmagnitude = input$hist_minmag,
            bbox         = bbox,
            limit        = HIST_LIMIT
          )
        } else {
          # Browser'dan UTF-8 gelen string'leri açıkça işaretle (Windows R quirk)
          city_name <- enc2utf8(input$hist_city %||% "")
          district  <- enc2utf8(input$hist_district %||% "_all_")

          # Şehir merkezini güvenli şekilde çek — eşleşme yoksa Türkiye merkezi
          city_center <- function(name) {
            city <- TR_CITIES[TR_CITIES$name == name, ]
            if (nrow(city) == 0) {
              list(lat = 39.0, lon = 35.0)  # Türkiye merkezi (fallback)
            } else {
              list(lat = city$lat[1], lon = city$lon[1])
            }
          }

          # İlçe seçili mi? Varsa onun merkezi + daha dar yarıçap
          if (!is.null(district) && district != "_all_" &&
              city_name %in% names(TR_DISTRICTS)) {
            ds  <- TR_DISTRICTS[[city_name]]
            row <- ds[ds$name == district, ]
            if (nrow(row) > 0) {
              center    <- list(lat = row$lat[1], lon = row$lon[1])
              radius_km <- min(input$hist_radius, 50)  # ilçe için max 50 km
            } else {
              center    <- city_center(city_name)
              radius_km <- input$hist_radius
            }
          } else {
            center    <- city_center(city_name)
            radius_km <- input$hist_radius
          }

          fetch_usgs_history(
            starttime    = input$hist_dates[1],
            endtime      = input$hist_dates[2],
            minmagnitude = input$hist_minmag,
            center       = center,
            radius_km    = radius_km,
            limit        = HIST_LIMIT
          )
        }
      }
    )

    n <- nrow(df)
    if (n == 0) {
      status <- attr(df, "fetch_status") %||% "empty"
      http   <- attr(df, "http_status")
      msg <- if (status == "fail") {
        paste0("USGS sorgusu başarısız oldu",
               if (!is.null(http)) sprintf(" (HTTP %s)", http) else "",
               ". İnternet bağlantısını kontrol et veya tekrar dene.")
      } else {
        paste0("Bu kriterlere uyan deprem bulunamadı. ",
               "Tarih aralığını genişletmeyi veya minimum büyüklüğü ",
               "düşürmeyi dene.")
      }
      showNotification(msg, type = "warning", duration = 7)
    } else if (n >= HIST_LIMIT) {
      showNotification(
        sprintf("İlk %s sonuç gösteriliyor — daha fazlası için tarih aralığını daraltın veya min. büyüklüğü artırın.",
                fmt_tr_int(HIST_LIMIT)),
        type = "warning", duration = 8)
    } else {
      showNotification(sprintf("%s deprem bulundu.", fmt_tr_int(n)),
                       type = "message", duration = 4)
    }

    # En yakın plaka sınırı bilgisini ekle (popup'ta gösterilir).
    # Çok sayıda kayıt için ilerleme bildirimi göster.
    if (n > 0) {
      df <- withProgress(
        message = "Plaka sınırı mesafeleri hesaplanıyor...",
        detail  = sprintf("%s kayıt", fmt_tr_int(n)),
        value   = 0.5,
        nearest_fault(df)
      )
    }
    hist_data(df)
  })

  # NOT: Marker çizimi artık output$hist_map renderLeaflet içinde —
  # leafletProxy timing sorunu (renderUI ile output yeniden mount olunca
  # proxy komutlarının kaybolması) bu şekilde çözüldü.

  # Kart çıktıları
  output$hist_count <- renderText({
    df <- hist_data()
    if (is.null(df)) "—" else fmt_tr_int(nrow(df))
  })
  output$hist_maxmag <- renderText({
    df <- hist_data()
    if (is.null(df) || nrow(df) == 0) return("—")
    mx <- suppressWarnings(max(df$mag, na.rm = TRUE))
    if (!is.finite(mx)) return("—")
    sprintf("M %.1f", mx)
  })
  output$hist_depth <- renderText({
    df <- hist_data()
    if (is.null(df) || nrow(df) == 0) return("—")
    mn <- suppressWarnings(mean(df$depth_km, na.rm = TRUE))
    if (!is.finite(mn)) return("—")
    sprintf("%.1f km", mn)
  })
  output$hist_range <- renderText({
    df <- hist_data()
    if (is.null(df) || nrow(df) == 0) return("—")
    rng <- range(df$time, na.rm = TRUE)
    sprintf("%s – %s",
            format(rng[1], "%Y"), format(rng[2], "%Y"))
  })

  output$hist_summary <- renderText({
    df <- hist_data()
    if (is.null(df)) return("Henüz sorgu yapılmadı")
    if (nrow(df) == 0) return("Sonuç yok")
    sprintf("%d deprem", nrow(df))
  })

  output$hist_table <- renderDT({
    df <- hist_data()
    if (is.null(df) || nrow(df) == 0) {
      empty <- data.frame(
        Mesaj = "Sol panelden filtreleri seçip 'Sorgula' butonuna bas.",
        check.names = FALSE
      )
      return(datatable(empty, rownames = FALSE,
                       options = list(dom = "t", paging = FALSE)))
    }
    show <- df |>
      arrange(desc(mag)) |>
      transmute(
        date       = format(time, "%d.%m.%Y %H:%M UTC"),
        magnitude  = round(mag, 1),
        depth      = round(depth_km, 1),
        place      = ifelse(is.na(place) | !nzchar(place),
                            "Bilinmeyen konum", place),
        continent  = continent,
        coords     = sprintf("%.3f, %.3f", lat, lon),
        url        = ifelse(is.na(url) | !nzchar(url),
                            "—",
                            sprintf("<a href='%s' target='_blank'>USGS</a>",
                                    url))
      )
    datatable(
      show, escape = FALSE, rownames = FALSE,
      colnames = c("Tarih", "Büyüklük", "Derinlik", "Yer",
                   "Kıta", "Koordinat", "Detay"),
      options = list(
        pageLength = 25, order = list(list(1, "desc")),
        dom = "ftip",
        language = list(
          emptyTable   = "Sonuç yok",
          zeroRecords  = "Aramayla eşleşen kayıt yok",
          info         = "_TOTAL_ kayıt: _START_ – _END_",
          infoEmpty    = "0 kayıt",
          infoFiltered = "(_MAX_ kayıttan filtrelendi)",
          lengthMenu   = "Sayfa başına _MENU_ kayıt",
          search       = "Ara:",
          paginate     = list(previous = "Önceki", `next` = "Sonraki")
        )
      ),
      class = "compact stripe hover"
    ) |>
      formatStyle("magnitude",
        background = styleColorBar(c(0, 10), "#fee2e2"),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold")
  })

  # ---- Türkiye İstatistikleri Sekmesi --------------------------------
  # raw_data() refresh_tick'i zaten içeriyor — ayrıca çağırmaya gerek yok.
  tr_data <- reactive({
    refresh_tick()  # CSV yoksa periyodik tekrar dene (kullanıcı script'i çalıştırınca yakala)
    turkey_stats(raw_data())
  })

  # CSV yoksa kullanıcıya net açıklama göster
  output$tr_data_status <- renderUI({
    refresh_tick()  # her yenilemede yeniden kontrol et
    # Path'i her seferinde yeniden çöz (kullanıcı setwd yapmış olabilir)
    p <- resolve_turkey_history_path()
    if (file.exists(p)) {
      hist <- turkey_history()
      if (!is.null(hist) && nrow(hist) > 0) return(NULL)
    }
    div(class = "info-banner",
        style = paste("background:#fef2f2; border-left:4px solid #dc2626;",
                      "color:#7f1d1d; font-size:13px; padding:14px 18px;",
                      "margin-bottom:12px; border-radius:8px;",
                      "line-height:1.55;"),
        tags$div(
          style = "display:flex; align-items:center; gap:10px;
                   font-weight:700; color:#991b1b; margin-bottom:8px;
                   font-size:15px;",
          icon("triangle-exclamation"),
          "Türkiye geçmiş verisi henüz indirilmedi"),
        tags$p(style = "margin-bottom:8px;",
          "Bu sekme ", tags$code("data/turkey_history.csv"),
          " dosyasından beslenir (1900+ Türkiye depremleri; AFAD 1990+ M≥4, USGS 1900-1989 M≥6). ",
          "Dosya yok ya da boş — tüm istatistikler ‘—’ görünüyor."),
        tags$p(style = "margin-bottom:6px;",
          tags$b("Çözüm:"),
          " RStudio konsoluna aşağıdaki komutu yazıp çalıştır ",
          "(birkaç dakika sürer):"),
        tags$pre(style = "background:#fff; border:1px solid #fecaca;
                          padding:10px; border-radius:6px; margin:6px 0;
                          font-size:13px; color:#0f172a;",
          'source("data/fetch_turkey_history.R")'),
        tags$p(style = "margin-bottom:0; font-size:12px; color:#7f1d1d;",
          "Tamamlandığında uygulamayı tekrar başlatmana gerek yok; ",
          "60 saniye içinde otomatik dolacak."))
  })

  output$tr_last24 <- renderText({
    s <- tr_data()
    if (is.null(s$last24) || !is.numeric(s$last24) || !is.finite(s$last24))
      return("—")
    paste0(fmt_tr_int(s$last24), " deprem")
  })
  output$tr_total_m5 <- renderText({
    s <- tr_data()
    if (!is.numeric(s$total_m5) || !is.finite(s$total_m5)) return("—")
    fmt_tr_int(s$total_m5)
  })
  output$tr_avg_depth <- renderText({
    s <- tr_data()
    if (!is.numeric(s$avg_depth) || !is.finite(s$avg_depth)) return("—")
    sprintf("%.1f km", s$avg_depth)
  })
  output$tr_year_range <- renderText({
    s <- tr_data()
    if (is.null(s$by_year) || nrow(s$by_year) == 0) return("—")
    sprintf("%d-%d", min(s$by_year$year, na.rm = TRUE),
                     max(s$by_year$year, na.rm = TRUE))
  })

  output$tr_biggest_box <- renderUI({
    s <- tr_data()
    if (is.null(s$biggest)) return(HTML("<i>Veri yok.</i>"))
    b <- s$biggest
    city <- extract_city(b$place)
    depth_txt <- if (is.finite(b$depth_km)) sprintf("%.1f km", b$depth_km) else "—"
    HTML(sprintf(
      "<div style='display:flex;gap:16px;align-items:center;padding:8px 4px'>
        <div style='flex-shrink:0;width:80px;height:80px;border-radius:14px;
                    background:%s;color:#fff;display:flex;flex-direction:column;
                    align-items:center;justify-content:center;
                    box-shadow:0 6px 20px %s55;font-family:Inter'>
          <div style='font-size:10px;letter-spacing:.5px;opacity:.85'>BÜYÜKLÜK</div>
          <div style='font-size:30px;font-weight:700;line-height:1'>%.1f</div>
        </div>
        <div style='flex:1'>
          <div style='font-size:11px;color:#94a3b8;letter-spacing:.5px;
                      text-transform:uppercase;font-weight:700'>EN BÜYÜK DEPREM</div>
          <div style='font-size:20px;font-weight:700;color:#0f172a;
                      margin:4px 0'>%s</div>
          <div style='font-size:13px;color:#475569'>
            <b>Tarih:</b> %s &nbsp;|&nbsp;
            <b>Bölge:</b> %s &nbsp;|&nbsp;
            <b>Derinlik:</b> %s &nbsp;|&nbsp;
            <b>Konum:</b> %.3f, %.3f
          </div>
        </div>
      </div>",
      mag_color(b$mag), mag_color(b$mag), b$mag,
      ifelse(is.na(b$place), "Bilinmeyen konum", b$place),
      format(b$time, "%d.%m.%Y %H:%M UTC"),
      city, depth_txt, b$lat, b$lon
    ))
  })

  output$tr_yearly_plot <- renderPlotly({
    s <- tr_data()
    if (is.null(s$by_year) || nrow(s$by_year) == 0) return(plotly_empty())
    df <- s$by_year
    # Vurgu: 1999 İzmit, 2011 Van, 2020 İzmir/Sisam, 2023 Kahramanmaraş
    highlight <- list(
      list(year = 1999L, label = "İzmit"),
      list(year = 2011L, label = "Van"),
      list(year = 2020L, label = "İzmir"),
      list(year = 2023L, label = "Maraş")
    )
    highlight_years <- vapply(highlight, function(h) h$year, integer(1))
    df$color <- ifelse(df$year %in% highlight_years, "#e11d48", "#06b6d4")
    df$label <- ifelse(df$year %in% highlight_years,
                       sprintf("<b>%d</b><br>%d deprem", df$year, df$n),
                       sprintf("%d<br>%d deprem", df$year, df$n))

    # Sadece veride bulunan yıllar için annotation üret (boş indeks → hata önle)
    annots <- Filter(Negate(is.null), lapply(highlight, function(h) {
      idx <- which(df$year == h$year)
      if (length(idx) == 0) return(NULL)
      list(x = h$year, y = df$n[idx[1]], yanchor = "bottom",
           text = h$label, showarrow = TRUE, arrowsize = .8,
           font = list(size = 11, color = "#b91c1c"))
    }))

    plot_ly(df) |>
      add_bars(x = ~year, y = ~n,
               marker = list(color = ~color, opacity = 0.85,
                             line = list(color = "#fff", width = 1)),
               text = ~label, hoverinfo = "text",
               name = "Deprem sayısı") |>
      layout(
        xaxis = list(title = "Yıl", tickangle = -45, dtick = 2),
        yaxis = list(title = "Deprem sayısı (M ≥ 4.0)"),
        showlegend = FALSE,
        margin = list(t = 30, b = 60, l = 50, r = 20),
        annotations = annots
      )
  })

  output$tr_mag_plot <- renderPlotly({
    s <- tr_data()
    if (is.null(s$history)) return(plotly_empty())
    df <- s$history
    df$cat <- cut(df$mag, breaks = c(4, 5, 6, 7, 10),
                  right = FALSE, include.lowest = TRUE,
                  labels = c("M 4-5", "M 5-6", "M 6-7", "M 7+"))
    cat_counts <- as.data.frame(table(df$cat))
    names(cat_counts) <- c("category", "n")
    cat_counts$color <- c("#facc15", "#f97316", "#dc2626", "#7f1d1d")
    plot_ly(cat_counts) |>
      add_bars(x = ~category, y = ~n,
               marker = list(color = ~color,
                             line = list(color = "#fff", width = 1)),
               text = ~fmt_tr_int(n),
               textposition = "outside",
               name = "Adet") |>
      layout(
        xaxis = list(title = "Kategori"),
        yaxis = list(title = "Deprem sayısı"),
        showlegend = FALSE,
        margin = list(t = 30, b = 40)
      )
  })

  output$tr_depth_plot <- renderPlotly({
    s <- tr_data()
    if (is.null(s$history)) return(plotly_empty())
    plot_ly(x = s$history$depth_km, type = "histogram", nbinsx = 40,
            marker = list(color = "#1abc9c",
                          line = list(color = "#0e8c70", width = 0.5))) |>
      layout(
        xaxis = list(title = "Derinlik (km)", range = c(0, 200)),
        yaxis = list(title = "Frekans"),
        margin = list(t = 30, b = 40)
      )
  })

  output$tr_top10 <- renderDT({
    s <- tr_data()
    if (is.null(s$history) || nrow(s$history) == 0)
      return(datatable(data.frame(message = "Veri yok"),
                       colnames = "Mesaj"))
    ordered <- s$history[order(-s$history$mag), ]
    top <- ordered[seq_len(min(10, nrow(ordered))), , drop = FALSE]
    show <- data.frame(
      date       = format(top$time, "%d.%m.%Y %H:%M UTC"),
      magnitude  = round(top$mag, 1),
      place      = top$place,
      city       = vapply(top$place, extract_city, character(1)),
      depth      = round(top$depth_km, 1),
      coords     = sprintf("%.3f, %.3f", top$lat, top$lon),
      stringsAsFactors = FALSE
    )
    datatable(show, rownames = FALSE,
              colnames = c("Tarih", "Büyüklük", "Yer", "Şehir",
                           "Derinlik", "Koordinat"),
              options = list(pageLength = 10, dom = "t",
                             order = list(list(1, "desc"))),
              class = "compact stripe hover") |>
      formatStyle("magnitude",
        background = styleColorBar(c(0, 10), "#fee2e2"),
        backgroundSize = "98% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold")
  })

}

shinyApp(ui, server)
