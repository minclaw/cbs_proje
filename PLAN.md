# Dünya Deprem Haritası — Proje Planı

CBS dersi projesi. Shiny + leaflet + USGS GeoJSON akışı kullanarak
anlık deprem verilerini interaktif harita üzerinde gösteren bir web
uygulaması.

## Teknoloji Yığını

- **R** 4.5.2 (Windows / RStudio)
- **shiny** + **bslib** — web arayüzü ve tema
- **leaflet** — interaktif harita (OpenStreetMap, Esri katmanları)
- **httr2** + **jsonlite** — USGS GeoJSON API
- **sf** — coğrafi veri (CBS dersi gereği)
- **dplyr** — veri dönüşümü
- **DT** — deprem listesi tablosu
- **leaflet.extras** — ısı haritası (heatmap)

## Veri Kaynağı

USGS Earthquake Hazards Program — GeoJSON Summary Feeds.
Anahtar gerektirmez, dakikada bir güncellenir.

| Zaman | URL |
|-------|-----|
| Son 1 saat | `summary/all_hour.geojson` |
| Son 1 gün | `summary/all_day.geojson` |
| Son 7 gün | `summary/all_week.geojson` |
| Son 30 gün | `summary/all_month.geojson` |

Base URL: `https://earthquake.usgs.gov/earthquakes/feed/v1.0/`

Her özellik (feature) şunları içerir: `mag` (büyüklük), `place` (yer
açıklaması), `time` (Unix ms), `coordinates` (lon, lat, derinlik km),
`url`, `tsunami`, `sig` (önem skoru), `type`.

## Özellikler

1. **Harita** — Tüm depremler renkli/boyutlu daire işaretçi
   - Büyüklük → daire yarıçapı (logaritmik ölçek)
   - Büyüklük → renk (viridis/yeşil-sarı-kırmızı)
   - Tsunami uyarısı varsa farklı simge
   - İşaretçiye tıklayınca popup: yer, büyüklük, derinlik, zaman, USGS
     linki
2. **Filtreler** (yan panel)
   - Zaman aralığı: 1 saat / 1 gün / 1 hafta / 1 ay
   - Minimum büyüklük (slider, 0–9)
   - Maksimum derinlik (slider, 0–700 km)
   - Sadece tsunami uyarılı olanlar (checkbox)
3. **Otomatik Yenileme** — 60 sn'de bir veriyi yeniden çek
4. **İstatistik Panosu** — toplam sayı, max büyüklük, ortalama derinlik
5. **Tablo Sekmesi** — DT ile sıralanabilir/aranabilir deprem listesi
6. **Isı Haritası Katmanı** — yoğunluk gösterimi (toggle)
7. **Türkçe arayüz**

## Dosya Yapısı

```
emin/
├── app.R              # Shiny UI + server
├── helpers.R          # USGS verisi çekme + harita fonksiyonları
├── install_packages.R # Tek seferlik kütüphane kurulumu
├── README.md          # Kurulum ve çalıştırma
└── PLAN.md            # Bu dosya
```

## Yol Haritası

- [x] Plan ve teknoloji seçimi
- [ ] `install_packages.R` ile kütüphaneleri kur
- [ ] `helpers.R` — `fetch_usgs()` ve `build_map()` fonksiyonları
- [ ] `app.R` — UI iskeleti (sidebar + harita + tablo sekmeleri)
- [ ] `app.R` — server reaktif zinciri ve filtreler
- [ ] Otomatik yenileme + ısı haritası
- [ ] CBS sunum metni / ekran görüntüleri
