# Dünya Deprem Haritası

USGS verilerinden anlık dünya deprem aktivitesini interaktif bir
Shiny + leaflet haritasında gösterir. CBS dersi projesi.

![preview](https://earthquake.usgs.gov/img/eqhz_logo.png)

## Hızlı Başlangıç

> **Not:** Bu proje WSL `\\wsl.localhost\Ubuntu\home\xamki\emin\`
> klasöründe oluşturuldu. Windows RStudio'dan açmak için ya bu yola
> doğrudan gidebilir ya da klasörü Windows tarafına kopyalayabilirsin.

### 1. Projeyi RStudio'da Aç

RStudio → **File → Open Project / Open File…** → `app.R` dosyasını seç.

Veya `Files` panelinde **Set As Working Directory** butonuyla proje
klasörünü çalışma dizini yap.

### 2. Paketleri Kur (tek seferlik)

```r
source("install_packages.R")
```

Kurulum birkaç dakika sürebilir (özellikle `sf` ilk kez kurulurken).
Tüm satırların `OK` çıktığını gör.

### 3. Türkiye Geçmiş Verisi (opsiyonel, tek seferlik)

"Türkiye İstatistikleri" sekmesi `data/turkey_history.csv` dosyasını
kullanır. Dosya yoksa AFAD'ın `apiv2/event/filter` API'sinden çekmek için:

```r
source("data/fetch_turkey_history.R")
```

Birkaç dakika sürer (1900–bugün, yıl yıl). Bu adımı atlarsan diğer
sekmeler yine çalışır, sadece Türkiye İstatistikleri sekmesi boş görünür.

### 4. Uygulamayı Çalıştır

`app.R` açıkken sağ üstteki **Run App** butonuna bas (veya konsola):

```r
shiny::runApp()
```

Tarayıcıda açılır. Yan paneldeki filtrelerle oyna.

## Dosyalar

| Dosya | Görev |
|-------|-------|
| `app.R` | Shiny UI + server (giriş noktası) |
| `helpers.R` | USGS/Kandilli çekme, filtreleme, harita çizimi |
| `install_packages.R` | Bağımlılık kurulum scripti |
| `data/plate_boundaries.geojson` | Bird (2003) plaka sınırları |
| `data/turkey_history.csv` | Türkiye 1990+ tarihsel deprem CSV (script ile üretilir) |
| `data/fetch_turkey_history.R` | CSV'yi USGS query API'den indirir |
| `PLAN.md` | Proje planı ve teknik kararlar |
| `README.md` | Bu dosya |

## Özellikler

- 4 zaman aralığı: son 1 saat / 1 gün / 1 hafta / 1 ay
- 2 canlı veri kaynağı: USGS (dünya, M ≥ 4.5) + Kandilli (Türkiye, M ≥ 1.0)
- Minimum büyüklük ve maksimum derinlik filtreleri
- Tsunami uyarılı kayıtları izole etme
- 60 saniyede bir otomatik yenileme; başarısız çekimde önceki veri korunur
- 5 harita altlığı: Esri Sokak, National Geographic, Voyager, OSM, Uydu
- Global tektonik plaka sınırları katmanı (her deprem için en yakın
  plaka sınırı ve mesafe popup'ta)
- Isı haritası (heatmap) katmanı
- Tarihi büyük depremler katmanı (1939–2023, Türkiye + dünya)
- "Son Dakika" şeridi — en yeni 5 deprem; tıklayınca odak modu
- Zaman Seyahati slider'ı — geçmiş bir ana git, animasyonu oynat
- "Türkiye İstatistikleri" sekmesi: 1990+ tarihsel veriden yıllık
  grafik, en büyük 10 deprem, derinlik/kategori dağılımı
- "Geçmiş Depremler" sekmesi: USGS FDSN query API'si üzerinden
  tarih aralığı + bölge (Dünya/kıta/Türkiye) veya şehir+yarıçap
  + min büyüklük ile geçmiş depremleri sorgulama (1900'a kadar)
- Sıralanabilir/aranabilir tablo (sayfa/sıralama/arama yenilemeden
  sonra korunur)

## Veri Kaynakları

**USGS Earthquake Hazards Program** — public GeoJSON Summary Feeds:
<https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/>

Örnek uç nokta:
`https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson`

**Kandilli Rasathanesi (KOERI)** — Türkiye odaklı canlı feed
(üçüncü taraf API):
`https://api.orhanaydogdu.com.tr/deprem/kandilli/live`

**Plaka sınırları** — Bird (2003) PB2002 modeli; yerel dosya
`data/plate_boundaries.geojson`.

**Türkiye geçmişi** — AFAD'ın resmi `apiv2/event/filter` API'sinden
bir kez indirilmiş yerel CSV: `data/turkey_history.csv` (1900+, M ≥ 4,
yıl yıl çekilir).

## Sorun Giderme

**`sf` kurulumu hata veriyor**
Windows'ta genelde sorunsuz çalışır. Hata alırsan:

```r
install.packages("sf", type = "binary")
```

**Veri gelmiyor / boş harita**
Otomatik yenileme açıkken bekle veya `Şimdi yenile` butonuna bas.
Çekme başarısız olursa uygulama "önceki veri korunuyor" bildirimi
gösterir; ilk açılışta cache boş olduğundan harita boş kalır. USGS
uç noktasına VPN/proxy engellemesi olabilir; tarayıcıdan
`https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson`
adresini aç ve JSON dönüyor mu kontrol et.

**"Türkiye İstatistikleri" sekmesi boş**
`data/turkey_history.csv` yok demektir. Bir kez `source("data/fetch_turkey_history.R")`
çalıştır.

**Harita boyutu garip**
Tarayıcı sekmesini yenile (F5).
