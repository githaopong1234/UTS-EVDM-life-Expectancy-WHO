# Memanggil package
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(shiny)
library(bslib)
library(readxl)
Life_Expectancy_Data <- read_excel("D:/TIKA GITA OPONG/SEMESTER 7/EKSPLORASI & VISUALISASI DATA MULTIVARIATE/Life Expectancy Data.xlsx")
View(Life_Expectancy_Data)
# Melihat struktur data
str(Life_Expectancy_Data)
# Melihat dimensi data
dim(Life_Expectancy_Data)
# Melihat nama variabel
names(Life_Expectancy_Data)
# Melihat beberapa baris pertama
head(Life_Expectancy_Data)

# Melihat ringkasan seluruh data
summary(Life_Expectancy_Data)
# Memilih variabel penelitian
data_eda <- Life_Expectancy_Data %>%
  select(
    Country,
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    `BMI`,
    GDP,
    Schooling
  )

# Melihat data terpilih
View(data_eda)
# Memeriksa struktur data terpilih
str(data_eda)
# Fungsi konversi angka
konversi_angka <- function(x) {
  x <- str_trim(as.character(x))
  hasil <- ifelse(
    str_detect(x, "^\\d{1,2}:\\d{2}(:\\d{2})?$"),
    {
      bagian <- str_split_fixed(x, ":", 3)
      as.numeric(bagian[,1]) +
        as.numeric(bagian[,2]) / 100 +
        ifelse(
          bagian[,3] == "",
          0,
          as.numeric(bagian[,3]) / 10000
        )
    },
    parse_number(x)
  )
  as.numeric(hasil)
}
# Memilih variabel penelitian dan mengubah tipe data
data_eda <- data_awal %>%
  select(
    Country,
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  mutate(
    `Adult Mortality` = konversi_angka(`Adult Mortality`),
    `HIV/AIDS` = konversi_angka(`HIV/AIDS`),
    BMI = konversi_angka(BMI),
    GDP = konversi_angka(GDP),
    Schooling = konversi_angka(Schooling),
    Country = as.factor(Country),
    Status = as.factor(Status)
  )
# Memeriksa hasil
str(data_eda)
View(data_eda)
# Menghitung missing value
missing_data <- data_eda %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Jumlah Missing"
  ) %>%
  mutate(
    Persentase = round(
      `Jumlah Missing` / nrow(data_eda) * 100, 2
    )
  )
print(missing_data)
library(dplyr)
library(tidyr)
library(knitr)

# Fungsi menghitung modus
hitung_modus <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return(NA_real_)
  
  frekuensi <- table(x)
  
  modus <- names(frekuensi)[
    frekuensi == max(frekuensi)
  ]
  
  as.numeric(modus[1])
}


# Statistik deskriptif numerik
deskriptif_numerik <- data_eda %>%
  
  summarise(
    across(
      c(
        `Adult Mortality`,
        `HIV/AIDS`,
        BMI,
        GDP,
        Schooling
      ),
      
      list(
        N = ~ sum(!is.na(.)),
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        Modus = ~ hitung_modus(.),
        Minimum = ~ min(., na.rm = TRUE),
        Maksimum = ~ max(., na.rm = TRUE),
        Range = ~ diff(range(., na.rm = TRUE)),
        Q1 = ~ quantile(., 0.25, na.rm = TRUE),
        Q3 = ~ quantile(., 0.75, na.rm = TRUE),
        Varians = ~ var(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE),
        CV = ~ sd(., na.rm = TRUE) /
          mean(., na.rm = TRUE) * 100
      )
    )
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = c("Variabel", ".value"),
    names_pattern =
      "^(.*)_(N|Mean|Median|Modus|Minimum|Maksimum|Range|Q1|Q3|Varians|SD|CV)$"
  ) %>%
  
  mutate(
    across(
      -Variabel,
      ~ round(., 3)
    )
  )


# Menampilkan tabel di R Markdown
knitr::kable(
  deskriptif_numerik,
  caption = "Tabel Statistik Deskriptif Variabel Numerik",
  align = "c"
)
## deskriptif kategorik
deskriptif_status <- data_eda %>%
  
  count(Status, name = "Frekuensi") %>%
  
  mutate(
    Persentase = round(
      Frekuensi / sum(Frekuensi) * 100,
      2
    )
  )

knitr::kable(
  deskriptif_status,
  caption = "Tabel Distribusi Frekuensi Status",
  align = "c"
)
deskriptif_country <- data_eda %>%
  
  count(Country, name = "Frekuensi") %>%
  
  mutate(
    Persentase = round(
      Frekuensi / sum(Frekuensi) * 100,
      2
    )
  ) %>%
  
  arrange(desc(Frekuensi))

knitr::kable(
  deskriptif_country,
  caption = "Tabel Distribusi Frekuensi Country",
  align = "c"
)
## UNIVARIAT VARIABEL NUMERIK
library(ggplot2)
library(tidyr)
library(dplyr)

data_numerik_long <- data_eda %>%
  select(
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Nilai"
  )


ggplot(data_numerik_long, aes(x = Nilai)) +
  geom_histogram(
    bins = 30,
    fill = "skyblue",
    color = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Histogram Distribusi Variabel Numerik",
    x = "Nilai",
    y = "Frekuensi"
  ) +
  theme_minimal()
## BOXPLOT
ggplot(data_numerik_long, aes(x = "", y = Nilai)) +
  geom_boxplot(
    fill = "lightgreen",
    color = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Boxplot Variabel Numerik",
    x = "Variabel",
    y = "Nilai"
  ) +
  theme_minimal()
## UNIVARIAT VARIABEL KATEGORIK
ggplot(data_eda, aes(x = Status, fill = Status)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribusi Frekuensi Status",
    x = "Status",
    y = "Frekuensi",
    fill = "Status"
  ) +
  theme_minimal()
data_country_top5 <- data_eda %>%
  count(Country, name = "Frekuensi") %>%
  slice_max(order_by = Frekuensi, n = 5)

ggplot(
  data_country_top5,
  aes(x = reorder(Country, Frekuensi), y = Frekuensi)
) +
  geom_col(fill = "skyblue", color = "black") +
  coord_flip() +
  labs(
    title = "5 Negara dengan Frekuensi Observasi Tertinggi",
    x = "Negara",
    y = "Frekuensi"
  ) +
  theme_minimal()
# BIVARIAT
## Korelasi Pearson
variabel_numerik <- c(
  "Adult Mortality",
  "HIV/AIDS",
  "BMI",
  "GDP",
  "Schooling"
)

pasangan_variabel <- combn(
  variabel_numerik,
  2,
  simplify = FALSE
)

hasil_korelasi <- map_dfr(pasangan_variabel, function(p) {
  
  data_pasangan <- data_eda %>%
    select(all_of(p)) %>%
    drop_na()
  
  uji <- cor.test(
    data_pasangan[[1]],
    data_pasangan[[2]],
    method = "pearson"
  )
  
  data.frame(
    Variabel_1 = p[1],
    Variabel_2 = p[2],
    N = nrow(data_pasangan),
    Korelasi_Pearson = unname(uji$estimate),
    P_Value = uji$p.value
  )
})

hasil_korelasi <- hasil_korelasi %>%
  mutate(
    Korelasi_Pearson = round(Korelasi_Pearson, 3),
    P_Value = round(P_Value, 4)
  )

knitr::kable(
  hasil_korelasi,
  caption = "Hasil Uji Korelasi Pearson",
  align = "c"
)
## Boxplot berdasarkan Status
data_boxplot <- data_eda %>%
  select(
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  pivot_longer(
    cols = -Status,
    names_to = "Variabel",
    values_to = "Nilai"
  )

ggplot(
  data_boxplot,
  aes(x = Status, y = Nilai, fill = Status)
) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Boxplot Variabel Numerik Berdasarkan Status",
    x = "Status Negara",
    y = "Nilai",
    fill = "Status"
  ) +
  theme_minimal()
## Scatter Plot dengan Garis Linear
ggplot(
  data_eda,
  aes(x = Schooling, y = `Adult Mortality`)
) +
  geom_point(
    color = "steelblue",
    size = 2,
    alpha = 0.7,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    na.rm = TRUE
  ) +
  labs(
    title = "Hubungan Schooling dengan Adult Mortality",
    x = "Schooling",
    y = "Adult Mortality"
  ) +
  theme_minimal()
## Correlation Plot

library(corrplot)

matriks_korelasi <- data_eda %>%
  select(all_of(variabel_numerik)) %>%
  cor(
    use = "pairwise.complete.obs",
    method = "pearson"
  )

corrplot(
  matriks_korelasi,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  title = "Correlation Plot Antarvariabel Numerik",
  mar = c(0, 0, 2, 0)
)
## MULTIVARIAT
library(dplyr)
library(ggplot2)
library(GGally)

data_multivariat <- data_eda %>%
  select(
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  drop_na()

# Melihat jumlah observasi
nrow(data_multivariat)
vektor_rata_rata <- colMeans(data_multivariat)
vektor_rata_rata
matriks_kovariansi <- cov(data_multivariat)
matriks_kovariansi
## Scatter Plot Multivariat menggunakan GGally::ggpairs
GGally::ggpairs(
  data_multivariat,
  upper = list(
    continuous = GGally::wrap(
      "cor",
      colour = "darkgreen",
      size = 3
    )
  ),
  lower = list(
    continuous = GGally::wrap(
      "points",
      colour = "red",
      alpha = 0.4,
      size = 0.8
    )
  ),
  diag = list(
    continuous = GGally::wrap(
      "densityDiag",
      fill = "darkgreen",
      colour = "red",
      alpha = 0.5
    )
  )
) +
  ggtitle("Scatter Plot Multivariat Antarvariabel Numerik")
# Memeriksa variansi setiap variabel
apply(data_multivariat, 2, var)

data_standar <- as.data.frame(scale(data_multivariat))

# Melihat variansi setelah standardisasi
apply(data_standar, 2, var)
matriks_korelasi <- cor(data_standar)
round(matriks_korelasi, 4)
matriks_kovariansi <- cov(data_standar)
qr(matriks_kovariansi)$rank
ncol(data_standar)
det(matriks_kovariansi)
# Menghitung matriks kovariansi data standar
matriks_kovariansi_standar <- cov(data_standar)

# Menghitung jarak Mahalanobis
jarak_mahalanobis <- mahalanobis(
  data_standar,
  center = colMeans(data_standar),
  cov = matriks_kovariansi_standar
)
# Menampilkan hasil
head(jarak_mahalanobis)
# Derajat bebas
df <- ncol(data_standar)

# Batas Chi-Square dengan alpha 0,025
batas_mahalanobis <- qchisq(
  0.975,
  df = df
)
batas_mahalanobis
hasil_outlier <- data.frame(
  Observasi = 1:nrow(data_standar),
  Jarak_Mahalanobis = jarak_mahalanobis,
  Status = ifelse(
    jarak_mahalanobis > batas_mahalanobis,
    "Outlier",
    "Tidak Outlier"
  )
)
# Menampilkan data outlier
hasil_outlier %>%
  filter(Status == "Outlier")
ggplot(
  hasil_outlier,
  aes(
    x = Observasi,
    y = Jarak_Mahalanobis,
    color = Status
  )
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = batas_mahalanobis,
    color = "darkgreen",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Outlier" = "red",
      "Tidak Outlier" = "darkgreen"
    )
  ) +
  labs(
    title = "Deteksi Outlier Multivariat dengan Jarak Mahalanobis",
    x = "Observasi",
    y = "Jarak Mahalanobis Kuadrat",
    color = "Status"
  ) +
  theme_minimal()
## Evaluasi Normalitas Multivariat dengan Chi-Square Q-Q Plot
# Mengurutkan jarak Mahalanobis
jarak_urut <- sort(jarak_mahalanobis)

# Menentukan jumlah observasi dan variabel
n <- length(jarak_urut)
p <- ncol(data_standar)

# Menghitung kuantil Chi-Square
kuantil_chisquare <- qchisq(
  ppoints(n),
  df = p
)

# Membuat data Q-Q Plot
data_qq <- data.frame(
  Chi_Square = kuantil_chisquare,
  Mahalanobis = jarak_urut
)
# Membuat Chi-Square Q-Q Plot
ggplot(
  data_qq,
  aes(x = Chi_Square, y = Mahalanobis)
) +
  geom_point(
    color = "red",
    alpha = 0.6
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "darkgreen",
    linewidth = 1
  ) +
  labs(
    title = "Chi-Square Q-Q Plot Normalitas Multivariat",
    x = "Kuantil Teoretis Chi-Square",
    y = "Jarak Mahalanobis Kuadrat"
  ) +
  theme_minimal()
## Konfirmasi Normalitas Variat dengan Shapiro-Wilk
hasil_shapiro <- lapply(
  names(data_multivariat),
  function(variabel) {
    
    uji <- shapiro.test(
      data_multivariat[[variabel]]
    )
    
    data.frame(
      Variabel = variabel,
      Statistik_W = unname(uji$statistic),
      P_Value = uji$p.value,
      Keputusan = ifelse(
        uji$p.value > 0.05,
        "Tidak menolak H0",
        "Menolak H0"
      )
    )
  }
) %>%
  bind_rows()

# Menampilkan hasil pengujian
hasil_shapiro %>%
  mutate(
    Statistik_W = round(Statistik_W, 4),
    P_Value = signif(P_Value, 4)
  ) %>%
  knitr::kable(
    caption = "Hasil Uji Normalitas Shapiro-Wilk",
    align = "c"
  )
## REGRESI LINEAR BERGANDA
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(car)
library(lmtest)

data_regresi <- data_awal %>%
  select(
    `Life expectancy`,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  mutate(
    across(
      everything(),
      konversi_angka
    )
  ) %>%
  drop_na()

# Melihat struktur data
str(data_regresi)

# Melihat jumlah observasi
nrow(data_regresi)
## Identifikasi pola hubungan X dan Y
variabel_x <- c(
  "Adult Mortality",
  "HIV/AIDS",
  "BMI",
  "GDP",
  "Schooling"
)


data_scatter <- data_regresi %>%
  pivot_longer(
    cols = all_of(variabel_x),
    names_to = "Variabel_X",
    values_to = "Nilai_X"
  )

ggplot(
  data_scatter,
  aes(x = Nilai_X, y = `Life expectancy`)
) +
  geom_point(
    color = "#800000",
    alpha = 0.4,
    size = 1.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#000080"
  ) +
  facet_wrap(
    ~ Variabel_X,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    title = "Pola Hubungan Variabel X dengan Life Expectancy",
    x = "Variabel Bebas (X)",
    y = "Life Expectancy (Y)"
  ) +
  theme_minimal()
## Estimasi Regresi Linear Berganda
model_regresi <- lm(
  `Life expectancy` ~
    `Adult Mortality` +
    `HIV/AIDS` +
    BMI +
    GDP +
    Schooling,
  data = data_regresi
)

# Menampilkan hasil estimasi
summary(model_regresi)
hasil_estimasi <- broom::tidy(
  model_regresi,
  conf.int = TRUE
)

hasil_estimasi %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 4)
    )
  ) %>%
  knitr::kable(
    caption = "Hasil Estimasi Regresi Linear Berganda",
    align = "c"
  )
hasil_model <- broom::glance(model_regresi)

hasil_model %>%
  select(
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  ) %>%
  mutate(
    across(
      everything(),
      ~ round(., 4)
    )
  ) %>%
  knitr::kable(
    caption = "Ringkasan Model Regresi Linear Berganda",
    align = "c"
  )
## PENGUJIAN ASUMSI KLASIK
# UJI LINEARITAS
data_diagnostik <- data.frame(
  Fitted = fitted(model_regresi),
  Residual = residuals(model_regresi)
)

ggplot(
  data_diagnostik,
  aes(x = Fitted, y = Residual)
) +
  geom_point(
    color = "#800000",
    alpha = 0.5
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#000080"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Plot Pemeriksaan Linearitas",
    x = "Nilai Prediksi",
    y = "Residual"
  ) +
  theme_minimal()
# UJI NORMALITAS RESIDUAL
uji_shapiro_regresi <- shapiro.test(
  residuals(model_regresi)
)

uji_shapiro_regresi

# Q Q PLOT RESIDUAL
ggplot(
  data_diagnostik,
  aes(sample = Residual)
) +
  stat_qq(
    color = "#800000",
    alpha = 0.6
  ) +
  stat_qq_line(
    color = "#000080",
    linewidth = 1
  ) +
  labs(
    title = "Normal Q-Q Plot Residual",
    x = "Kuantil Teoretis",
    y = "Kuantil Sampel"
  ) +
  theme_minimal()
## Uji multikolinearitas
hasil_vif <- car::vif(model_regresi)

hasil_vif
## VISUALISASI VIF
data_vif <- data.frame(
  Variabel = names(hasil_vif),
  VIF = as.numeric(hasil_vif)
)

ggplot(
  data_vif,
  aes(x = reorder(Variabel, VIF), y = VIF)
) +
  geom_col(
    fill = "#000080",
    color = "#800000"
  ) +
  geom_hline(
    yintercept = 10,
    color = "#800000",
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Plot Nilai Variance Inflation Factor",
    x = "Variabel Bebas",
    y = "Nilai VIF"
  ) +
  theme_minimal()
## UJI HETEROS KEDASTISITAS
uji_bp <- lmtest::bptest(model_regresi)

uji_bp
# VISUALISASI SCALE LOCATION
data_diagnostik$Scale_Location <- sqrt(
  abs(rstandard(model_regresi))
)

ggplot(
  data_diagnostik,
  aes(x = Fitted, y = Scale_Location)
) +
  geom_point(
    color = "#800000",
    alpha = 0.5
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#000080"
  ) +
  labs(
    title = "Scale-Location Plot",
    x = "Nilai Prediksi",
    y = "Akar Nilai Absolut Residual Terstandarisasi"
  ) +
  theme_minimal()
# Uji Durbin-Watson
uji_dw <- lmtest::dwtest(model_regresi)

uji_dw

## RINGKASAN ASUMSI KLASIK
hasil_asumsi <- data.frame(
  Pengujian = c(
    "Normalitas Residual",
    "Multikolinearitas",
    "Heteroskedastisitas",
    "Autokorelasi (Durbin-Watson)"
  ),
  Statistik = c(
    unname(uji_shapiro_regresi$statistic),
    NA,
    unname(uji_bp$statistic),
    unname(uji_dw$statistic)
  ),
  P_Value = c(
    uji_shapiro_regresi$p.value,
    NA,
    uji_bp$p.value,
    uji_dw$p.value
  ),
  Keputusan = c(
    ifelse(uji_shapiro_regresi$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0"),
    "Lihat nilai VIF",
    ifelse(uji_bp$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0"),
    ifelse(uji_dw$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0")
  )
)

knitr::kable(
  hasil_asumsi,
  caption = "Ringkasan Pengujian Asumsi Klasik",
  digits = 4,
  align = "c"
)
## FOREST PLOT KOEFISIEN REGRESI
data_forest <- broom::tidy(
  model_regresi,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = factor(
      term,
      levels = term[order(estimate)]
    )
  )

ggplot(
  data_forest,
  aes(x = estimate, y = term)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#800000"
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    width = 0.2,
    color = "#000080",
    linewidth = 0.8
  ) +
  geom_point(
    color = "#800000",
    size = 3
  ) +
  labs(
    title = "Forest Plot Koefisien Regresi Linear Berganda",
    x = "Estimasi Koefisien dan Interval Kepercayaan 95%",
    y = "Variabel Bebas"
  ) +
  theme_minimal()
# Mengatur tampilan menjadi 2 baris dan 2 kolom
par(mfrow = c(2, 2))

# Menampilkan empat plot diagnostik regresi
plot(model_regresi, which = 1:4)

# Mengembalikan tampilan seperti semula
par(mfrow = c(1, 1))

# =====================================================
# PERSIAPAN OBJEK DASHBOARD
# =====================================================

library(ggplot2)
library(purrr)
library(knitr)
library(corrplot)
library(GGally)
library(broom)
library(car)
library(lmtest)

# Memastikan nama objek data konsisten
data_awal <- Life_Expectancy_Data

# Memastikan Country hanya menampilkan 5 negara
data_country_top5 <- data_eda %>%
  count(Country, name = "Frekuensi") %>%
  slice_max(
    order_by = Frekuensi,
    n = 5,
    with_ties = FALSE
  )

# Menyiapkan tabel hasil uji asumsi klasik
hasil_asumsi <- data.frame(
  Pengujian = c(
    "Normalitas Residual",
    "Multikolinearitas",
    "Heteroskedastisitas",
    "Autokorelasi (Durbin-Watson)"
  ),
  Statistik = c(
    unname(uji_shapiro_regresi$statistic),
    NA,
    unname(uji_bp$statistic),
    unname(uji_dw$statistic)
  ),
  P_Value = c(
    uji_shapiro_regresi$p.value,
    NA,
    uji_bp$p.value,
    uji_dw$p.value
  ),
  Keputusan = c(
    ifelse(
      uji_shapiro_regresi$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    ),
    "Lihat nilai VIF",
    ifelse(
      uji_bp$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    ),
    ifelse(
      uji_dw$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    )
  )
)

# Warna sesuai syntax analisis sebelumnya
warna_regresi <- c(
  biru_dongker = "#000080",
  maroon = "#800000"
)

warna_multivariat <- c(
  merah = "red",
  hijau = "darkgreen"
)

# =========================================================
# R SHINY DASHBOARD
# LIFE EXPECTANCY DATA
# =========================================================

# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  titlePanel(
    "Interactive Statistical Explorer"
  ),
  
  p(
    "Dashboard Eksplorasi Data, Analisis Multivariat, dan Regresi Linear Berganda"
  ),
  
  tabsetPanel(
    
    # =====================================================
    # 1. RINGKASAN DATA
    # =====================================================
    
    tabPanel(
      "Ringkasan Data",
      
      br(),
      
      h4("Informasi Data"),
      verbatimTextOutput("info_data"),
      
      h4("Statistik Deskriptif Numerik"),
      DTOutput("tabel_numerik"),
      
      br(),
      
      h4("Statistik Deskriptif Kategorik - Status"),
      DTOutput("tabel_status"),
      
      br(),
      
      h4("Statistik Deskriptif Kategorik - Country"),
      DTOutput("tabel_country"),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_ringkasan")
      )
    ),
    
    # =====================================================
    # 2. UNIVARIAT
    # =====================================================
    
    tabPanel(
      "Univariat",
      
      br(),
      
      h4("Histogram Variabel Numerik"),
      plotOutput(
        "plot_histogram",
        height = "650px"
      ),
      
      br(),
      
      h4("Boxplot Variabel Numerik"),
      plotOutput(
        "plot_boxplot",
        height = "650px"
      ),
      
      br(),
      
      h4("Diagram Lingkaran Distribusi Status"),
      plotOutput(
        "plot_pie_status",
        height = "450px"
      ),
      
      br(),
      
      h4("5 Negara dengan Frekuensi Observasi Tertinggi"),
      plotOutput(
        "plot_country",
        height = "400px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_univariat")
      )
    ),
    
    # =====================================================
    # 3. BIVARIAT
    # =====================================================
    
    tabPanel(
      "Bivariat",
      
      br(),
      
      h4("Hasil Korelasi Pearson"),
      DTOutput("tabel_korelasi"),
      
      br(),
      
      h4("Scatter Plot Hubungan Schooling dan Adult Mortality"),
      plotOutput(
        "plot_scatter",
        height = "450px"
      ),
      
      br(),
      
      h4("Boxplot Variabel Numerik Berdasarkan Status"),
      plotOutput(
        "plot_boxplot_status",
        height = "650px"
      ),
      
      br(),
      
      h4("Correlation Plot Antarvariabel Numerik"),
      plotOutput(
        "plot_corrplot",
        height = "550px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_bivariat")
      )
    ),
    
    # =====================================================
    # 4. MULTIVARIAT
    # =====================================================
    
    tabPanel(
      "Multivariat",
      
      br(),
      
      h4("Vektor Rata-Rata"),
      DTOutput("tabel_mean"),
      
      br(),
      
      h4("Matriks Kovariansi Data Terstandarisasi"),
      DTOutput("tabel_kovariansi"),
      
      br(),
      
      h4("Scatter Plot Multivariat"),
      plotOutput(
        "plot_ggpairs",
        height = "750px"
      ),
      
      br(),
      
      h4("Deteksi Outlier dengan Jarak Mahalanobis"),
      plotOutput(
        "plot_mahalanobis",
        height = "450px"
      ),
      
      DTOutput("tabel_outlier"),
      
      br(),
      
      h4("Chi-Square Q-Q Plot"),
      plotOutput(
        "plot_qq",
        height = "450px"
      ),
      
      br(),
      
      h4("Uji Normalitas Shapiro-Wilk"),
      DTOutput("tabel_shapiro"),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_multivariat")
      )
    ),
    
    # =====================================================
    # 5. REGRESI LINEAR BERGANDA
    # =====================================================
    
    tabPanel(
      "Regresi Berganda",
      
      br(),
      
      h4("Hasil Estimasi Regresi"),
      DTOutput("tabel_estimasi"),
      
      br(),
      
      h4("Ringkasan Model"),
      DTOutput("tabel_model"),
      
      br(),
      
      h4("Pola Hubungan Variabel X dengan Life Expectancy"),
      plotOutput(
        "plot_regresi",
        height = "650px"
      ),
      
      br(),
      
      h4("Forest Plot Koefisien Regresi"),
      plotOutput(
        "plot_forest",
        height = "450px"
      ),
      
      br(),
      
      h4("Pengujian Asumsi Klasik"),
      DTOutput("tabel_asumsi"),
      
      br(),
      
      h4("Visualisasi Nilai VIF"),
      plotOutput(
        "plot_vif",
        height = "400px"
      ),
      
      DTOutput("tabel_vif"),
      
      br(),
      
      h4("Empat Plot Diagnostik Residual"),
      plotOutput(
        "plot_diagnostik",
        height = "650px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_regresi")
      )
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  # =====================================================
  # 1. RINGKASAN DATA
  # =====================================================
  
  output$info_data <- renderPrint({
    
    cat(
      "Jumlah observasi:", nrow(Life_Expectancy_Data), "\n",
      "Jumlah variabel:", ncol(Life_Expectancy_Data), "\n",
      "Jumlah negara:", nlevels(data_eda$Country), "\n"
    )
    
  })
  
  output$tabel_numerik <- renderDT({
    
    datatable(
      deskriptif_numerik,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_status <- renderDT({
    
    datatable(
      deskriptif_status,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_country <- renderDT({
    
    datatable(
      deskriptif_country,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$interpretasi_ringkasan <- renderText({
    
    paste0(
      "Data terdiri dari ",
      nrow(Life_Expectancy_Data),
      " observasi dan ",
      ncol(Life_Expectancy_Data),
      " variabel. Analisis menggunakan lima variabel numerik, ",
      "yaitu Adult Mortality, HIV/AIDS, BMI, GDP, dan Schooling. ",
      "Variabel kategorik yang dianalisis adalah Status dan Country. ",
      "Statistik deskriptif numerik memberikan gambaran ukuran pemusatan ",
      "dan penyebaran data, sedangkan statistik kategorik menunjukkan ",
      "jumlah serta persentase masing-masing kategori."
    )
    
  })
  
  
  # =====================================================
  # 2. UNIVARIAT
  # =====================================================
  
  output$plot_histogram <- renderPlot({
    
    ggplot(
      data_numerik_long,
      aes(x = Nilai)
    ) +
      geom_histogram(
        bins = 30,
        fill = "skyblue",
        color = "black",
        na.rm = TRUE
      ) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Histogram Distribusi Variabel Numerik",
        x = "Nilai",
        y = "Frekuensi"
      ) +
      theme_minimal()
    
  })
  
  output$plot_boxplot <- renderPlot({
    
    ggplot(
      data_numerik_long,
      aes(
        x = "",
        y = Nilai
      )
    ) +
      geom_boxplot(
        fill = "lightgreen",
        color = "black",
        na.rm = TRUE
      ) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Boxplot Variabel Numerik",
        x = "Variabel",
        y = "Nilai"
      ) +
      theme_minimal()
    
  })
  
  output$plot_status <- renderPlot({
    
    ggplot(
      data_eda,
      aes(
        x = Status,
        fill = Status
      )
    ) +
      geom_bar(color = "black") +
      labs(
        title = "Distribusi Frekuensi Status",
        x = "Status",
        y = "Frekuensi",
        fill = "Status"
      ) +
      theme_minimal()
    
  })
  
  # Diagram Lingkaran Distribusi Status
  
  output$plot_pie_status <- renderPlot({
    
    data_status_pie <- data_eda %>%
      count(Status, name = "Frekuensi") %>%
      mutate(
        Persentase = Frekuensi / sum(Frekuensi) * 100,
        Label = paste0(
          Status, "\n",
          round(Persentase, 1), "%"
        )
      )
    
    ggplot(
      data_status_pie,
      aes(x = "", y = Frekuensi, fill = Status)
    ) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = Label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5
      ) +
      labs(
        title = "Diagram Lingkaran Distribusi Status",
        fill = "Status"
      ) +
      theme_void()
    
  })
  output$interpretasi_univariat <- renderText({
    
    status_tertinggi <- data_eda %>%
      count(Status, name = "Frekuensi") %>%
      mutate(
        Persentase = Frekuensi / sum(Frekuensi) * 100
      ) %>%
      slice_max(
        order_by = Frekuensi,
        n = 1
      )
    
    paste0(
      "Histogram memperlihatkan distribusi masing-masing variabel numerik. ",
      "Boxplot digunakan untuk melihat median, penyebaran, dan kemungkinan ",
      "pencilan pada setiap variabel. Grafik batang dan diagram lingkaran ",
      "menunjukkan distribusi negara berdasarkan Status. Kategori Status ",
      "dengan frekuensi tertinggi adalah ",
      status_tertinggi$Status,
      " sebanyak ",
      status_tertinggi$Frekuensi,
      " observasi atau ",
      round(status_tertinggi$Persentase, 2),
      "%. Grafik Country menampilkan lima negara dengan frekuensi observasi tertinggi."
    )
    
  })
  
  # =====================================================
  # 3. BIVARIAT
  # =====================================================
  
  output$tabel_korelasi <- renderDT({
    
    datatable(
      hasil_korelasi,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$plot_scatter <- renderPlot({
    
    ggplot(
      data_eda,
      aes(
        x = Schooling,
        y = `Adult Mortality`
      )
    ) +
      geom_point(
        color = "steelblue",
        size = 2,
        alpha = 0.7,
        na.rm = TRUE
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red",
        na.rm = TRUE
      ) +
      labs(
        title = "Hubungan Schooling dengan Adult Mortality",
        x = "Schooling",
        y = "Adult Mortality"
      ) +
      theme_minimal()
    
  })
  
  output$plot_boxplot_status <- renderPlot({
    
    ggplot(
      data_boxplot,
      aes(
        x = Status,
        y = Nilai,
        fill = Status
      )
    ) +
      geom_boxplot(na.rm = TRUE) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Boxplot Variabel Numerik Berdasarkan Status",
        x = "Status Negara",
        y = "Nilai",
        fill = "Status"
      ) +
      theme_minimal()
    
  })
  
  output$plot_corrplot <- renderPlot({
    
    corrplot(
      matriks_korelasi,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.7,
      col = colorRampPalette(
        c("red", "white", "darkgreen")
      )(200),
      title = "Correlation Plot Antarvariabel Numerik",
      mar = c(0, 0, 2, 0)
    )
    
  })
  
  output$interpretasi_bivariat <- renderText({
    
    korelasi_tertinggi <- hasil_korelasi %>%
      filter(!is.na(Korelasi_Pearson)) %>%
      slice_max(
        order_by = abs(Korelasi_Pearson),
        n = 1,
        with_ties = FALSE
      )
    
    paste0(
      "Berdasarkan hasil korelasi Pearson, pasangan variabel dengan ",
      "korelasi paling kuat secara absolut adalah ",
      korelasi_tertinggi$Variabel_1,
      " dan ",
      korelasi_tertinggi$Variabel_2,
      " dengan koefisien korelasi sebesar ",
      korelasi_tertinggi$Korelasi_Pearson,
      ". Scatter plot Schooling dan Adult Mortality menunjukkan ",
      "pola hubungan kedua variabel, sedangkan boxplot berdasarkan ",
      "Status digunakan untuk membandingkan distribusi variabel numerik ",
      "antara negara berkembang dan negara maju."
    )
    
  })
  
  
  # =====================================================
  # 4. MULTIVARIAT
  # =====================================================
  
  output$tabel_mean <- renderDT({
    
    datatable(
      data.frame(
        Variabel = names(vektor_rata_rata),
        Rata_Rata = as.numeric(vektor_rata_rata)
      ),
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_kovariansi <- renderDT({
    
    datatable(
      as.data.frame(matriks_kovariansi_standar),
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = TRUE
    )
    
  })
  
  output$plot_ggpairs <- renderPlot({
    
    print(
      GGally::ggpairs(
        data_multivariat,
        upper = list(
          continuous = GGally::wrap(
            "cor",
            colour = "darkgreen",
            size = 3
          )
        ),
        lower = list(
          continuous = GGally::wrap(
            "points",
            colour = "red",
            alpha = 0.4,
            size = 0.8
          )
        ),
        diag = list(
          continuous = GGally::wrap(
            "densityDiag",
            fill = "darkgreen",
            colour = "red",
            alpha = 0.5
          )
        )
      ) +
        ggtitle(
          "Scatter Plot Multivariat Antarvariabel Numerik"
        )
    )
    
  })
  
  output$plot_mahalanobis <- renderPlot({
    
    ggplot(
      hasil_outlier,
      aes(
        x = Observasi,
        y = Jarak_Mahalanobis,
        color = Status
      )
    ) +
      geom_point(size = 2) +
      geom_hline(
        yintercept = batas_mahalanobis,
        color = "darkgreen",
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_color_manual(
        values = c(
          "Outlier" = "red",
          "Tidak Outlier" = "darkgreen"
        )
      ) +
      labs(
        title = "Deteksi Outlier Multivariat dengan Jarak Mahalanobis",
        x = "Observasi",
        y = "Jarak Mahalanobis Kuadrat",
        color = "Status"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_outlier <- renderDT({
    
    datatable(
      hasil_outlier,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$plot_qq <- renderPlot({
    
    ggplot(
      data_qq,
      aes(
        x = Chi_Square,
        y = Mahalanobis
      )
    ) +
      geom_point(
        color = "red",
        alpha = 0.6
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "darkgreen",
        linewidth = 1
      ) +
      labs(
        title = "Chi-Square Q-Q Plot Normalitas Multivariat",
        x = "Kuantil Teoretis Chi-Square",
        y = "Jarak Mahalanobis Kuadrat"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_shapiro <- renderDT({
    
    datatable(
      hasil_shapiro,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$interpretasi_multivariat <- renderText({
    
    jumlah_outlier <- sum(
      hasil_outlier$Status == "Outlier"
    )
    
    paste0(
      "Analisis multivariat menggunakan lima variabel numerik. ",
      "Sebelum menghitung jarak Mahalanobis, data distandardisasi ",
      "agar perbedaan skala antarvariabel tidak mendominasi perhitungan. ",
      "Dengan batas Chi-Square sebesar ",
      round(batas_mahalanobis, 4),
      ", ditemukan ",
      jumlah_outlier,
      " observasi yang terindikasi sebagai outlier. ",
      "Chi-Square Q-Q Plot digunakan untuk melihat kesesuaian jarak ",
      "Mahalanobis dengan kuantil teoretis Chi-Square. ",
      "Uji Shapiro-Wilk digunakan untuk memeriksa normalitas setiap ",
      "variabel secara univariat."
    )
    
  })
  
  
  # =====================================================
  # 5. REGRESI LINEAR BERGANDA
  # =====================================================
  
  output$tabel_estimasi <- renderDT({
    
    datatable(
      hasil_estimasi,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c(
          "estimate",
          "std.error",
          "statistic",
          "p.value",
          "conf.low",
          "conf.high"
        ),
        digits = 4
      )
    
  })
  
  # =====================================================
  # HASIL MODEL REGRESI LINEAR BERGANDA
  # =====================================================
  
  # Menampilkan hasil summary regresi seperti R Console
  output$hasil_summary_regresi <- renderPrint({
    
    summary(model_regresi)
    
  })
  
  
  # =====================================================
  # HASIL UJI t (PARSIAL)
  # =====================================================
  
  output$tabel_uji_t <- renderDT({
    
    hasil_estimasi %>%
      filter(term != "(Intercept)") %>%
      select(
        Variabel = term,
        Estimate = estimate,
        Std_Error = std.error,
        t_value = statistic,
        P_Value = p.value
      ) %>%
      mutate(
        Keputusan = ifelse(
          P_Value < 0.05,
          "Signifikan",
          "Tidak Signifikan"
        )
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatRound(
        columns = c(
          "Estimate",
          "Std_Error",
          "t_value",
          "P_Value"
        ),
        digits = 4
      )
    
  })
  
  
  
  # =====================================================
  # RINGKASAN MODEL DAN HASIL UJI F (SIMULTAN)
  # =====================================================
  
  output$tabel_model <- renderDT({
    
    data.frame(
      Keterangan = c(
        "Residual Standard Error",
        "Degrees of Freedom",
        "Multiple R-squared",
        "Adjusted R-squared",
        "F-statistic",
        "P-value Uji F"
      ),
      Nilai = c(
        hasil_model$sigma,
        hasil_model$df.residual,
        hasil_model$r.squared,
        hasil_model$adj.r.squared,
        hasil_model$statistic,
        hasil_model$p.value
      )
    ) %>%
      datatable(
        options = list(
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatRound(
        columns = "Nilai",
        digits = 4
      )
    
  })
  
  
  output$plot_regresi <- renderPlot({
    
    ggplot(
      data_scatter,
      aes(
        x = Nilai_X,
        y = `Life expectancy`
      )
    ) +
      geom_point(
        color = "#800000",
        alpha = 0.4,
        size = 1.5
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "#000080"
      ) +
      facet_wrap(
        ~ Variabel_X,
        scales = "free_x",
        ncol = 2
      ) +
      labs(
        title = "Pola Hubungan Variabel X dengan Life Expectancy",
        x = "Variabel Bebas (X)",
        y = "Life Expectancy (Y)"
      ) +
      theme_minimal()
    
  })
  
  output$plot_forest <- renderPlot({
    
    ggplot(
      data_forest,
      aes(
        x = estimate,
        y = term
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "#800000"
      ) +
      geom_errorbar(
        aes(
          xmin = conf.low,
          xmax = conf.high
        ),
        width = 0.2,
        color = "#000080",
        linewidth = 0.8
      ) +
      geom_point(
        color = "#800000",
        size = 3
      ) +
      labs(
        title = "Forest Plot Koefisien Regresi Linear Berganda",
        x = "Estimasi Koefisien dan Interval Kepercayaan 95%",
        y = "Variabel Bebas"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_asumsi <- renderDT({
    
    datatable(
      hasil_asumsi,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Statistik", "P_Value"),
        digits = 4
      )
    
  })
  
  output$plot_vif <- renderPlot({
    
    ggplot(
      data_vif,
      aes(
        x = reorder(Variabel, VIF),
        y = VIF
      )
    ) +
      geom_col(
        fill = "#000080",
        color = "#800000"
      ) +
      geom_hline(
        yintercept = 10,
        color = "#800000",
        linetype = "dashed"
      ) +
      coord_flip() +
      labs(
        title = "Plot Nilai Variance Inflation Factor",
        x = "Variabel Bebas",
        y = "Nilai VIF"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_vif <- renderDT({
    
    datatable(
      data_vif,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = "VIF",
        digits = 4
      )
    
  })
  
  output$plot_diagnostik <- renderPlot({
    
    par(
      mfrow = c(2, 2)
    )
    
    plot(
      model_regresi,
      which = 1:4,
      col = "#800000"
    )
    
    par(
      mfrow = c(1, 1)
    )
    
  })
  
  output$interpretasi_regresi <- renderText({
    
    r2 <- hasil_model$r.squared
    adj_r2 <- hasil_model$adj.r.squared
    p_model <- hasil_model$p.value
    
    keputusan_model <- if (p_model < 0.05) {
      "Model regresi signifikan secara simultan pada taraf 5%."
    } else {
      "Model regresi tidak signifikan secara simultan pada taraf 5%."
    }
    
    keputusan_normalitas <- if (
      uji_shapiro_regresi$p.value > 0.05
    ) {
      "Residual tidak menunjukkan penyimpangan signifikan dari normalitas."
    } else {
      "Residual menunjukkan penyimpangan dari normalitas berdasarkan Shapiro-Wilk."
    }
    
    keputusan_bp <- if (uji_bp$p.value > 0.05) {
      "Tidak terdapat bukti heteroskedastisitas berdasarkan uji Breusch-Pagan."
    } else {
      "Terdapat indikasi heteroskedastisitas berdasarkan uji Breusch-Pagan."
    }
    
    keputusan_vif <- if (all(hasil_vif < 10)) {
      "Seluruh nilai VIF berada di bawah 10 sehingga tidak terdapat indikasi multikolinearitas kuat berdasarkan batas tersebut."
    } else {
      "Terdapat variabel dengan nilai VIF 10 atau lebih sehingga perlu diperiksa lebih lanjut."
    }
    
    paste0(
      "Model regresi menggunakan Life Expectancy sebagai variabel ",
      "dependen dan Adult Mortality, HIV/AIDS, BMI, GDP, serta Schooling ",
      "sebagai variabel independen. Nilai R-squared sebesar ",
      round(r2, 4),
      " menunjukkan bahwa ",
      round(r2 * 100, 2),
      "% variasi Life Expectancy dapat dijelaskan oleh variabel bebas ",
      "dalam model. Adjusted R-squared sebesar ",
      round(adj_r2, 4),
      ". P-value uji F sebesar ",
      signif(p_model, 4),
      ". ",
      keputusan_model,
      " ",
      keputusan_normalitas,
      " ",
      keputusan_bp,
      " ",
      keputusan_vif,
      " Forest plot menampilkan estimasi koefisien beserta interval ",
      "kepercayaan 95%, sedangkan empat plot diagnostik digunakan untuk ",
      "memeriksa pola residual, normalitas, kesamaan varians, dan ",
      "pengamatan berpengaruh."
    )
    
  })
  
}


# jalankan dashboard
# =====================================================

shinyApp(
  ui = ui,
  server = server
)

