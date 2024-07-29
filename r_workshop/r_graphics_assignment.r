###################################################################### 
# 2024年度課題用Rコード
# 2024年7月2日
# 宇都宮　譲（長崎大学経済学部）
# 
###################################################################### 
# 
# ----- read.library -----
# ライブラリをロードする
library(tidyverse)
library(khroma)
# 
# ----- data.wrangling -----
# データを読み込む
ssdse <- 
  readxl::read_excel(
    # ファイル名
    "kenbetsu_suii_ssdse.xlsx",
    # 1行目は読み込まない。
    # 使わないから。
    skip = 1
  ) %>% 
  # 縦型データに変換
  tidyr::pivot_longer(
    # 並べ替えない列を指定する。
    cols = -c(1:3),
    names_to = "variables",
    values_to = "number"
  ) %>%
  # 列名を与える
  # 列名はアルファベットで書いたほうがいろいろと楽ができる。
  data.table::setnames(
    c(
      "year",
      "region_code",
      "prefecture",
      "variables",
      "number"
    )
  ) %>% 
  # 地域コードは不要だから除却
  dplyr::select(-region_code) %>%
  # 年月日データとして取り扱ったほうが楽だから年月日に変換。
  # 日付はとりあえず。
  dplyr::mutate(
    year = lubridate::ymd(
      paste0(year, "/01/01")
      )
  ) %>% 
  # character型をfactor型に変換。
  # 因子型にしておいたほうがなにかと便利。今回はさほど変わらないけど。
  dplyr::mutate(across(where(is_character), as_factor))
# 
# 必要なデータをとりだす。
# 日本語を使いたくないけれど、仕方なく使う。それでも十分だから。
ssdse_population <- 
  ssdse %>% 
  dplyr::filter(
    variables == "総人口"
    )
# 
# ----- plot.line -----
# 作図
ssdse_population_line <- 
  # データを与える
  ssdse_population %>% 
  ggplot2::ggplot(
    aes(
      x = year,
      y = number,
      # 色は都道府県別に変える。
      color = prefecture
    )
  ) +
  geom_point() +
  geom_line() +
  # カラーパレットはおなじみkhroma::smoothrainbow
  # 離散型因子がたくさんあるときはとっても便利
  scale_color_smoothrainbow(discrete = TRUE) +
  # おなじみfacet_wrap()
  # y軸を動かしてみましょう。
  facet_wrap(~ prefecture, scales = "free") +
  # themeもおなじみ。
  theme_classic() +
  # 各種ラベルを設定する。
  labs(
    # x軸
    x = "年次（2010-2021年）",
    # y軸
    y = "総人口（単位：万人）",
    # メインタイトル
    title = "都道府県別総人口年次推移",
    # サブタイトル
    subtitle = "独立行政法人統計センター（https://www.nstac.go.jp/use/literacy/ssdse/?doing_wp_cron=1719879749.2496199607849121093750#SSDSE-B）より作成。",
    # キャプション。
    # ここに名前を書けるかどうが分かれ道。
    caption = "作成者氏名を挿入せよ。例）本職が作成するならば、宇都宮譲と書く。"
  ) +
  # その他設定
  theme(
    # 凡例は不要。
    legend.position = "none",
    # 都道府県名背景不要
    strip.background = element_blank()
  )
# 保存
ggsave(
  "ssdse_population_line.pdf",
  plot = ssdse_population_line,
  # プロットエリア高さ
  height = 300,
  # プロットエリア幅
  width = 300,
  # プロットエリア寸法単位
  units = "mm",
  # 日本語を使えるようにするデバイスおまじない
  device = cairo_pdf
)
# おしまい


