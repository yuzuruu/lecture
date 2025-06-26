####################################################################
# 2023年労務管理論関連Rコード
# 2023年5月25日
# Yuzuru Utsunomiya, Ph. D.
# 
# ---- read.library.hrm2023 ----
library(tidyverse)
library(khroma)
library(estatapi)
library(khroma)
library(viridis)
library(progressr)
appID <- source("appID.r")$value
library(furrr)
# plan(multisession, workers = 8)
library(sf)
library(spdep)
library(dplyr)
library(stringdist)
library(ggrepel)
library(showtext)
font_add("jpfont", regular = "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc")
# 
# # 
# 
# # ----- systematic.soldering -----
# n <- 3000
# a <- 10 #切片
# b <- 20 #回帰係数
# x <- rnorm(n=n) #変数x（回帰モデルへの入力データ）
# e <- rnorm(n=n, sd = 5) #回帰モデルの残差（正規分布と仮定しているので）
# y <- b*x + a + e #線形回帰モデルに基づいたデータを作成
# model <- lm(y~x)
# sample_data <- 
#   dplyr::tibble(
#     x = x,
#     y = y
#     ) |> 
#   dplyr::mutate(
#     color_group = dplyr::if_else(
#       model$residuals < 0, "TRUE", "FALSE"
#     )
#   )
# sample_plot <- 
#   sample_data |> 
#   ggplot2::ggplot(
#     aes(
#       x = x, y = y, color = color_group
#     )
#   ) +
#   geom_point() + 
#   geom_smooth(method = "lm", color = "black") +
#   scale_color_manual(values = c("#F5C710","black")) +
#   labs(x = "作業量", y = "賃金") +
#   xlim(0,1.5) +
#   ylim(0,40) +
#   theme_classic() +
#   theme(
#     legend.position = "none"
#   )
# 
# 
# # ---- immigrant.transition ----
# df_immigrant <- 
#   readxl::read_excel(
#     "hrm_2023.xlsx",
#     sheet = "immigrant"
#   ) %>% 
#   data.table::setnames(
#     c("country","1860s","1870s","1880s","1890s","1900s")
#   ) %>% 
#   tidyr::pivot_longer(
#     col = -country,
#     names_to = "generation",
#     values_to = "immigrant"
#   ) %>% 
#   dplyr::mutate(
#     country = factor(country),
#     generation = factor(generation, levels = c("1860s","1870s","1880s","1890s","1900s"))
#   )
# # 
# line_immigrant <- 
#   df_immigrant %>% 
#   ggplot2::ggplot(
#     aes(
#       x = generation, 
#       y = immigrant, 
#       color = country, 
#       group = country
#     )
#   ) +
#   geom_line() + 
#   geom_point() +
#   scale_color_smoothrainbow(discrete = TRUE) +
#   labs(
#     x = "Generation",
#     y = "Immigrant (Unit: Persons)",
#     caption = "https://www.jetro.go.jp/ext_images/jfile/report/05000661/05000661_001_BUP_0.pdf"
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "bottom"
#   )
# # 
# ----- labourforce -----
#
lfs_japan <-
  estat_getStatsData(
    appId = appID,
    statsDataId = "0002060047"
  )
#
lfs_japan_selected <-
  lfs_japan %>%
  dplyr::select(
    c(4,6,8,10,14, 16)
  ) %>%
  data.table::setnames(
    c("industry","gender","status","ageclass","year", "population")
  ) %>%
  dplyr::mutate(
    industry = factor(industry),
    gender = factor(gender),
    status = factor(status),
    ageclass = factor(ageclass),
    year = lubridate::ymd(paste0(stringr::str_sub(year, start = 1, end = 4), "/06/01"))
  ) %>%
  dplyr::filter(ageclass %in% c("15～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳","60～64歳", "65～69歳", "70～74歳", "75～79歳", "80～84歳", "85歳以上")) %>%
  dplyr::mutate(
    ageclass = factor(ageclass, levels = c("15～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳","60～64歳", "65～69歳", "70～74歳", "75～79歳", "80～84歳", "85歳以上"))
  )
#
whole_lfs <-
  lfs_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "人数（単位：万人）",
    color = "年齢階級"
  ) +
  facet_wrap(~ status + gender, scales = "free_y") +
  guides(color=guide_legend(nrow=2,byrow=FALSE)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
# 折れ線グラフ保存
ggsave(
  "whole_lfs.pdf",
  plot = whole_lfs,
  width = 300,
  height = 300,
  units = "mm",
  device = cairo_pdf
)
#
lf  <-
  lfs_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業" & status == "労働力人口") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "労働力人口（単位：万人）",
    color = "年齢階級"
  ) +
  guides(color=guide_legend(nrow=3,byrow=FALSE)) +
  facet_wrap(~ gender, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
#
engaged  <-
  lfs_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業" & status == "就業者") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "就業者（単位：万人）",
    color = "年齢階級"
  ) +
  guides(color=guide_legend(nrow=3,byrow=FALSE)) +
  facet_wrap(~ gender, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
#
unemployed <-
  lfs_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業" & status == "完全失業者") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "完全失業者（単位：万人）",
    color = "年齢階級"
  ) +
  guides(color=guide_legend(nrow=3,byrow=FALSE)) +
  facet_wrap(~ gender, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
#
notinlabourforce  <-
  lfs_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業" & status == "非労働力人口") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "就業者（単位：万人）",
    color = "年齢階級"
  ) +
  guides(color=guide_legend(nrow=3,byrow=FALSE)) +
  facet_wrap(~ gender, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
# 労働力率
lpr <-
  lfs_japan_selected %>%
  drop_na(population) %>%
  dplyr::filter(status %in% c("15歳以上人口", "労働力人口")) %>%
  tidyr::pivot_wider(
    names_from = status,
    values_from = population
  ) %>%
  dplyr::mutate(
    lpr = 100*c(労働力人口/`15歳以上人口`),
    year = lubridate::year(year)
  )
# M字カーブ
line_lpr <-
  lpr %>%
  dplyr::filter(gender != "総数") %>%
  ggplot2::ggplot(
    aes(x = ageclass, y = lpr, group = factor(year))
  ) +
  geom_line(aes(color = factor(year))
  ) +
  labs(x = "年齢階級", y = "労働力率（1953-2021年。単位：%）") +
  scale_color_smoothrainbow(discrete = TRUE) +
  scale_x_discrete(breaks = levels(lpr$ageclass)[c(2,6,10,14)]) +
  facet_wrap(~ gender) +
  theme_classic()+
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )
# M字カーブ保存
ggsave(
  "lpr_line.pdf",
  plot = line_lpr,
  width = 300,
  height = 300,
  units = "mm",
  device = cairo_pdf
)
# 就業者と雇用者
employment_japan <-
  estat_getStatsData(
    appId = appID,
    statsDataId = "0002060048"
  )
employment_japan_selected <-
  employment_japan %>%
  dplyr::select(
    c(4,6,8,10,14, 16)
  ) %>%
  data.table::setnames(
    c("industry","gender","status","ageclass","year", "population")
  ) %>%
  dplyr::mutate(
    industry = factor(industry),
    gender = factor(gender),
    status = factor(status),
    ageclass = factor(ageclass),
    year = lubridate::ymd(paste0(stringr::str_sub(year, start = 1, end = 4), "/06/01"))
  ) %>%
  dplyr::filter(ageclass %in% c("15～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳","60～64歳", "65～69歳", "70～74歳", "75～79歳", "80～84歳", "85歳以上")) %>%
  dplyr::mutate(
    ageclass = factor(ageclass, levels = c("15～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳","60～64歳", "65～69歳", "70～74歳", "75～79歳", "80～84歳", "85歳以上"))
  )
#
employment_lfs <-
  employment_japan_selected %>%
  dplyr::filter(gender != "総数" & industry == "全産業") %>%
  ggplot2::ggplot(
    aes(
      x = year,
      y = population,
      color = ageclass
    )
  ) +
  geom_line() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "年次（1968-2022年）",
    y = "人数（単位：万人）",
    color = "年齢階級"
  ) +
  facet_wrap(~ status + gender, scales = "free_y") +
  guides(color=guide_legend(nrow=2,byrow=FALSE)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
# 雇用者推移折れ線グラフ保存
ggsave(
  "employment_lfs.pdf",
  plot = employment_lfs,
  width = 300,
  height = 300,
  units = "mm",
  device = cairo_pdf
)
# 就業者に雇用者が占める割合年次推移
employed_ratio <-
  employment_japan_selected %>%
  dplyr::filter(status %in% c("総数", "雇用者")) %>%
  tidyr::pivot_wider(
    names_from = status,
    values_from = population
  ) %>%
  drop_na() %>%
  dplyr::mutate(
    employment_ratio = 100*c(雇用者/総数),
    year = lubridate::year(year)
  )
#
line_employment <-
  employed_ratio %>%
  dplyr::filter(gender != "総数" & industry == "全産業") %>%
  ggplot2::ggplot(
    aes(x = year, y = employment_ratio, group = ageclass)
  ) +
  geom_line(aes(color = ageclass)
  ) +
  labs(x = "年齢階級", y = "労働力率（1953-2021年。単位：%）") +
  scale_color_smoothrainbow(discrete = TRUE) +
  scale_x_discrete(breaks = levels(lpr$ageclass)[c(2,6,10,14)]) +
  facet_wrap(~ gender) +
  theme_classic()+
  theme(
    # legend.position = "none",
    strip.background = element_blank()
  )
#
line_employment_aggregate <-
  employment_japan_selected %>%
  dplyr::filter(status %in% c("総数", "雇用者")) %>%
  tidyr::pivot_wider(
    names_from = status,
    values_from = population
  ) %>%
  drop_na() %>%
  dplyr::group_by(factor(year)) %>%
  summarise(
    n_aggregate = sum(総数),
    n_employed = sum(雇用者)
  ) %>%
  dplyr::mutate(
    employment_aggregate_ratio = 100*(n_employed/n_aggregate)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    year = lubridate::ymd(`factor(year)` )
  ) %>%
  ggplot2::ggplot(
    aes(x = year, y = employment_aggregate_ratio)
  ) +
  geom_line() +
  labs(x = "年齢階級", y = "雇用者比（1968-2021年。単位：%）") +
  theme_classic()

# # ----- working.hours -----
# # 総労働時間
# hours_total <- 
#   estat_getStatsData(
#     appId = appID,
#     statsDataId = c("0003138221")
#   ) %>% 
#   dplyr::mutate(division = c("総実労働時間"))
# # 所定内労働時間
# hours_scheduled <- 
#   estat_getStatsData(
#     appId = appID,
#     statsDataId = c("0003138255")
#   ) %>% 
#   dplyr::mutate(division = c("所定内労働時間"))
# # 所定外労働時間
# hours_extra <- 
#   estat_getStatsData(
#     appId = appID,
#     statsDataId = c("0003138105")
#   ) %>% 
#   dplyr::mutate(division = c("所定外労働時間"))
# # 上記データを統合
# hours_combined <- 
#   hours_scheduled %>% 
#   dplyr::bind_rows(hours_extra) %>% 
#   dplyr::bind_rows(hours_total) %>% 
#   dplyr::select("調査月", "産業分類(200711改定)", "事業所規模", "就業形態", "調査年", "value", "division") %>% 
#   data.table::setnames(c("month", "industry", "size", "type", "year", "value", "division")) %>% 
#   dplyr::filter(
#     month %in% c("10月", "11月", "12月", "1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月")
#   ) %>% 
#   dplyr::mutate(
#     year_month_date = lubridate::ymd(paste0(stringr::str_sub(year, start = 1, end = 4), "/", stringr::str_replace_all(month, "月",""),"/01")),
#     industry = stringr::str_remove_all(industry, "(.+?_)")
#   ) %>% 
#   dplyr::select(industry, size, type, division, year_month_date, value) %>% 
#   dplyr::mutate(across(is.character,  factor)) %>% 
#   na.omit() %>% 
#   dplyr::filter(type != "就業形態計") 
# write_rds(hours_combined, "./hrm/hours_combined.rds")
# # 弾力的労働時間採用状況データ
# wh_system <- 
#   estatapi::estat_getStatsData(
#     appId = appID,
#     statsDataId = "0003297623"
#   ) %>% 
#   dplyr::select(-表章項目, -tab_code, -cat01_code, -cat02_code, -time_code, -unit, -annotation) %>% 
#   data.table::setnames(c("industry","status","year","number")) %>% 
#   dplyr::mutate(
#     year = factor(stringr::str_sub(year, start = 1, end = 4), levels = c("2016","2018","2020","2022")),
#     status = stringr::str_remove_all(status, "(.+?_)")
#   ) %>% 
#   dplyr::mutate(across(is.character,  factor)) %>% 
#   dplyr::filter(status != "集計社数", industry != "調査産業計") %>% 
#   droplevels()
# # データを保存
# write_rds(wh_system, "./hrm/wh_system.rds")
# # 
# # ----- working.hours.plot -----
# # 作図
# # hours_combined <- readr::read_rds("./hrm/hours_combined.rds")
# hours_combined <- readRDS("hours_combined.rds")
# line_hours_combined <- 
#   hours_combined %>% 
#   group_by(industry, size, division) %>% 
#   nest() %>% 
#   dplyr::mutate(
#     figure = purrr::map(
#       data, 
#       ~ 
#         ggplot2::ggplot(
#           data = .,
#           aes(
#             x = year_month_date,
#             y = value,
#             color = type
#           )
#         ) +
#         geom_line() +
#         scale_color_okabeito() +
#         labs(
#           x = "年次（1993-2015年）",
#           y = "労働時間（指数。2010年平均＝100）",
#           color = "就業形態",
#           title = paste(industry, size, division, sep = " ")
#         ) +
#         theme_classic() +
#         theme(
#           legend.position = "bottom",
#           strip.background = element_blank(),
#           text = element_text("Noto Sans Mono CJK JP")
#         )
#     )
#   )
# # # 保存
# # 一度保存したら十分だから普段はコメントアウト。枚数多くて時間かかるし。
# # Cairo::Cairo(
# #   type = "pdf",
# #   file = "line_hours_combined.pdf",
# #   width = 200, 
# #   height = 200,
# #   units = "mm"
# # )
# # line_hours_combined$figure
# # dev.off()
# 
# # ----- wh.system.plot -----
# # 弾力的労働時間棒グラフを描くよ
# # データ読み込み
# wh_system <- readRDS("wh_system.rds")
# # 作図
# mosaic_wh_system <- 
#   wh_system %>% 
#   dplyr::mutate(
#     number = ifelse(is.na(number), 0, number)
#   ) %>% 
#   ggplot2::ggplot(
#     data = .,
#     aes(
#       x = year,
#       y = number,
#       fill = status
#     )
#   ) +
#   geom_col(position = "fill") +
#   scale_fill_okabeito() +
#   labs(x = "年次", y = "調査社数比", fill = "労働時間制度") +
#   guides(fill=guide_legend(ncol=3))+
#   theme_classic() +
#   theme(legend.position = "bottom") 
# 
# overwork <- 
#   estat_getStatsData(
#     appId = appID,
#     statsDataId = "0003138108")
# levels(factor(overwork$調査年月))
# readr::write_rds(overwork, "overwork.rds")
# 
# # 年功賃金用作図コード
# # データ読み込み
# seniority <- 
#   estat_getStatsData(
#     appId = appID,
#     statsDataId = "0003425894"
#     )
# seniority
# readr::write_excel_csv(seniority,"seniority_0003425894.csv")
# # フィルタ
# seniority <- 
#   readr::read_csv("seniority_0003425894.csv") %>% 
#   dplyr::select(-tab_code, -cat01_code, -cat02_code, -cat03_code, -cat04_code, -cat05_code, -cat06_code, -cat07_code, -time_code, -unit,	-annotation) %>% 
#   data.table::setnames(c("type", "size", "length_service", "industry", "gender", "school", "age_class", "public_private", "year", "amount")) %>% 
#   dplyr::mutate(
#     dplyr::across(
#       where(is.character), as.factor
#     )
#   ) %>% 
#   dplyr::filter(
#     type != "労働者数" 
#     & size != "企業規模計（10人以上）"
#     & length_service != "勤続年数計"
#     & industry == "Ｔ１ 産業計"
#     & gender != "男女計"
#     & school != "学歴計"
#     & school != "不明"
#     & age_class != "年齢計" 
#     & public_private == "民営＋公営"
#     ) %>% 
#   droplevels() %>% 
#   na.omit() %>% 
#   dplyr::mutate(
#     length_service = factor(length_service, levels = c("0年", "1～2年", "3～4年", "5～9年", "10～14年", "15～19年", "20～24年", "25～29年", "30年以上")),
#     age_class = factor(age_class, levels = c("～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳", "60～64歳", "65～69歳", "70歳～"))
#   )
# # 作図
# seniority_line <- 
#   seniority %>% 
#   ggplot2::ggplot(
#     aes(
#       x = length_service,
#       y = amount,
#       color = age_class,
#       group = age_class
#     )
#   )  +
#   geom_line() +
#   geom_point() +
#   scale_color_smoothrainbow(discrete = TRUE) +
#   labs(x = "勤続年数", y = "賃金支給額（単位：1,000円）", title = "賃金区分・企業規模・性・学歴・勤続年数別賃金推移（産業計） ", subtitle = "2021年賃金構造基本統計調査より宇都宮作成。") + 
#   facet_wrap(~ type + size + gender + school, scales = "free_y") +
#   theme_classic() +
#   theme(
#     legend.position = "bottom",
#     strip.background = element_blank()
#   ) +
#   guides(color=guide_legend(nrow=1))
# # 保存
# ggsave(
#   "seniority_line.pdf",
#   plot = seniority_line,
#   height = 1200,
#   width = 1200,
#   units = "mm",
#   device = cairo_pdf
# )
# 
# END

# ----- n.of.enterprise -----
# # 経済センサスをつかって、企業数をプロットしたよ♪
# StatAPI用IDを読み込む
# appId <- source("../yuzurulab/appId.r")
# # 
# # 総務省estatからAPIを用いてデータ読み込む
# # estatapiパッケージを使う。
# # install.packages()を使ってインストールする。1回でいい。
# # 使うたびにlibrary()を使って読み込む。
# # 詳細は
# # https://yutannihilation.github.io/estatapi/　を参照。
# # 
# # まずデータリストをつくる
# enterprize_data_list <-
#   estatapi::estat_getStatsList(
#     # APIのIDを読み込む
#     # appId$valueは、
#     # appIdというオブジェクトのvalueという変数
#     # を意味する。
#     # $の使い方は、Rでデータを扱うにはとっても重要。なれておくように。
#     appId = appId$value,
#     searchWord = "企業数"
#   )
# readr::write_excel_csv(enterprize_data_list, "enterprize_data_list.csv")
# 
# # 上で見つけた統計を読み込む。
# # 上はデータリストだけを取得してるから、データは別途読み込む必要がある。
# 
# readr::write_rds(enterprize_prefecture, "enterprize_prefecture.rds")
# enterprize_prefecture <-
#   estat_getStatsData(
#     appId = appId$value,
#     statsDataId = "0004006304"
#     ) %>%
#   dplyr::select(都道府県, time_code, value) %>%
#   data.table::setnames(c("prefecture","year","tfr")) %>%
#   dplyr::mutate(
#     year = lubridate::ymd(paste0(stringr::str_sub(year, start = 1, end = 4),"/06/01")),
#     prefecture = factor(prefecture)
#   )
# # save
# readr::write_excel_csv(enterprize_prefecture, "enterprize_prefecture.csv")
# 
# # 何度も読み込むと面倒だから、上データを保存する。
# # readr::write_rds(tfr_prefecture,"tfr_prefecture.rds")
# # ということで、データは読み込んで使う。
# enterprise_prefecture <- 
#   readr::read_csv("enterprize_prefecture.csv")
# list_prefecture <- 
#   sf::read_sf("./JPN_adm/JPN_adm1.shp") %>% 
#   sf::st_drop_geometry() %>% 
#   dplyr::select(NL_NAME_1) %>% 
#   data.table::setnames(c("prefecture")) %>% 
#   dplyr::mutate(across(where(is.character), factor))
# 
# enterprise_prefecture_2021 <- 
#   enterprise_prefecture %>% 
#   dplyr::select(-tab_code, -cat01_code, -cat02_code, -cat03_code, -time_code, -area_code, -annotation, -時間軸) %>% 
#   dplyr::filter(表章項目 %in% c("企業数", "従業者数_女", "従業者数_男")) %>% 
#   dplyr::filter(地域 %in% list_prefecture$prefecture) %>% 
#   dplyr::mutate(across(where(is.character), factor)) %>% 
#   data.table::setnames(c("attribute", "industry", "size", "capital", "prefecture", "unit", "value")) %>% 
#   dplyr::filter(!str_detect(capital, "再掲|総数")) %>% 
#   dplyr::filter(!str_detect(size, "再掲|総数")) 
# save
# readr::write_excel_csv(enterprise_prefecture_2021, "enterprise_prefecture_2021.csv")
# 
# ----- randd.and.wage.japan -----
# Transition of R&D expenditure and wage by country
# read data
randd_japan <- 
  readxl::read_excel(
    "randd_expense.xlsx",
    sheet = "randd_expense_total"
  ) |> 
  dplyr::mutate(trait = "total") |> 
  dplyr::bind_rows(
    readxl::read_excel(
      "randd_expense.xlsx",
      sheet = "randd_expense_company"
    ) |> 
      dplyr::mutate(trait = "company")
  ) |> 
  tidyr::pivot_longer(
    cols = -c(1,10),
    names_to = "country",
    values_to = "expense"
  )
wage_japan <- 
  readxl::read_excel(
    "randd_expense.xlsx",
    sheet = "wage"
  ) |> 
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      factor
    )
  ) |> 
  dplyr::mutate(
    year = lubridate::ymd(paste0(year, "/01/01"))
  ) |> 
  tidyr::pivot_longer(
    cols = -c(1, 8),
    names_to = "country",
    values_to = "index"
  )
# draw figures
# R and D
line_randd_japan <- 
  randd_japan |> 
  ggplot2::ggplot(
    aes(
      x = year, 
      y = expense,
      color = country
    )
  ) +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data = subset(randd_japan, year == max(year, na.rm = TRUE)),
    aes(
      label = country
    ),
    nudge_x = 1,
    segment.alpha = 0.5,
    size = 5
  ) +
  labs(
    x = "Year",
    y = "Expense for R&D (Unit: Million JPY in PPP)",
    caption = "Your name",
    title = "Transition in investment for R&D by country",
    subtitle = "Data: NISTEP (https://www.nistep.go.jp/sti_indicator/2022/RM318_table.html)"
    
  ) +
  scale_color_discreterainbow() +
  facet_wrap(~ trait) +
  lims(x = c(min(randd_japan$year), max(randd_japan$year + 5))) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 18, hjust = 0, margin=margin(l=0))
  )
# save
ggsave(
  "line_randd_japan.pdf",
  plot = line_randd_japan,
  width = 300,
  height = 150,
  units = "mm",
  device = cairo_pdf
)
# wage
line_wage_japan <- 
  wage_japan |> 
  ggplot2::ggplot(
    aes(
      x = year, 
      y = index,
      color = country
    )
  ) +
  geom_line() +
  geom_text_repel(
    data = subset(wage_japan, year == max(year)),
    aes(
      label = country
    ),
    nudge_x = 500,
    segment.alpha = 1.0,
    size = 5
  ) +
  labs(
    x = "Year",
    y = "Wage index (CY1991 = 100)", 
    caption = "Your name",
    title = "Transition in wage by country",
    subtitle = "Data: OECD"
  ) +
  geom_point() +
  scale_color_okabeito() +
  facet_wrap(~ trait, scales = "free_y") +
  ggplot2::lims(x = c(min(wage_japan$year), max(wage_japan$year+years(5)))) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 18, hjust = 0, margin=margin(l = 0))
  )
# save
ggsave(
  "line_wage_japan.pdf",
  plot = line_wage_japan,
  width = 300,
  height = 150,
  units = "mm",
  device = cairo_pdf
)

# ----- wage.census -----
# read estatapi
appId <- source("appID.r")
# find data list from the estatapi
census_data_list <- 
  estatapi::estat_getStatsList(
    appId = appId$value,
    searchWord = "賃金構造基本統計調査"
  ) 
# save
write_excel_csv(census_data_list, "census_data_list.csv")
# お目当てのデータを取得する
# データサイズが大きいから、取得後一旦保存する。
census_length_school <-
  estatapi::estat_getStatsData(
    appId = appID,
    statsDataId = "0003425894"
  ) 
# データを保存
write_rds(census_length_school, "census_length_school.rds")
# データ読み込み
# 少々時間がかかる。
census_length_school <- 
  readr::read_rds("census_length_school.rds")
# 日本語版データ作成
census_length_school_sub_jp <- 
  census_length_school %>% 
  dplyr::select(-tab_code, -cat01_code, -cat02_code, -cat03_code, -cat04_code, -cat05_code, -cat06_code, -cat07_code, -time_code, -unit, -annotation) %>%
  data.table::setnames(c("attribute", "size","length_service","industry","gender", "school", "age_class", "private_public", "year", "value")) %>%
  dplyr::mutate(
    year = stringr::str_sub(year, start = 1, end = 4)
  ) %>% 
  dplyr::filter(
    size != "企業規模計（10人以上）" & length_service != "勤続年数計" & gender != "男女計" & school != "学歴計" & age_class != "年齢計" & private_public != "民営＋公営" & industry != "Ｔ１ 産業計" & attribute != "労働者数"
  ) %>% 
  dplyr::mutate(across(where(is.character), factor)) %>%
  droplevels() %>% 
  dplyr::mutate(
    year = factor(year, levels = c("2020","2021","2022","2023")),
    size = factor(size, levels = c("10～99人", "100～999人", "1,000人以上")),
    length_service = factor(length_service, levels = c("0年", "1～2年", "3～4年", "5～9年", "10～14年", "15～19年", "20～24年", "25～29年","30年以上")),
    school = factor(school, levels = c("不明", "中学", "高校", "専門学校", "高専・短大", "大学", "大学院")),
    age_class = factor(age_class, levels = c("～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳", "60～64歳", "65～69歳", "70歳～"))
  )
# 保存
readr::write_rds(census_length_school_sub_jp, "census_length_school_sub_jp.rds")
# read data
# Japanese
census_length_school_sub_jp <- 
  readr::read_rds("census_length_school_sub_jp.rds")
# line plot
# Japanese
line_census_length_school_sub_jp <- 
  census_length_school_sub_jp %>% 
  select(-private_public) %>% 
  group_by(attribute, school, year, industry) %>% 
  nest() %>% 
  dplyr::mutate(
    figure = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = .,
          aes(
            x = length_service,
            y = value,
            color = age_class,
            group = age_class
          )
        ) +
        geom_line() +
        geom_point() +
        khroma::scale_color_discreterainbow() + 
        labs(
          x = "勤続年数階級（単位：年）",
          y = "賃金（単位：1,000円）",
          title = paste0(attribute, " (", year, ")"),
          subtitle = paste0(industry,"・",school),
          color = "年齢階級"
        ) +
        facet_wrap(~ size + gender, scales = "free", ncol = 2) +
        theme_classic(base_family = "jpfont") +
        theme(
          strip.background = element_blank()
        )
    )
  )
# save
pdf("line_census_length_school_sub_jp.pdf", width = 12, height = 12)
# Loop through and print each plot
# line_census_length_school_sub_jp$figure
purrr::walk(line_census_length_school_sub_jp$figure, print)
dev.off()
