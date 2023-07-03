####################################################################
# 2023年労務管理論関連Rコード
# 2023年5月25日
# Yuzuru Utsunomiya, Ph. D.
# 
# ---- read.library ----
library(tidyverse)
library(khroma)
library(estatapi)
library(khroma)
library(viridis)
appID <- source("appID.r")$value
# 
# ---- immigrant.transition ----
df_immigrant <- 
  readxl::read_excel(
    "hrm_2023.xlsx",
    sheet = "immigrant"
  ) %>% 
  data.table::setnames(
    c("country","1860s","1870s","1880s","1890s","1900s")
  ) %>% 
  tidyr::pivot_longer(
    col = -country,
    names_to = "generation",
    values_to = "immigrant"
  ) %>% 
  dplyr::mutate(
    country = factor(country),
    generation = factor(generation, levels = c("1860s","1870s","1880s","1890s","1900s"))
  )
# 
line_immigrant <- 
  df_immigrant %>% 
  ggplot2::ggplot(
    aes(
      x = generation, 
      y = immigrant, 
      color = country, 
      group = country
    )
  ) +
  geom_line() + 
  geom_point() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(
    x = "Generation",
    y = "Immigrant (Unit: Persons)",
    caption = "https://www.jetro.go.jp/ext_images/jfile/report/05000661/05000661_001_BUP_0.pdf"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# 
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

# ----- working.hours -----
# 総労働時間
hours_total <- 
  estat_getStatsData(
    appId = appID,
    statsDataId = c("0003138221")
  ) %>% 
  dplyr::mutate(division = c("総実労働時間"))
# 所定内労働時間
hours_scheduled <- 
  estat_getStatsData(
    appId = appID,
    statsDataId = c("0003138255")
  ) %>% 
  dplyr::mutate(division = c("所定内労働時間"))
# 所定外労働時間
hours_extra <- 
  estat_getStatsData(
    appId = appID,
    statsDataId = c("0003138105")
  ) %>% 
  dplyr::mutate(division = c("所定外労働時間"))
# 上記データを統合
hours_combined <- 
  hours_scheduled %>% 
  dplyr::bind_rows(hours_extra) %>% 
  dplyr::bind_rows(hours_total) %>% 
  dplyr::select("調査月", "産業分類(200711改定)", "事業所規模", "就業形態", "調査年", "value", "division") %>% 
  data.table::setnames(c("month", "industry", "size", "type", "year", "value", "division")) %>% 
  dplyr::filter(
    month %in% c("10月", "11月", "12月", "1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月")
  ) %>% 
  dplyr::mutate(
    year_month_date = lubridate::ymd(paste0(stringr::str_sub(year, start = 1, end = 4), "/", stringr::str_replace_all(month, "月",""),"/01")),
    industry = stringr::str_remove_all(industry, "(.+?_)")
  ) %>% 
  dplyr::select(industry, size, type, division, year_month_date, value) %>% 
  dplyr::mutate(across(is.character,  factor)) %>% 
  na.omit() %>% 
  dplyr::filter(type != "就業形態計") 
write_rds(hours_combined, "./hrm/hours_combined.rds")
# 弾力的労働時間採用状況データ
wh_system <- 
  estatapi::estat_getStatsData(
    appId = appID,
    statsDataId = "0003297623"
  ) %>% 
  dplyr::select(-表章項目, -tab_code, -cat01_code, -cat02_code, -time_code, -unit, -annotation) %>% 
  data.table::setnames(c("industry","status","year","number")) %>% 
  dplyr::mutate(
    year = factor(stringr::str_sub(year, start = 1, end = 4), levels = c("2016","2018","2020","2022")),
    status = stringr::str_remove_all(status, "(.+?_)")
  ) %>% 
  dplyr::mutate(across(is.character,  factor)) %>% 
  dplyr::filter(status != "集計社数", industry != "調査産業計") %>% 
  droplevels()
# データを保存
write_rds(wh_system, "./hrm/wh_system.rds")
# 
# ----- working.hours.plot -----
# 作図
# hours_combined <- readr::read_rds("./hrm/hours_combined.rds")
hours_combined <- readRDS("hours_combined.rds")
line_hours_combined <- 
  hours_combined %>% 
  group_by(industry, size, division) %>% 
  nest() %>% 
  dplyr::mutate(
    figure = purrr::map(
      data, 
      ~ 
        ggplot2::ggplot(
          data = .,
          aes(
            x = year_month_date,
            y = value,
            color = type
          )
        ) +
        geom_line() +
        scale_color_okabeito() +
        labs(
          x = "年次（1993-2015年）",
          y = "労働時間（指数。2010年平均＝100）",
          color = "就業形態",
          title = paste(industry, size, division, sep = " ")
        ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          strip.background = element_blank(),
          text = element_text("Noto Sans Mono CJK JP")
        )
    )
  )
# # 保存
# 一度保存したら十分だから普段はコメントアウト。枚数多くて時間かかるし。
# Cairo::Cairo(
#   type = "pdf",
#   file = "line_hours_combined.pdf",
#   width = 200, 
#   height = 200,
#   units = "mm"
# )
# line_hours_combined$figure
# dev.off()

# ----- wh.system.plot -----
# 弾力的労働時間棒グラフを描くよ
# データ読み込み
wh_system <- readRDS("wh_system.rds")
# 作図
mosaic_wh_system <- 
  wh_system %>% 
  dplyr::mutate(
    number = ifelse(is.na(number), 0, number)
  ) %>% 
  ggplot2::ggplot(
    data = .,
    aes(
      x = year,
      y = number,
      fill = status
    )
  ) +
  geom_col(position = "fill") +
  scale_fill_okabeito() +
  labs(x = "年次", y = "調査社数比", fill = "労働時間制度") +
  guides(fill=guide_legend(ncol=3))+
  theme_classic() +
  theme(legend.position = "bottom") 

overwork <- 
  estat_getStatsData(
    appId = appID,
    statsDataId = "0003138108")
levels(factor(overwork$調査年月))
readr::write_rds(overwork, "overwork.rds")

# 年功賃金用作図コード
# データ読み込み
seniority <- 
  estat_getStatsData(
    appId = appID,
    statsDataId = "0003425894"
    )
seniority
readr::write_excel_csv(seniority,"seniority_0003425894.csv")
# フィルタ
seniority <- 
  readr::read_csv("seniority_0003425894.csv") %>% 
  dplyr::select(-tab_code, -cat01_code, -cat02_code, -cat03_code, -cat04_code, -cat05_code, -cat06_code, -cat07_code, -time_code, -unit,	-annotation) %>% 
  data.table::setnames(c("type", "size", "length_service", "industry", "gender", "school", "age_class", "public_private", "year", "amount")) %>% 
  dplyr::mutate(
    dplyr::across(
      where(is.character), as.factor
    )
  ) %>% 
  dplyr::filter(
    type != "労働者数" 
    & size != "企業規模計（10人以上）"
    & length_service != "勤続年数計"
    & industry == "Ｔ１ 産業計"
    & gender != "男女計"
    & school != "学歴計"
    & school != "不明"
    & age_class != "年齢計" 
    & public_private == "民営＋公営"
    ) %>% 
  droplevels() %>% 
  na.omit() %>% 
  dplyr::mutate(
    length_service = factor(length_service, levels = c("0年", "1～2年", "3～4年", "5～9年", "10～14年", "15～19年", "20～24年", "25～29年", "30年以上")),
    age_class = factor(age_class, levels = c("～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳", "60～64歳", "65～69歳", "70歳～"))
  )
# 作図
seniority_line <- 
  seniority %>% 
  ggplot2::ggplot(
    aes(
      x = length_service,
      y = amount,
      color = age_class,
      group = age_class
    )
  )  +
  geom_line() +
  geom_point() +
  scale_color_smoothrainbow(discrete = TRUE) +
  labs(x = "勤続年数", y = "賃金支給額（単位：1,000円）", title = "賃金区分・企業規模・性・学歴・勤続年数別賃金推移（産業計） ", subtitle = "2021年賃金構造基本統計調査より宇都宮作成。") + 
  facet_wrap(~ type + size + gender + school, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  ) +
  guides(color=guide_legend(nrow=1))
# 保存
ggsave(
  "seniority_line.pdf",
  plot = seniority_line,
  height = 1200,
  width = 1200,
  units = "mm",
  device = cairo_pdf
)

