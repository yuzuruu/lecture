####################################################################
# R graphics
# Make: 29th. March 2024
# Revision: 
# by Yuzuru Utsunomiya, Ph. D.
# Faculty of Economics, Nagasaki University
####################################################################
# 
# ----- read.library -----
library(tidyverse)
library(furrr)
library(khroma)
library(estatapi)
library(khroma)
library(viridis)
future::plan(multisession, workers = 16)
# ID should be obtained from estatapi independently.
# appID <- source("./r_workshop/appID.r")$value
# # 
# # ----- make.data -----
# # NOTE
# # This data is not only for education but also for my paper.
# # Students need not to run here.
# # obtain target list
# # hoge <-
# #   estat_getStatsList(
# #     appId = appID,
# #     searchWord = "賃金構造基本統計調査 AND 勤続年数 AND 年齢階級")
# # readr::write_excel_csv(hoge, "hoge.csv")
# 
# # set a list of target data ids
# # NOTE
# # The IDs below can be fixed using the target list above (hoge.csv)
# seniority_id <-
#   dplyr::tibble(
#     c(
#       "3029921", "3029940", "3029950", "3029958", "3029991", "3030013", "3030026", "3030038", "3030370", "3030640", "3030646", "3029853", "3030015", "3030017", "3030041", "3030042", "3030249", "3030279", "3030295", "3030309", "3030313", "3030329", "3030334", "3030340", "3030352", "3030369", "3030379", "3030641", "3030648", "3031418", "3030433", "3031709", "3031710", "3031711", "3031712", "3031713", "3031714", "3031715", "3031716", "3030841", "3030885", "3031421", "3030964", "3172122", "3173200", "3084623", "3084809", "3085551", "3085561", "3086610", "3089189", "3089206", "3089442", "3425894", "3426254", "3446898", "3130236", "3130256", "3130276", "3425893", "3426253", "3426933", "3439118", "3446258", "3446258", "3029886", "3030006", "3029801", "3029884", "3029911", "3029914", "3029923", "3029927", "3029936", "3029941", "3029949", "3029952", "3029959", "3029961", "3029971", "3029975", "3029982", "3029985", "3029986", "3030385", "3030468", "3030475", "3030484", "3030501", "3030509", "3030518", "3030654", "3030867", "3030868", "3030692", "3030761", "3030789", "3030801", "3030803", "3030804", "3030806", "3030808", "3030809", "3030813", "3030824", "3030838", "3030877", "3030909", "3030915", "3030928", "3030946", "3030974", "3030991", "3030999", "3031012", "3031018", "3031020", "3031029", "3031033", "3031037", "3031038", "3031053", "3031064", "3031065", "3031066", "3031078", "3031141", "3031154", "3031209", "3031210", "3030870", "3030879", "3030927", "3030965", "3171770", "3172100", "3082750", "3084009", "3084561", "3084609", "3084616", "3084983", "3085001", "3085030", "3085317", "3085570", "3085571", "3089163", "3446878"
#       )
#     ) %>%
#   data.table::setnames(c("id")) %>%
#   dplyr::mutate(
#     id = paste0("000", id)
#   )
# # download the target data and save them into a folder named "seniority_data"
# # NOTE
# # This process spends loong computation period (Approx. 12 hours)
# # COMMENT OUT WHEN NOT IN USE.
# # USE SAVED csv FILES
# seniority_data <-
#   for(i in 1:length(seniority_id$id)){
#     dt <- furrr::future_map(
#           .x = seniority_id,
#           ~ 
#             estat_getStatsData(
#               appId = appID,
#               statsDataId = .x[i]
#             )
#         )
#     # save the computation results
#     readr::write_excel_csv(
#       # fix target column
#       dt$id,
#       file = paste0("seniority_data/", seniority_id$id[i], ".csv")
#     )
#   }
# # read and reshape the read data
# seniority_data_01 <-
#   # read the data
#   # We have downloaded many files. This time, we use only one target file.
#   # Let us make use of other data in the future.
#   readr::read_csv("./seniority_data/0003084623.csv") %>%
#   # Transform character variable into factor.
#   dplyr::mutate(
#     across(
#       where(is.character),  
#       factor
#       )
#     ) %>%
#   # select necessary variables
#   dplyr::select(-tab_code, -cat01_code, -cat02_code, -cat03_code, -cat04_code, -cat05_code, -cat06_code, -cat07_code, -time_code, -annotation) %>%
#   # Change variables' names for convenience
#   data.table::setnames(c("type", "gender", "school", "age_class", "length_service", "industry", "size", "public_private", "year", "unit", "value")) %>%
#   # Remove aggregate element
#   dplyr::filter(
#     size != "企業規模計（10人以上）"
#     & length_service != "勤続年数計"
#     & industry != "Ｔ１ 産業計"
#     & gender != "男女計"
#     & school != "学歴計"
#     & school != "不明"
#     & age_class != "年齢計"
#     & public_private != "民営＋公営"
#     & unit == "千円"
#   ) %>%
#   droplevels() %>%
#   dplyr::mutate(
#     length_service = factor(length_service, levels = c("0年", "1～2年", "3～4年", "5～9年", "10～14年", "15～19年", "20～24年", "25～29年", "30年以上")),
#     age_class = factor(age_class, levels = c("～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳", "40～44歳", "45～49歳", "50～54歳", "55～59歳", "60～64歳", "65～69歳", "70歳～")),
#     # For convenience, we assume that the statistics are uploaded on 1st. June every year.
#     year = lubridate::ymd(paste0(year, "/06/01"))
#   ) %>%
#   # replace Japanese phrases into English ones
#   dplyr::mutate(
#     type = dplyr::case_when(
#       type == "労働者数" ~ "labour_force",
#       type == "年間賞与その他特別給与額" ~ "regular_payment",
#       type == "所定内給与額" ~ "dividend",
#       TRUE ~ "hoge"
#     ),
#     gender = dplyr::case_when(
#       gender == "男" ~ "male",
#       gender == "女" ~ "female",
#       TRUE ~ "hoge"
#       ),
#     school = dplyr::case_when(
#       school == "中学卒" ~ "junior",
#       school == "高校卒" ~ "high",
#       school == "高専・短大卒" ~ "college",
#       school == "大学・大学院卒" ~ "university",
#       TRUE ~ "hoge"
#       ),
#     age_class = dplyr::case_when(
#       age_class == "～19歳" ~ "under_19",
#       age_class == "20～24歳" ~ "20-24",
#       age_class == "25～29歳" ~ "25-29",
#       age_class == "30～34歳" ~ "30-34",
#       age_class == "35～39歳" ~ "35-39",
#       age_class == "40～44歳" ~ "40-44",
#       age_class == "45～49歳" ~ "45-49",
#       age_class == "50～54歳" ~ "50-54",
#       age_class == "55～59歳" ~ "55-59",
#       age_class == "60～64歳" ~ "60-64",
#       age_class == "65～69歳" ~ "65-69",
#       age_class == "70歳～" ~ "over_70",
#       TRUE ~ "hoge"
#     ),
#     length_service = dplyr::case_when(
#       length_service == "0年" ~ "0_years",
#       length_service == "1～2年" ~ "1-2",
#       length_service == "3～4年" ~ "3-4",
#       length_service == "5～9年" ~ "5-9",
#       length_service == "10～14年" ~ "10-14",
#       length_service == "15～19年" ~ "15-19",
#       length_service == "20～24年" ~ "20-24",
#       length_service == "25～29年" ~ "25-29",
#       length_service == "30年以上" ~ "over_30_years",
#       TRUE ~ "hoge"
#     ),
#     size = dplyr::case_when(
#       size == "10～99人" ~ "10-99",
#       size == "100～999人" ~ "100-999",
#       size == "1,000人以上" ~ "over_1000_persons",
#       TRUE ~ "hoge"
#     ),
#     industry = dplyr::case_when(
#       industry == "Ｃ 鉱業，採石業，砂利採取業" ~ "MINING AND QUARRYING OF STONE",
#       industry == "Ｄ 建設業" ~ "CONSTRUCTION",
#       industry == "Ｄ０６ 総合工事業" ~ "CONSTRUCTION WORK, GENERAL INCLUDING PUBLIC AND PRIVATE CONSTRUCTION WORK",
#       industry == "Ｄ０７ 職別工事業（設備工事業を除く）" ~ "CONSTRUCTION WORK BY SPECIALIST CONTRACTOR, EXCEPT EQUIPMENT INSTALLATION WORK",
#       industry == "Ｄ０８ 設備工事業" ~ "EQUIPMENT INSTALLATION WORK",
#       industry == "Ｅ 製造業" ~ "MANUFACTURING",
#       industry == "Ｅ０９ 食料品製造業" ~ "MANUFACTURE OF FOOD",
#       industry == "Ｅ１０ 飲料・たばこ・飼料製造業" ~ "MANUFACTURE OF BEVERAGES,TOBACCO AND FEED",
#       industry == "Ｅ１１ 繊維工業" ~ "MANUFACTURE OF TEXTILE PRODUCTS",
#       industry == "Ｅ１２ 木材・木製品製造業(家具を除く)" ~ "MANUFACTURE OF LUMBER AND WOOD PRODUCTS, EXCEPT FOURNITURE",
#       industry == "Ｅ１３ 家具・装備品製造業" ~ "MANUFACTURE OF FURNITURE AND FIXTURES  ",
#       industry == "Ｅ１４ パルプ・紙・紙加工品製造業" ~ "MANUFACTURE OF PULP, PAPER AND PAPER PRODUCTS ",
#       industry == "Ｅ１５ 印刷・同関連業" ~ "PRINTING AND ALLIED INDUSTRIES",
#       industry == "Ｅ１６ 化学工業" ~ "MANUFACTURE OF CHEMICAL AND ALLIED PRODUCT",
#       industry == "Ｅ１７ 石油製品・石炭製品製造業" ~ "MANUFACTURE OF PETROLEUM AND COAL PRODUCTS",
#       industry == "Ｅ１８ プラスチック製品製造業(別掲を除く)" ~ "MANUFACTURE OF PLASTIC PRODUCTS, EXCEPT OTHERWISE CLASSIFIED",
#       industry == "Ｅ１９ ゴム製品製造業" ~ "MANUFACTURE OF RUBBER PRODUCTS ",
#       industry == "Ｅ２０ なめし革・同製品・毛皮製造業" ~ "MANUFACTURE OF LEATHER TANNING, LEATHER PRODUCTS AND FUR SKINS",
#       industry == "Ｅ２１ 窯業・土石製品製造業" ~ "MANUFACTURE OF CERAMIC, STONE AND CLAY PRODUCTS",
#       industry == "Ｅ２２ 鉄鋼業" ~ "MANUFACTURE OF IRON AND STEEL",
#       industry == "Ｅ２３ 非鉄金属製造業" ~ "MANUFACTURE OF NON-FERROUS METALS AND PRODUCTS",
#       industry == "Ｅ２４ 金属製品製造業" ~ "MANUFACTURE OF FABRICATED METAL PRODUCTS",
#       industry == "Ｅ２５ はん用機械器具製造業" ~ "MANUFACTURE OF GENERAL-PURPOSE MACHINERY",
#       industry == "Ｅ２６ 生産用機械器具製造業" ~ "MANUFACTURE OF PRODUCTION MACHINERY",
#       industry == "Ｅ２７ 業務用機械器具製造業" ~ "MANUFACTURE OF BUSINESS ORIENTED MACHINERY",
#       industry == "Ｅ２８ 電子部品・デバイス・電子回路製造業" ~ "ELECTRONIC PARTS, DEVICES AND ELECTRONIC CIRCUITS",
#       industry == "Ｅ２９ 電気機械器具製造業" ~ "MANUFACTURE OF ELECTRICAL MACHINERY, EQUIPMENT AND SUPPLIES",
#       industry == "Ｅ３０ 情報通信機械器具製造業" ~ "MANUFACTURE OF INFORMATION AND COMMUNICATION ELECTRONICS EQUIPMENT",
#       industry == "Ｅ３１ 輸送用機械器具製造業" ~ "MANUFACTURE OF TRANSPORTATION EQUIPMENT",
#       industry == "Ｅ３２ その他の製造業" ~ "MISCELLANEOUS MANUFACTURING INDUSTRIES",
#       industry == "Ｆ 電気・ガス・熱供給・水道業" ~ "ELECTRICITY, GAS, HEAT SUPPLY AND WATER",
#       industry == "Ｆ３３ 電気業" ~ "PRODUCTION, TRANSMISSION AND DISTRIBUTION OF ELECTRICITY",
#       industry == "Ｆ３４ ガス業" ~ "PRODUCTION AND DISTRIBUTION OF GAS",
#       industry == "Ｆ３５ 熱供給業" ~ "HEAT SUPPLY",
#       industry == "Ｆ３６ 水道業" ~ "COLLECTION, PURIFICATION AND DISTRIBUTION OF WATER, AND SEWAGE COLLECTION, PROCESSING AND DISPOSAL",
#       industry == "Ｇ 情報通信業" ~ "INFORMATION AND COMMUNICATIONS",
#       industry == "Ｇ３７ 通信業" ~ "COMMUNICATIONS",
#       industry == "Ｇ３８ 放送業" ~ "BROADCASTING",
#       industry == "Ｇ３９ 情報サービス業" ~ "INFORMATION SERVICES",
#       industry == "Ｇ４０ インターネット附随サービス業" ~ "SERVICES INCIDENTAL TO INTERNET",
#       industry == "Ｇ４１ 映像・音声・文字情報制作業" ~ "VIDEO PICTURE INFORMATION, SOUND INFORMATION, CHARACTER INFORMATION PRODUCTION AND DISTRIBUTION",
#       industry == "Ｈ 運輸業，郵便業" ~ "TRANSPORT AND POSTAL SERVICES",
#       industry == "Ｈ４２ 鉄道業" ~ "RAILWAY TRANSPORT",
#       industry == "Ｈ４３ 道路旅客運送業" ~ "ROAD PASSENGER TRANSPORT",
#       industry == "Ｈ４４ 道路貨物運送業" ~ "ROAD FREIGHT TRANSPORT",
#       industry == "Ｈ４５ 水運業" ~ "WATER TRANSPORT",
#       industry == "Ｈ４６ 航空運輸業" ~ "AIR TRANSPORT",
#       industry == "Ｈ４７ 倉庫業" ~ "WAREHOUSING",
#       industry == "Ｈ４８ 運輸に附帯するサービス業" ~ "SERVICES INCIDENTAL TO TRANSPORT",
#       industry == "Ｈ４９ 郵便業（信書便事業を含む）" ~ "POSTAL SERVICES, INCLUDING MAIL DELIVERY",
#       industry == "Ｉ 卸売業，小売業" ~ "WHOLESALE AND RETAIL TRADE",
#       industry == "Ｉ５０ 各種商品卸売業" ~ "WHOLESALE TRADE, GENERAL MERCHANDISE",
#       industry == "Ｉ５０～５５ 卸売業" ~ "hoge",
#       industry == "Ｉ５１ 繊維・衣服等卸売業" ~ "WHOLESALE TRADE (TEXTILE AND APPAREL)",
#       industry == "Ｉ５２ 飲食料品卸売業" ~ "WHOLESALE TRADE (FOOD AND BEVERAGES)",
#       industry == "Ｉ５３ 建築材料，鉱物・金属材料等卸売業" ~ "WHOLESALE TRADE  (BUILDING MATERIALS, MINERALS AND METALS, ETC) ",
#       industry == "Ｉ５４ 機械器具卸売業" ~ "WHOLESALE TRADE (MACHINERY AND EQUIPMENT)",
#       industry == "Ｉ５５ その他の卸売業" ~ "MISCELLANEOUS WHOLESALE TRADE ",
#       industry == "Ｉ５６ 各種商品小売業" ~ "RETAIL TRADE, GENERAL MERCHANDISE",
#       industry == "Ｉ５６～６１ 小売業" ~ "hoge",
#       industry == "Ｉ５７ 織物・衣服・身の回り品小売業" ~ "RETAIL TRADE (WOVEN FABRICS, APPAREL, APPAREL ACCESSORIES AND NOTIONS)",
#       industry == "Ｉ５８ 飲食料品小売業" ~ "RETAIL TRADE (FOOD AND BEVERAGE)",
#       industry == "Ｉ５９ 機械器具小売業" ~ "RETAIL TRADE (MACHINERY AND EQUIPMENT)",
#       industry == "Ｉ６０ その他の小売業" ~ "MISCELLANEOUS RETAIL TRADE",
#       industry == "Ｉ６１ 無店舗小売業" ~ "NONSTORE RETAILERS",
#       industry == "Ｊ 金融業，保険業" ~ "FINANCE AND INSURANCE",
#       industry == "Ｊ６２ 銀行業" ~ "BANKING",
#       industry == "Ｊ６３ 協同組織金融業" ~ "FINANCIAL INSTITUTIONS FOR COOPERATIVE ORGANIZATIONS",
#       industry == "Ｊ６４ 貸金業，クレジットカード業等非預金信用機関" ~ "NON-DEPOSIT MONEY CORPORATIONS, INCLUDING LENDING AND CREDIT CARD BUSINESS",
#       industry == "Ｊ６５ 金融商品取引業，商品先物取引業" ~ "FINANCIAL PRODUCTS TRANSACTION DEALERS AND FUTURES COMMODITY TRANSACTION DEALERS",
#       industry == "Ｊ６６ 補助的金融業等" ~ "FINANCIAL AUXILIARIES",
#       industry == "Ｊ６７ 保険業(保険媒介代理業,保険サービス業を含む)" ~ "INSURANCE INSTITUTIONS, INCLUDING INSURANCE AGENTS, BROKERS AND SERVICES ",
#       industry == "Ｋ 不動産業，物品賃貸業" ~ "REAL ESTATE AND GOODS RENTAL AND LEASING",
#       industry == "Ｋ６８ 不動産取引業" ~ "REAL ESTATE AGENCIES",
#       industry == "Ｋ６９ 不動産賃貸業・管理業" ~ "REAL ESTATE LESSORS AND MANAGERS",
#       industry == "Ｋ７０ 物品賃貸業" ~ "GOODS RENTAL AND LEASING",
#       industry == "Ｌ 学術研究，専門・技術サービス業" ~ "SCIENTIFIC RESEARCH, PROFESSIONAL AND TECHNICAL SERVICES",
#       industry == "Ｌ７１ 学術・開発研究機関" ~ "SCIENTIFIC AND DEVELOPMENT RESEARCH INSTITUTES",
#       industry == "Ｌ７２ 専門サービス業（他に分類されないもの）" ~ "PROFESSIONAL SERVICES, N.E.C.",
#       industry == "Ｌ７３ 広告業" ~ "ADVERTISING",
#       industry == "Ｌ７４ 技術サービス業(他に分類されないもの）" ~ "TECHNICAL SERVICES, N.E.C.",
#       industry == "Ｍ 宿泊業，飲食サービス業" ~ "ACCOMMODATIONS, EATING AND DRINKING SERVICES",
#       industry == "Ｍ７５ 宿泊業" ~ "ACCOMMODATIONS",
#       industry == "Ｍ７６ 飲食店" ~ "EATING AND DRINKING PLACES",
#       industry == "Ｍ７７ 持ち帰り・配達飲食サービス業" ~ "FOOD TAKE OUT AND DELIVERY SERVICES",
#       industry == "Ｎ 生活関連サービス業，娯楽業" ~ "LIVING-RELATED AND PERSONAL SERVICES AND AMUSEMENT SERVICES",
#       industry == "Ｎ７８ 洗濯・理容・美容・浴場業" ~ "LAUNDRY, BEAUTY AND BATH SERVICES",
#       industry == "Ｎ７９ その他の生活関連サービス業" ~ "MISCELLANEOUS LIVING-RELATED AND PERSONAL SERVICES",
#       industry == "Ｎ８０ 娯楽業" ~ "SERVICES FOR AMUSEMENT AND RECREATION",
#       industry == "Ｏ 教育，学習支援業" ~ "EDUCATION, LEARNING SUPPORT",
#       industry == "Ｏ８１ 学校教育" ~ "SCHOOL EDUCATION",
#       industry == "Ｏ８２ その他の教育，学習支援業" ~ "MISCELLANEOUS EDUCATION, LEARNING SUPPORT",
#       industry == "Ｐ 医療，福祉" ~ "MEDICAL, HEALTH CARE AND WELFARE",
#       industry == "Ｐ８３ 医療業" ~ "MEDICAL AND OTHER HEALTH SERVICES",
#       industry == "Ｐ８４ 保健衛生" ~ "PUPLIC HEALTH AND HYGIENE ",
#       industry == "Ｐ８５ 社会保険・社会福祉・介護事業" ~ "SOCIAL INSURANCE, SOCIAL WELFARE AND CARE SERVICES",
#       industry == "Ｑ 複合サービス事業" ~ "COMPOUND SERVICES",
#       industry == "Ｑ８６ 郵便局" ~ "POSTAL SERVICES",
#       industry == "Ｑ８７ 協同組合（他に分類されないもの）" ~ "COOPERATIVE ASSOCIATIONS, N.E.C",
#       industry == "Ｒ サービス業（他に分類されないもの）" ~ "SERVICES, N.E.C.",
#       industry == "Ｒ８８ 廃棄物処理業" ~ "WASTE DISPOSAL BUSINESS",
#       industry == "Ｒ８９ 自動車整備業" ~ "AUTOMOBILE MAINTENANCE SERVICES",
#       industry == "Ｒ９０ 機械等修理業(別掲を除く)" ~ "MACHINE, ETC. REPAIR SERVICES, EXCEPT OTHERWISE CLASSIFIED",
#       industry == "Ｒ９１ 職業紹介・労働者派遣業" ~ "EMPLOYMENT AND WORKER DISPATCHING  SERVICES",
#       industry == "Ｒ９２ その他の事業サービス業" ~ "MISCELLANEOUS BUSINESS SERVICES",
#       industry == "Ｒ９３ 政治・経済・文化団体" ~ "POLITICAL, BUSINESS AND CULTURAL ORGANIZATIONS ",
#       industry == "Ｒ９４ 宗教" ~ "RELIGION",
#       industry == "Ｒ９５ その他のサービス業" ~ "MISCELLANEOUS SERVICES",
#       TRUE ~ "hoge"
#       )
#     ) %>%
#   dplyr::select(-public_private, -unit) %>%
#   dplyr::filter(industry != "hoge") %>%
#   dplyr::mutate(across(where(is.character),  factor)) %>%
#   dplyr::mutate(
#     age_class = factor(age_class, levels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "over_70", "under_19")),
#     length_service = factor(length_service, levels = c("0_years", "1-2", "3-4", "5-9", "10-14", "15-19", "20-24", "25-29", "over_30_years")),
#     size = factor(size, levels = c("10-99", "100-999", "over_1000_persons"))
#   )
# # save the results
# # csv
# readr::write_excel_csv(seniority_data_01, "seniority_data_01.csv")
# # rds
# readr::write_rds(seniority_data_01, "seniority_data_01.rds")
# #
# # ----- line.seniority.01 -----
# # read data
# seniority_data_01 <- readr::read_rds("seniority_data_01.rds")
# # plot
# seniority_data_01_line <- 
#   seniority_data_01 %>% 
#   dplyr::filter(industry %in% c("MINING AND QUARRYING OF STONE", "CONSTRUCTION", "MANUFACTURING", "ELECTRICITY, GAS, HEAT SUPPLY AND WATER", "INFORMATION AND COMMUNICATIONS", "TRANSPORT AND POSTAL SERVICES", "WHOLESALE AND RETAIL TRADE", "FINANCE AND INSURANCE", "REAL ESTATE AND GOODS RENTAL AND LEASING", "SCIENTIFIC RESEARCH, PROFESSIONAL AND TECHNICAL SERVICES", "ACCOMMODATIONS, EATING AND DRINKING SERVICES", "LIVING-RELATED AND PERSONAL SERVICES AND AMUSEMENT SERVICES",  "EDUCATION, LEARNING SUPPORT", "MEDICAL, HEALTH CARE AND WELFARE", "COMPOUND SERVICES", "SERVICES, N.E.C.")) %>% 
#   # na.omit() %>% 
#   group_by(type, industry, size) %>% 
#   nest() %>% 
#   dplyr::mutate(
#     seniority_line = purrr::map(
#       data,
#       ~
#         ggplot2::ggplot(
#           data = .,
#           aes(
#             x = length_service,
#             y = value,
#             color = age_class,
#             group = age_class
#           ) 
#         ) +
#         geom_line() +
#         geom_point() + 
#         scale_color_smoothrainbow(discrete = TRUE) +
#         labs(
#           x = "Length of service (Unit: years)",
#           y = "Wage amount (Unit: 1,000JPY)",
#           color = "Age class (Unit: years)",
#           title = industry, 
#           subtitle = paste0(type, " (",size,")")
#         ) + 
#         facet_wrap(~ gender + school + year, scales = "free_y") +
#         theme_classic() +
#         theme(
#           axis.text.x = element_text(angle =45, hjust = 1),
#           legend.position = "bottom",
#           strip.background = element_blank()
#         ) +
#         guides(color=guide_legend(nrow=1))
#     )
#   )
# # save the line plots
# pdf(
#   "seniority_data_01_line.pdf",
#   width = 20,
#   height = 20
# )
# hoge$seniority_line
# dev.off()

