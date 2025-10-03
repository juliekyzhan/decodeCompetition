# new try for map+ppvi

# data cleansing ----

# adver

adver_data_unique <- adver_data %>%
  distinct(full_name, congress, .keep_all = TRUE) %>%
  select(full_name, congress, ppvi)

adver_cg_heatmap$congress <- as.character(adver_cg_heatmap$congress)
adver_data_unique$congress <- as.character(adver_data_unique$congress)

adver_cg_heatmap <- adver_cg_heatmap %>%
  left_join(adver_data_unique, by = c("full_name", "congress"))

# legis

legis_data_unique <- legis_data %>%
  distinct(full_name, congress, .keep_all = TRUE) %>%
  select(full_name, congress, ppvi)

legis_cg_heatmap$congress <- as.character(legis_cg_heatmap$congress)
legis_data_unique$congress <- as.character(legis_data_unique$congress)

legis_cg_heatmap <- legis_cg_heatmap %>%
  left_join(legis_data_unique, by = c("full_name", "congress"))

# state


adver_cg_heatmap1 <- adver_cg %>%
  filter(chamber == "Senate")

adver_cg_heatmap1$congress <- as.character(adver_cg_heatmap1$congress)
adver_cg_heatmap1 <- adver_cg_heatmap1 %>%
  left_join(adver_data_unique, by = c("full_name", "congress"))

adver_cg_heatmap1  <- adver_cg_heatmap1  %>%
  mutate(
    district_clean = case_when(
      grepl("At Large", district, ignore.case = TRUE) ~ "00",
      state_code %in% c("DC", "AS", "GU", "MP", "PR", "VI") ~ "98",
      grepl("^[0-9]+$", district) ~ sprintf("%02d", as.integer(district)),
      grepl("^[0-9]+(st|nd|rd|th)$", district, ignore.case = TRUE) ~ sprintf(
        "%02d", as.integer(str_extract(district, "^[0-9]+"))
      ),
      TRUE ~ NA_character_  
    ),
    state_district = paste0(state_code, "-", district_clean)
  )


legis_cg_heatmap1 <- legis_cg %>%
  filter(chamber == "Senate")
legis_cg_heatmap1$congress <- as.character(legis_cg_heatmap1$congress)
legis_cg_heatmap1 <- legis_cg_heatmap1 %>%
  left_join(legis_data_unique, by = c("full_name", "congress"))

legis_cg_heatmap1  <- legis_cg_heatmap1  %>%
  mutate(
    district_clean = case_when(
      grepl("At Large", district, ignore.case = TRUE) ~ "00",
      state_code %in% c("DC", "AS", "GU", "MP", "PR", "VI") ~ "98",
      grepl("^[0-9]+$", district) ~ sprintf("%02d", as.integer(district)),
      grepl("^[0-9]+(st|nd|rd|th)$", district, ignore.case = TRUE) ~ sprintf(
        "%02d", as.integer(str_extract(district, "^[0-9]+"))
      ),
      TRUE ~ NA_character_  
    ),
    state_district = paste0(state_code, "-", district_clean)
  )






# prepartion ----

# debug district data issue
unique(adver_cg_heatmap$district[is.na(as.integer(adver_cg_heatmap$district))])

# turn at-large into 00
adver_cg_heatmap  <- adver_cg_heatmap  %>%
  mutate(
    district_clean = case_when(
      grepl("At Large", district, ignore.case = TRUE) ~ "00",
      state_code %in% c("DC", "AS", "GU", "MP", "PR", "VI") ~ "98",
      grepl("^[0-9]+$", district) ~ sprintf("%02d", as.integer(district)),
      grepl("^[0-9]+(st|nd|rd|th)$", district, ignore.case = TRUE) ~ sprintf(
        "%02d", as.integer(str_extract(district, "^[0-9]+"))
      ),
      TRUE ~ NA_character_  
    ),
    state_district = paste0(state_code, "-", district_clean)
  )


unique(adver_cg_heatmap$district_clean[is.na(as.integer(adver_cg_heatmap$district_clean))]) 

unique(legis_cg_heatmap$district[is.na(as.integer(legis_cg_heatmap$district))])

legis_cg_heatmap  <- legis_cg_heatmap  %>%
  mutate(
    district_clean = case_when(
      grepl("At Large", district, ignore.case = TRUE) ~ "00",
      state_code %in% c("DC", "AS", "GU", "MP", "PR", "VI") ~ "98",
      grepl("^[0-9]+$", district) ~ sprintf("%02d", as.integer(district)),
      grepl("^[0-9]+(st|nd|rd|th)$", district, ignore.case = TRUE) ~ sprintf(
        "%02d", as.integer(str_extract(district, "^[0-9]+"))
      ),
      TRUE ~ NA_character_  
    ),
    state_district = paste0(state_code, "-", district_clean)
  )


unique(legis_cg_heatmap$district_clean[is.na(as.integer(legis_cg_heatmap$district_clean))]) 


# fips-state
fips_state_map <- data.frame(
  fips = c(
    "01","02","04","05","06","08","09","10","12","13","15","16","17","18","19",
    "20","21","22","23","24","25","26","27","28","29","30","31","32","33","34",
    "35","36","37","38","39","40","41","42","44","45","46","47","48","49","50",
    "51","53","54","55","56","11","60","66","69","72","78"
  ),
  state = c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
    "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
    "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
    "VA","WA","WV","WI","WY","DC","AS","GU","MP","PR","VI"
  ),
  stringsAsFactors = FALSE
)

# custom transformers
state_to_fips <- function(state_code) {
  res <- fips_state_map$fips[match(state_code, fips_state_map$state)]
  return(res)
}

fips_to_state <- function(fips_code) {
  # make sure fips_code is character
  fips_code <- sprintf("%02s", as.character(fips_code))
  res <- fips_state_map$state[match(fips_code, fips_state_map$fips)]
  return(res)
}

# test
fips_to_state("01")  # shoule be "AL"
fips_to_state(9)     # shoule be "CT"
fips_to_state(23)   # shoule be ME
fips_to_state(44)   # shoule be RI
fips_to_state(6)   # shoule be CA

# core function1 ----


library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(cowplot)
library(stringr)
# 以及你之前用到的包，例如 tigris / tidycensus（congressional_districts, fips_to_state 等）

plot_decision_map_by_congress1 <- function(congress_num,
                                           decision_data,
                                           model_type = c("adver", "legis"),
                                           save_path = NULL,
                                           save_width = 10,
                                           save_height = 7) {
  model_type <- match.arg(model_type)
  
  # 地图年份对应
  map_year <- switch(
    as.character(congress_num),
    "115" = 2016,
    "116" = 2020,
    "117" = 2020,
    "118" = 2022,
    stop("Unsupported congress number.")
  )
  
  # district字段对应
  district_col <- switch(
    as.character(congress_num),
    "115" = "CD115FP",
    "116" = "CD116FP",
    "117" = "CD116FP",
    "118" = "CD118FP",
    stop("Unsupported congress number.")
  )
  
  # 载入地图数据
  cd_map <- congressional_districts(cb = TRUE, year = map_year) %>%
    mutate(
      state_code = fips_to_state(STATEFP),
      district_code = as.character(!!rlang::sym(district_col)),
      district_clean = sprintf("%02d", as.integer(district_code)),
      state_district = paste0(state_code, "-", district_clean),
      state_district = stringr::str_trim(as.character(state_district))
    )
  
  # 决策数据
  
  decision_df <- decision_data %>%
    filter(congress == as.character(congress_num)) %>%
    mutate(state_district = stringr::str_trim(as.character(state_district)))
  
  # 合并
  plot_df <- cd_map %>%
    left_join(decision_df %>% select(state_district, decision, ppvi),
              by = "state_district") %>%
    mutate(
      ppvi = as.character(ppvi),
      decision = as.character(decision),
      group_var = ifelse(
        is.na(ppvi) | is.na(decision) | ppvi == "" | decision == "",
        "missing",
        paste0(ppvi, "_", decision)
      )
    )
  
  # ----------- 自定义颜色映射 -----------
  pal_vec <- c(
    # R 系：红色不同深浅
    "R_intl"  = "#b2182b",  # 深红
    "R_dom"   = "#ef8a62",  # 中红/橙红
    "R_prof"  = "#fddbc7",  # 浅红/粉红
    
    # D 系：蓝色不同深浅
    "D_intl"  = "#2166ac",  # 深蓝
    "D_dom"   = "#67a9cf",  # 中蓝
    "D_prof"  = "#d1e5f0",  # 浅蓝
    
    # 缺失值
    "missing" = "grey80"
  )
  # 只保留出现在数据里的颜色
  used_levels <- unique(plot_df$group_var)
  pal_vec <- pal_vec[names(pal_vec) %in% used_levels]
  
  plot_df <- plot_df %>%
    mutate(group_var = factor(group_var, levels = names(pal_vec)))
  
  # 主图
  main_map <- ggplot(plot_df) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(
      values = pal_vec,
      drop = FALSE,
      name = "PPVI + Decision"
    ) +
    labs(
      title = paste0(
        ifelse(model_type == "adver", "Adver", "Legis"),
        " Decision Mode with PPVI by District (", congress_num, "th)"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)
  
  # Alaska
  alaska_map <- ggplot(filter(plot_df, state_code == "AK")) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(values = pal_vec, guide = "none") +
    coord_sf(xlim = c(-180, -130), ylim = c(50, 72), expand = FALSE) +
    theme_void()
  
  # Hawaii
  hawaii_map <- ggplot(filter(plot_df, state_code == "HI")) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(values = pal_vec, guide = "none") +
    coord_sf(xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
    theme_void()
  
  final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(main_map, 0, 0, 1, 1) +
    cowplot::draw_plot(alaska_map, x = 0.03, y = 0.15, width = 0.25, height = 0.25) +
    cowplot::draw_plot(hawaii_map, x = 0.20, y = 0.15, width = 0.25, height = 0.25)
  
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = final_map, width = save_width, height = save_height)
  }
  
  return(final_map)
}


# core function2 ----

library(ggnewscale)

plot_decision_map_by_congress2 <- function(congress_num,
                                           decision_data,
                                           model_type = c("adver", "legis"),
                                           save_path = NULL,
                                           save_width = 10,
                                           save_height = 7) {
  model_type <- match.arg(model_type)
  
  # 地图年份
  map_year <- switch(
    as.character(congress_num),
    "115" = 2016,
    "116" = 2020,
    "117" = 2020,
    "118" = 2022,
    stop("Unsupported congress number.")
  )
  
  district_col <- switch(
    as.character(congress_num),
    "115" = "CD115FP",
    "116" = "CD116FP",
    "117" = "CD116FP",
    "118" = "CD118FP"
  )
  
  # 地图数据
  cd_map <- congressional_districts(cb = TRUE, year = map_year) %>%
    mutate(
      state_code = fips_to_state(STATEFP),
      district_code = as.character(!!rlang::sym(district_col)),
      district_clean = sprintf("%02d", as.integer(district_code)),
      state_district = paste0(state_code, "-", district_clean),
      state_district = str_trim(as.character(state_district))
    )
  
  # 决策数据
  decision_df <- decision_data %>%
    filter(congress == as.character(congress_num)) %>%
    mutate(state_district = str_trim(as.character(state_district)))
  
  # 合并
  plot_df <- cd_map %>%
    left_join(decision_df %>% select(state_district, decision, ppvi), 
              by = "state_district")
  
  # --- 定义颜色 ---
  ppvi_colors <- c("R" = "#f4a6a6", "D" = "#a6c8f4")   # 底色红蓝
  decision_colors <- c("intl" = "#1f77b4", 
                       "dom"  = "#ff7f0e", 
                       "prof" = "#2ca02c")
  
  # 主图（底层ppvi + 上层decision半透明）
  main_map <- ggplot(plot_df) +
    # 底层 ppvi
    geom_sf(aes(fill = ppvi), color = "white", size = 0.1) +
    scale_fill_manual(
      values = ppvi_colors,
      na.value = "grey90",
      name = "PPVI"
    ) +
    ggnewscale::new_scale_fill() +   # 开启新的 fill 颜色映射
    
    # 上层 decision
    geom_sf(aes(fill = decision), color = NA, alpha = 0.6) +
    scale_fill_manual(
      values = decision_colors,
      na.value = "grey90",
      name = "Decision Mode"
    ) +
    labs(
      title = paste0(ifelse(model_type == "adver", "Adver", "Legis"),
                     " Decision Mode & PPVI (", congress_num, "th)")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = main_map, width = save_width, height = save_height)
  }
  
  return(main_map)
}


# core function3 ----

plot_decision_map_by_congress3 <- function(congress_num,
                                           decision_data,
                                           model_type = c("adver", "legis"),
                                           save_path = NULL,
                                           save_width = 10,
                                           save_height = 7) {
  model_type <- match.arg(model_type)
  
  # 地图年份
  map_year <- switch(
    as.character(congress_num),
    "115" = 2016,
    "116" = 2020,
    "117" = 2020,
    "118" = 2022,
    stop("Unsupported congress number.")
  )
  
  district_col <- switch(
    as.character(congress_num),
    "115" = "CD115FP",
    "116" = "CD116FP",
    "117" = "CD116FP",
    "118" = "CD118FP"
  )
  
  # 地图数据
  cd_map <- congressional_districts(cb = TRUE, year = map_year) %>%
    mutate(
      state_code = fips_to_state(STATEFP),
      district_code = as.character(!!rlang::sym(district_col)),
      district_clean = sprintf("%02d", as.integer(district_code)),
      state_district = paste0(state_code, "-", district_clean),
      state_district = stringr::str_trim(as.character(state_district))
    )
  
  # 决策数据
  decision_df <- decision_data %>%
    filter(congress == as.character(congress_num)) %>%
    mutate(state_district = stringr::str_trim(as.character(state_district)))
  
  # 合并
  plot_df <- cd_map %>%
    left_join(decision_df %>% select(state_district, decision, ppvi), 
              by = "state_district")
  
  # 定义颜色
  ppvi_colors <- c("R" = "#e41a1c", "D" = "#377eb8")   # 亮红/亮蓝
  decision_colors <- c("intl" = "#aec7e8",   # 淡蓝
                       "dom"  = "#ffbb78",   # 淡橙
                       "prof" = "#98df8a")   # 淡绿
  
  # 计算质心点
  centroids <- st_centroid(plot_df) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    bind_cols(plot_df %>% st_drop_geometry() %>% 
                select(state_district, ppvi))
  
  # 主图：底层 decision + 上层 ppvi 点
  main_map <- ggplot(plot_df) +
    geom_sf(aes(fill = decision), color = "white", size = 0.1) +
    scale_fill_manual(values = decision_colors, na.value = "grey90", name = "Decision Mode") +
    geom_point(data = centroids, aes(x = X, y = Y, color = ppvi), size = 2.2, alpha = 0.9) +
    scale_color_manual(values = ppvi_colors, na.value = "grey30", name = "PPVI") +
    labs(
      title = paste0(ifelse(model_type == "adver", "Adver", "Legis"),
                     " Decision Mode (background) with PPVI Points (", congress_num, "th)")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = main_map, width = save_width, height = save_height)
  }
  
  return(main_map)
}

# core_function4 ----

plot_decision_map_by_congress4 <- function(congress_num,
                                           decision_data,
                                           model_type = c("adver", "legis"),
                                           save_path = NULL,
                                           save_width = 10,
                                           save_height = 7) {
  model_type <- match.arg(model_type)
  
  # 地图年份对应
  map_year <- switch(
    as.character(congress_num),
    "115" = 2016,
    "116" = 2020,
    "117" = 2020,
    "118" = 2022,
    stop("Unsupported congress number.")
  )
  
  # district字段对应
  district_col <- switch(
    as.character(congress_num),
    "115" = "CD115FP",
    "116" = "CD116FP",
    "117" = "CD116FP",
    "118" = "CD118FP",
    stop("Unsupported congress number.")
  )
  
  # 载入地图数据
  cd_map <- congressional_districts(cb = TRUE, year = map_year) %>%
    mutate(
      state_code = fips_to_state(STATEFP),
      district_code = as.character(!!rlang::sym(district_col)),
      district_clean = sprintf("%02d", as.integer(district_code)),
      state_district = paste0(state_code, "-", district_clean),
      state_district = stringr::str_trim(as.character(state_district))
    )
  
  # 决策数据
  decision_df <- decision_data %>%
    filter(congress == as.character(congress_num)) %>%
    mutate(
      state_district = stringr::str_trim(as.character(state_district)),
      party = case_when(
        party %in% c("R","Republican","REP") ~ "R",
        party %in% c("D","Democratic","DEM") ~ "D",
        party %in% c("I","Independent","IND") ~ "I",
        TRUE ~ NA_character_
      ),
      decision = case_when(
        decision %in% c("intl","international") ~ "intl",
        decision %in% c("dom","domestic") ~ "dom",
        decision %in% c("prof","professional") ~ "prof",
        TRUE ~ NA_character_
      )
    )
  
  # 合并
  plot_df <- cd_map %>%
    left_join(decision_df %>% select(state_district, decision, party),
              by = "state_district") %>%
    mutate(
      party = as.character(party),
      decision = as.character(decision),
      group_var = ifelse(
        is.na(party) | is.na(decision) | party == "" | decision == "",
        "missing",
        paste0(party, "_", decision)
      )
    )
  
  # ----------- 自定义颜色映射 -----------
  pal_vec <- c(
    # R 系：红色不同深浅
    "R_intl"  = "#b2182b",  
    "R_dom"   = "#ef8a62",  
    "R_prof"  = "#fddbc7",  
    
    # I 系：紫色调
    "I_intl"  = "#756bb1",  
    "I_dom"   = "#9e9ac8",  
    "I_prof"  = "#dadaeb",  
    
    # D 系：蓝色不同深浅
    "D_intl"  = "#2166ac",  
    "D_dom"   = "#67a9cf",  
    "D_prof"  = "#d1e5f0",  
    
    # 缺失值
    "missing" = "grey80"
  )
  
  # 固定顺序：R > I > D > missing
  all_levels <- c("R_intl","R_dom","R_prof",
                  "I_intl","I_dom","I_prof",
                  "D_intl","D_dom","D_prof",
                  "missing")
  
  plot_df <- plot_df %>%
    mutate(group_var = factor(group_var, levels = all_levels))
  
  # 确保颜色映射顺序一致
  pal_vec <- pal_vec[all_levels]
  
  # 主图
  main_map <- ggplot(plot_df) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(
      values = pal_vec,
      drop = FALSE,
      name = "Party + Decision"
    ) +
    labs(
      title = paste0(
        ifelse(model_type == "adver", "Adver", "Legis"),
        " Decision Mode with Party by District (", congress_num, "th)"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)
  
  # Alaska
  alaska_map <- ggplot(filter(plot_df, state_code == "AK")) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(values = pal_vec, guide = "none") +
    coord_sf(xlim = c(-180, -130), ylim = c(50, 72), expand = FALSE) +
    theme_void()
  
  # Hawaii
  hawaii_map <- ggplot(filter(plot_df, state_code == "HI")) +
    geom_sf(aes(fill = group_var), color = "white", size = 0.1) +
    scale_fill_manual(values = pal_vec, guide = "none") +
    coord_sf(xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
    theme_void()
  
  # 合成图
  final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(main_map, 0, 0, 1, 1) +
    cowplot::draw_plot(alaska_map, x = 0.03, y = 0.15, width = 0.25, height = 0.25) +
    cowplot::draw_plot(hawaii_map, x = 0.20, y = 0.15, width = 0.25, height = 0.25)
  
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = final_map, width = save_width, height = save_height)
  }
  
  return(final_map)
}


# state function 1 ----

# ------------------ 参议员州级绘图函数（需要进一步debug） ------------------
plot_decision_map_by_congress_senate_debug <- function(congress_num,
                                                       decision_data,
                                                       model_type = c("adver", "legis"),
                                                       save_path = NULL,
                                                       save_width = 10,
                                                       save_height = 7,
                                                       shapefile_path = "cb_2020_us_state_20m/cb_2020_us_state_20m.shp") {
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(stringr)
  
  model_type <- match.arg(model_type)
  
  cat("???? 开始读取州地图...\n")
  states_map <- sf::st_read(shapefile_path, quiet = TRUE) %>%
    filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
    mutate(state_code = toupper(STUSPS))
  
  cat("州地图数据前几行:\n")
  print(head(states_map %>% st_drop_geometry() %>% select(STATEFP, STUSPS, state_code)))
  
  cat("???? 读取决策数据...\n")
  
  decision_df <- decision_data %>%
    filter(congress == as.character(congress_num)) %>%
    mutate(
      state_code = toupper(str_trim(as.character(state_code))),
      party = as.character(party),
      decision = as.character(decision)
    ) %>%
    group_by(state_code) %>%
    summarize(
      party = first(na.omit(party)),
      decision = first(na.omit(decision)),
      group_var = ifelse(
        is.na(party) | is.na(decision) | party == "" | decision == "",
        "missing",
        paste0(party, "_", decision)
      ),
      .groups = "drop"
    )
  
  cat("决策数据前几行:\n")
  print(head(decision_df))
  
  missing_states <- setdiff(decision_df$state_code, states_map$state_code)
  if (length(missing_states) > 0) {
    warning("这些 state_code 在地图数据中找不到: ", paste(missing_states, collapse = ", "))
  } else {
    cat("??? 所有 state_code 都匹配。\n")
  }
  
  cat("???? 计算州中心点...\n")
  state_centroids <- states_map %>%
    st_centroid() %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(state_code = states_map$state_code)
  
  cat("州中心点数据前几行:\n")
  print(head(state_centroids))
  
  cat("???? 合并决策数据和中心点...\n")
  plot_points <- decision_df %>%
    left_join(state_centroids, by = "state_code")
  
  cat("合并后数据前几行:\n")
  print(head(plot_points))
  
  if (nrow(plot_points) == 0) {
    stop("??? 合并后没有数据！请检查 state_code 是否一致。")
  }
  
  plot_points <- plot_points %>%
    group_by(state_code) %>%
    mutate(
      X = X + runif(n(), -0.3, 0.3),
      Y = Y + runif(n(), -0.3, 0.3)
    ) %>%
    ungroup()
  
  pal_vec <- c(
    "R_intl"  = "#b2182b", "R_dom"   = "#ef8a62", "R_prof"  = "#fddbc7",
    "I_intl"  = "#756bb1", "I_dom"   = "#9e9ac8", "I_prof"  = "#dadaeb",
    "D_intl"  = "#2166ac", "D_dom"   = "#67a9cf", "D_prof"  = "#d1e5f0",
    "missing" = "grey80"
  )
  
  all_levels <- c("R_intl","R_dom","R_prof",
                  "I_intl","I_dom","I_prof",
                  "D_intl","D_dom","D_prof",
                  "missing")
  
  plot_points <- plot_points %>%
    mutate(group_var = factor(group_var, levels = all_levels))
  
  pal_vec <- pal_vec[names(pal_vec) %in% levels(plot_points$group_var)]
  
  main_map <- ggplot() +
    geom_sf(data = states_map, fill = "white", color = "black", size = 0.2) +
    geom_point(data = plot_points,
               aes(x = X, y = Y, fill = group_var),
               shape = 21, size = 3, color = "black") +
    scale_fill_manual(values = pal_vec, drop = FALSE, name = "Party + Decision") +
    labs(
      title = paste0(ifelse(model_type == "adver", "Adver", "Legis"),
                     " Senate Decision Mode by State (", congress_num, "th)")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(-125, -65), ylim = c(24, 50), expand = FALSE)
  
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = main_map, width = save_width, height = save_height)
  }
  
  return(list(
    plot = main_map,
    states_map = states_map,
    decision_df = decision_df,
    state_centroids = state_centroids,
    plot_points = plot_points
  ))
}









# 调用 ----
# adver
plot_decision_map_by_congress4(115, adver_cg_heatmap, model_type = "adver",
                               save_path = "adver_115_map.png")

plot_decision_map_by_congress4(116, adver_cg_heatmap, model_type = "adver",
                              save_path = "adver_116_map.png")

plot_decision_map_by_congress4(117, adver_cg_heatmap, model_type = "adver",
                              save_path = "adver_117_map.png")

plot_decision_map_by_congress4(118, adver_cg_heatmap, model_type = "adver",
                              save_path = "adver_118_map.png")

# legis

plot_decision_map_by_congress4(115, legis_cg_heatmap, model_type = "legis",
                              save_path = "legis_115_map.png")

plot_decision_map_by_congress4(116, legis_cg_heatmap, model_type = "legis",
                              save_path = "legis_116_map.png")

plot_decision_map_by_congress4(117, legis_cg_heatmap, model_type = "legis",
                              save_path = "legis_117_map.png")

plot_decision_map_by_congress4(118, legis_cg_heatmap, model_type = "legis",
                              save_path = "legis_118_map.png")


# adver-state

plot_decision_map_by_congress_senate_debug(
  115,
  adver_cg_heatmap1,
  model_type = "adver",
  save_path = "adver_115_map_state.png",
  shapefile_path = "cb_2020_us_state_20m/cb_2020_us_state_20m.shp"
)

unique(states_map$state_code)
unique(adver_cg_heatmap1$state_code)

# map cleansing ----

# 1. 读取本地 shapefile
states_map <- st_read("cb_2020_us_state_20m/cb_2020_us_state_20m.shp")

# 2. 去掉不是州的区域（比如波多黎各、关岛等）
states_map <- states_map %>% 
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))

# 3. 改列名，把 STUSPS 作为 state_code
states_map <- states_map %>%
  mutate(state_code = STUSPS)

# 4. 合并 shapefile 和你的数据
plot_df <- states_map %>%
  left_join(adver_cg_heatmap1 %>% 
              select(state_code, decision, party),
            by = "state_code") %>%
  mutate(
    party = as.character(party),
    decision = as.character(decision),
    group_var = ifelse(
      is.na(party) | is.na(decision) | party == "" | decision == "",
      "missing",
      paste0(party, "_", decision)
    )
  )

# 5. 画图
ggplot(plot_df) +
  geom_sf(aes(fill = group_var), color = "white") +
  theme_minimal() +
  labs(fill = "Party & Decision")


