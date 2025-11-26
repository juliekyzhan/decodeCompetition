
# 汇总每届国会各 chamber 的议员数量
legis_cg_summary <- legis_cg %>%
  filter(chamber %in% c("House of Representatives", "Senate")) %>%
  group_by(congress, chamber) %>%
  summarise(count = n(), .groups = "drop")

# 查看结果
print(legis_cg_summary)

adver_cg_summary <- adver_cg %>%
  filter(chamber %in% c("House of Representatives", "Senate")) %>%
  group_by(congress, chamber) %>%
  summarise(count = n(), .groups = "drop")

# 查看结果
print(adver_cg_summary)


# ---- 计算age_to_date & age_to_congress ----
congress_df <- congress_2025.10.23
names(congress_df)

# 从 congress_df 中提取唯一的基本信息字段
congress_basic <- congress_df %>%
  select(
    bioguide_id, full_name, direct_order_name, chamber, member_type,
    party, state_name, state_code, district, gender,
    birth_year, death_year, age_at_death, district_congress
  ) %>%
  distinct()   # 去除重复记录

# 查看结果前几行
head(congress_basic)
names(congress_basic)

# 检查唯一数量
cat("提取后的唯一议员记录数：", nrow(congress_basic), "\n")


# 计算

# === 定义每届国会的开幕年份 ===
congress_year_map <- tibble(
  district_congress = c(115, 116, 117, 118),
  congress_start_year = c(2019, 2021, 2023, 2025)
)

# === 计算年龄 ===
congress_basic <- congress_basic %>%
  # 合并国会年份映射表
  left_join(congress_year_map, by = "district_congress") %>%
  
  # 计算年龄
  mutate(
    age_to_date = 2025 - birth_year,
    age_to_congress = congress_start_year - birth_year
  ) %>%
  
  # 若缺失年份则设为 NA
  mutate(
    age_to_date = if_else(!is.na(birth_year), age_to_date, NA_real_),
    age_to_congress = if_else(!is.na(birth_year) & !is.na(congress_start_year),
                              age_to_congress, NA_real_)
  )

# === 检查结果 ===
cat("??? 年龄计算完成。\n")
print(congress_basic %>%
        select(full_name, district_congress, birth_year, age_to_congress, age_to_date) %>%
        head())

# === 筛选 full_name 缺失的议员 ===
missing_name_df <- congress_basic %>%
  filter(is.na(full_name) | full_name == "") %>%   # 筛出full_name为空或NA
  select(bioguide_id, direct_order_name, chamber, state_code, district, party, district_congress) %>%
  arrange(chamber, state_code, district)

write.csv(missing_name_df, "missing_name_origin.csv", row.names = FALSE)



# === 建立full_name缺失议员的映射表 ===

# 创建 direct_order_name 与 full_name 的映射表
name_map <- data.frame(
  direct_order_name = c(
    "Mike Rogers",
    "J. French Hill",
    "Dave Min",
    "Cory Mills",
    "Marjorie Taylor Greene",
    "David P. Joyce",
    "Michael Baumgartner",
    "Mark  Begich " ,
    "Richard C. Shelby",
    "Mark Udall",
    "Chuck Grassley",
    "Daniel Coats",
    "Joe Donnelly",
    "Pat Roberts",
    "Rand Paul",
    "David Vitter",
    "Carl Levin",
    "Al  Franken ",
    "Claire McCaskill",
    "Richard Burr",
    "John Hoeven",
    "Heidi  Heitkamp",
    "Kelly Ayotte",
    "Dean Heller",
    "Rob Portman",
    "Ron Wyden",
    "Jack Reed",
    "Sheldon Whitehouse",
    "Lindsey Graham",
    "Tim Johnson",
    "Bob Corker"
  ),
  full_name = c(
    "Rep. Rogers, Mike [R-AL-3]",
    "Rep. Hill, J. French [R-AR-2]",
    "Rep. Min, Dave [D-CA-47]",
    "Rep. Mills, Cory [R-FL-7]",
    "Rep. Greene, Marjorie Taylor [R-GA-14]",
    "Rep. Joyce, David P. [R-OH-14]",
    "Rep. Baumgartner, Michael [R-WA-5]",
    "Sen. Begich, Mark [D-AK]",
    "Sen. Shelby, Richard C. [R-AL]",
    "Sen. Udall, Mark [D-CO]",
    "Sen. Grassley, Charles E. [R-IA]",
    "Sen. Coats, Daniel R. [R-IN]",
    "Sen. Donnelly, Joe [D-IN]",
    "Sen. Roberts, Patrick J. [R-KS]",
    "Sen. Paul, Randal H. [R-KY]",
    "Sen. Vitter, David [R-LA]",
    "Sen. Levin, Carl [D-MI]",
    "Sen. Franken, Al [D-MN]",
    "Sen. McCaskill, Claire [D-MO]",
    "Sen. Burr, Richard [R-NC]",
    "Sen. Hoeven, John [R-ND]",
    "Sen. Heitkamp, Heidi [D-ND]",
    "Sen. Ayotte, Kelly [R-NH]",
    "Sen. Heller, Dean [R-NV]",
    "Sen. Portman, Robert J. [R-OH]",
    "Sen. Wyden, Ronald [D-OR]",
    "Sen. Reed, Jack [D-RI]",
    "Sen. Whitehouse, Sheldon [D-RI]",
    "Sen. Graham, Lindsey [R-SC]",
    "Sen. Johnson, Timothy M. [D-SD]",
    "Sen. Corker, Robert P. [R-TN]"
  ),
  stringsAsFactors = FALSE
)

# missing_df测试

missing_name_df <- missing_name_df %>%
  left_join(name_map, by = "direct_order_name")



#  用这个映射表填补 congress_basic 中的 full_name 缺失值


congress_basic <- congress_basic %>%
  mutate(full_name = na_if(full_name, "")) %>%       # 把 "" 变 NA
  mutate(full_name = na_if(full_name, "NA")) %>%     # 把 "NA" 变 NA
  mutate(direct_order_name_clean = clean_names(direct_order_name)) %>%
  left_join(
    missing_name_df %>% 
      mutate(direct_order_name_clean = clean_names(direct_order_name)) %>%
      select(direct_order_name_clean, full_name) %>% distinct(),
    by = "direct_order_name_clean"
  ) %>%
  mutate(full_name = ifelse(is.na(full_name.x), full_name.y, full_name.x)) %>%
  select(-ends_with(".x"), -ends_with(".y"), -direct_order_name_clean)

# Step 1: 标准化两边的名字列，防止空格问题
clean_names <- function(x) {
  x %>%
    stringr::str_replace_all("\\p{Zs}+", " ") %>%
    stringr::str_squish() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
}

congress_basic <- congress_basic %>%
  mutate(direct_order_name_clean = clean_names(direct_order_name))

missing_name_df <- missing_name_df %>%
  mutate(direct_order_name_clean = clean_names(direct_order_name))

# Step 2: 用 missing_name_ft 的 full_name 填补 congress_basic 中缺失值
congress_basic <- congress_basic %>%
  left_join(
    missing_name_df %>% select(direct_order_name_clean, full_name) %>% distinct(),
    by = "direct_order_name_clean"
  ) %>%
  mutate(full_name = ifelse(is.na(full_name.x), full_name.y, full_name.x)) %>%
  select(-ends_with(".x"), -ends_with(".y"), -direct_order_name_clean)


sum(is.na(congress_basic$full_name))  # name cleansing should be done by this line


# 清洗district
congress_basic <- congress_basic %>%
  mutate(
    district = as.character(district),  # 先确保是字符型
    district = case_when(
      is.na(district) & chamber == "Senate" ~ "99",
      is.na(district) & chamber == "House of Representatives" ~ "At Large",
      TRUE ~ district
    )
  )




