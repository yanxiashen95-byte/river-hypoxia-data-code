#============================ 土地利用类型合并 =======================
#============================ 土地利用类型合并 =======================
#============================ 土地利用类型合并 =======================
#============================ 土地利用类型合并 =======================

library(terra)

# =========================
# 0) 路径
# =========================
in_tif  <- "HYBMAP_IGBP_2020_LC.tif"
out_tif <- "HYBMAP_IGBP_2020_LC_5class.tif"

# =========================
# 1) 读栅格
# =========================
r <- rast(in_tif)

# =========================
# 2) 1–17 -> 5类重分类
#    输出编码：1=Natural, 2=Cropland, 3=Urban, 4=Wetland, 5=Other
# =========================
# IGBP codes:
# 1-10 Natural vegetation
# 11 Natural
# 12 Cropland
# 13 Urban
# 14 Cropland/Natural mosaic  -> Cropland
# 15 Snow/Ice -> Other
# 16 Barren   -> Other
# 17 Water    -> Other

m <- matrix(c(
  1,  1,
  2,  1,
  3,  1,
  4,  1,
  5,  1,
  6,  1,
  7,  1,
  8,  1,
  9,  1,
  10, 1,
  11, 1,
  12, 2,
  13, 3,
  14, 2,
  15, 4,
  16, 4,
  17, 4
), ncol = 2, byrow = TRUE)

r5 <- classify(r, m, others = NA)

# =========================
# 3) 写出结果（保持整型）
# =========================
r5 <- as.int(r5)
writeRaster(r5, out_tif, overwrite = TRUE)

# （可选）加一个“类名表”，方便你后续画图/统计
levels(r5) <- data.frame(
  value = 1:4,
  class = c("Natural vegetation", "Cropland", "Urban", "Other")
)

print(r5)
cat("Done! 输出：", out_tif, "\n")

#======================= 提取土地利用 ===============================
#======================= 提取土地利用 ===============================
#======================= 提取土地利用 ===============================

# install.packages(c("terra","dplyr","readr"))
library(terra)
library(readr)
library(dplyr)

# =========================
# 0) 路径（改成你的）
# =========================
site_csv <- "lat_lon.csv"  # 你的站点表：file_name, latitude, longitude
lc5_tif  <- "HYBMAP_IGBP_2020_LC_5class.tif"  # 5类土地利用栅格
out_csv  <- "sites_landcover_5class.csv"

# 缓冲半径（米）：建议 1000~5000 m，先用 2000 m
buf_m <- 2000

# =========================
# 1) 读数据
# =========================
sites <- read_csv(site_csv, show_col_types = FALSE)

# 统一列名（你图里是 latitude/longitude）
stopifnot(all(c("file_name","latitude","longitude") %in% names(sites)))

r5 <- rast(lc5_tif)

# 类别映射：1=Natural,2=Cropland,3=Urban,4=Wetland,5=Other
lc5_map <- tibble(
  lc5 = 1:4,
  lc5_name = c("Natural vegetation", "Cropland", "Urban", "Other")
)

# =========================
# 2) 站点转 SpatVector (WGS84)
# =========================
pts <- vect(
  sites,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326",
  keepgeom = TRUE
)

# =========================
# 3) A. 单像元提取（最快）
# =========================
v_point <- terra::extract(r5, pts)  # 返回 ID + 栅格值
# v_point[[2]] 是栅格值列（通常叫你 tif 的名字）
lc_val <- v_point[[2]]

sites2 <- sites %>%
  mutate(lc5_point = lc_val) %>%
  left_join(lc5_map, by = c("lc5_point" = "lc5"))

# =========================
# 4) B. 缓冲区主导类型 + 各类占比（推荐）
#     关键：要用米为单位的投影坐标系做 buffer
# =========================
# 用 Web Mercator（EPSG:3857）做缓冲（单位米），够用且方便
pts_m <- project(pts, "EPSG:3857")
r5_m  <- project(r5, "EPSG:3857", method = "near")

buf <- buffer(pts_m, width = buf_m)

# 提取缓冲区内所有像元类别（会比较大，但一般站点数量不爆炸就OK）
v_buf <- terra::extract(r5_m, buf)

# v_buf 格式：ID + value（值列名与栅格层名有关）
val_col <- names(v_buf)[2]

# 统计：主导类型（众数）+ 各类占比
buf_stat <- v_buf %>%
  as_tibble() %>%
  filter(!is.na(.data[[val_col]])) %>%
  mutate(lc5 = .data[[val_col]]) %>%
  group_by(ID, lc5) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()

# 主导类型
dom_tbl <- buf_stat %>%
  group_by(ID) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    ID,
    lc5_dom = lc5
  ) %>%
  left_join(lc5_map, by = c("lc5_dom" = "lc5")) %>%
  rename(lc5_dom_name = lc5_name)

# 各类占比（宽表）
p_tbl <- buf_stat %>%
  select(ID, lc5, p) %>%
  tidyr::pivot_wider(
    names_from = lc5,
    values_from = p,
    values_fill = 0
  ) %>%
  rename(
    p_natural = `1`,
    p_crop    = `2`,
    p_urban   = `3`,
    p_other   = `4`
  )

# 合并回站点表（ID=行号）
sites3 <- sites2 %>%
  mutate(ID = row_number()) %>%
  left_join(dom_tbl, by = "ID") %>%
  left_join(p_tbl,  by = "ID") %>%
  select(-ID)

# 缓冲区字段加后缀，避免你以后跑不同半径混淆
suffix <- paste0("_", buf_m/1000, "km")
sites3 <- sites3 %>%
  rename_with(~paste0(.x, suffix),
              .cols = c(lc5_dom, lc5_dom_name,
                        p_natural, p_crop, p_urban, p_other))

# =========================
# 5) 输出
# =========================
write_csv(sites3, out_csv)
cat("Done! 输出：", out_csv, "\n")

#================== 再分类 ========================
library(readr)
library(dplyr)

in_csv  <- "sites_landcover_5class.csv"
out_csv <- "your_sites_with_landuse_group.csv"

TH <- 0.30  # 你选的阈值

df <- read_csv(in_csv, show_col_types = FALSE)

df2 <- df %>%
  mutate(
    # 确保是数值
    p_urban_2km   = as.numeric(p_urban_2km),
    p_crop_2km    = as.numeric(p_crop_2km),
    p_natural_2km = as.numeric(p_natural_2km),
    p_wetland_2km = as.numeric(p_wetland_2km),
    p_other_2km   = as.numeric(p_other_2km),
    
    # 1) 人为影响：城市+农业 是否超过阈值
    human_share   = p_urban_2km + p_crop_2km,
    human_impacted = ifelse(is.na(human_share), NA, human_share > TH),
    
    # 2) 论文友好分组：谁占比最大 + 是否“明显主导”
    landuse_group = case_when(
      # 缺值兜底
      is.na(p_urban_2km) | is.na(p_crop_2km) | is.na(p_natural_2km) ~ NA_character_,
      
      # 主导类：最大者 >= 0.5（你也可以改成 0.4/0.6）
      p_urban_2km   >= 0.5 & p_urban_2km   >= p_crop_2km & p_urban_2km   >= p_natural_2km ~ "Urban-dominated",
      p_crop_2km    >= 0.5 & p_crop_2km    >= p_urban_2km & p_crop_2km    >= p_natural_2km ~ "Ag-dominated",
      p_natural_2km >= 0.5 & p_natural_2km >= p_urban_2km & p_natural_2km >= p_crop_2km ~ "Natural-dominated",
      
      # 否则就是混合
      TRUE ~ "Mixed"
    )
  )

write_csv(df2, out_csv)
message("Done! saved to: ", out_csv)