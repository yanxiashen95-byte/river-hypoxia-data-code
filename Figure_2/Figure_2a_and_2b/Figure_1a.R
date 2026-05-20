library(sf)
library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# 2. Read data
# =====================================================
in_csv  <- "hypoxia_duration_landuse.csv"

df <- read_csv(in_csv)

world <- st_read(
  "D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp",
  quiet = TRUE
)

# =====================================================
# 3. Convert to sf
# =====================================================
df_sf <- st_as_sf(
  df,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# =====================================================
# 4. 如果 fraction 是 0–1，转为百分比
# =====================================================
df_sf <- df_sf %>%
  mutate(
    # 确保变量名为数值型
    Med_Hrs = as.numeric(median_hours)
  ) %>%
  # 过滤掉缺失值
  filter(!is.na(Med_Hrs))

# =====================================================
# 5. 重新分级 (按你要求的 1-6, 6-12, 12-24, >24)
# =====================================================
df_sf <- df_sf %>%
  mutate(
    Hour_bin = case_when(
      Med_Hrs >= 0  & Med_Hrs <= 6  ~ "1–6",
      Med_Hrs > 6   & Med_Hrs <= 12 ~ "6–12",
      Med_Hrs > 12  & Med_Hrs <= 48 ~ "12–48",
      Med_Hrs > 48                 ~ "> 48",
      TRUE ~ NA_character_
    )
  )

# 🏆 关键：设置因子顺序，保证图例从短时间到长时间排列
df_sf$Hour_bin <- factor(
  df_sf$Hour_bin,
  levels = c("1–6", "6–12", "12–48", "> 48")
)

# =====================================================
# 7. Shape 设置 (补上缺失的这部分！)
# =====================================================
shape_values <- c(
  "Ag-dominated"      = 22, # 正方形
  "Natural-dominated" = 24, # 上三角形
  "Mixed"             = 21, # 圆形
  "Urban-dominated"   = 23  # 菱形
)

shape_labels <- c(
  "Ag-dominated"      = "Agriculture",
  "Natural-dominated" = "Undeveloped",
  "Mixed"             = "Mixed",
  "Urban-dominated"   = "Urban"
)

# 确保 landuse_group 是 factor 且水平对应
df_sf$landuse_group <- factor(
  df_sf$landuse_group,
  levels = c("Ag-dominated", "Natural-dominated", "Mixed", "Urban-dominated")
)

# =====================================================
# 8. 颜色分级 (0-3 mg/L)
# =====================================================
fill_values_hours <- c(
  "1–6"   = "#fdd49e", # 极浅粉（短时间）
  "6–12"  = "#fdbb84", # 浅红
  "12–48" = "#fc8d59", # 深粉
  "> 48"  = "#d7301f"  # 深紫（长时间/极端情况）
)

# =====================================================
# 9. 作图 (更新 fill 参数)
# =====================================================
p <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2) +
  
  geom_sf(
    # 🏆 排序：让持续时间最长的点（>24）叠在最上面，方便观察极端站点
    data = df_sf %>% arrange(Hour_bin),  
    aes(shape = landuse_group, fill = Hour_bin),
    size = 5.8,           
    color = "grey60",      
    stroke = 0.5
  ) +
  
  scale_shape_manual(values = shape_values, labels = shape_labels, name = "Land use") +
  
  # 🏆 颜色映射更新
  scale_fill_manual(
    values = fill_values_hours, 
    name = "hypoxia duration (h)"
  ) +
  
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  coord_sf(expand = FALSE) +
  
  guides(
    shape = guide_legend(override.aes = list(fill = "white", size = 5)),
    fill = guide_legend(override.aes = list(shape = 21, size = 5))
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 18, color = "black"), 
    legend.title = element_text(size = 12, fontface = "bold"),
    legend.text  = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p)

ggsave("global_map_hypoxia_duration.png", 
       p, width = 12, height = 6, dpi = 600, bg = "white")


#====================== 统计不同土地利用中的占比 
landuse_stats <- df_sf %>%
  group_by(landuse_group, Hour_bin) %>%
  tally() %>%
  group_by(landuse_group) %>%
  mutate(percentage = n / sum(n) * 100)

# 打印结果
print(landuse_stats)
