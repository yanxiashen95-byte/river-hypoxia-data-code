
library(sf)
library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# 2. Read data
# =====================================================
in_csv  <- "hypoxia_intensity_landuse.csv"

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
    # 确保是数值型
    Mean_DO = as.numeric(Hypoxic_Mean_DO)
  ) %>%
  filter(!is.na(Mean_DO), Mean_DO >= 0)

# =====================================================
# 5. 分级：针对 0-3 mg/L 进行科学划分
# =====================================================
# 建议划分：
# 0-0.5 (极重度缺氧), 0.5-1.0 (重度缺氧), 1.0-2.0 (中度), 2.0-3.0 (轻度)
df_sf <- df_sf %>%
  mutate(
    DO_bin = case_when(
      Mean_DO >= 0   & Mean_DO <= 0.5 ~ "0.0–0.5",
      Mean_DO > 0.5  & Mean_DO <= 1.0 ~ "0.5–1.0",
      Mean_DO > 1.0  & Mean_DO <= 2.0 ~ "1.0–2.0",
      Mean_DO > 2.0  & Mean_DO <= 3.0 ~ "2.0–3.0",
      TRUE ~ "> 3.0" # 防止溢出
    )
  )

# 固定因子顺序 (从低到高，颜色对应从深到浅或反之)
df_sf$DO_bin <- factor(
  df_sf$DO_bin,
  levels = c("0.0–0.5", "0.5–1.0", "1.0–2.0", "2.0–3.0")
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
fill_values_do <- c(
  "0.0–0.5" = "#e5f5e0", # 深红
  "0.5–1.0" = "#a1d99b", # 红
  "1.0–2.0" = "#41ab5d", # 浅红
  "2.0–3.0" = "#006d2c"  # 极浅
)

# =====================================================
# 9. 作图
# =====================================================
p <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2) +
  
  # 为了防止点太多互相遮盖，建议按 DO_bin 降序排列，让严重缺氧的点排在上面
  geom_sf(
    data = df_sf %>% arrange(desc(DO_bin)), 
    aes(shape = landuse_group, fill = DO_bin),
    size = 5.8, 
    color = "grey60",
    stroke = 0.7
  ) +
  
  scale_shape_manual(
    values = shape_values, 
    labels = shape_labels, 
    name = "Land use"
  ) +
  
  scale_fill_manual(
    values = fill_values_do,
    name = "hypoxia intensity (mg/L)"
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
    # 🏆 核心修改：调整横纵坐标数字大小
    axis.text.x = element_text(size = 18, color = "black"), # 横坐标数字
    axis.text.y = element_text(size = 18, color = "black"), # 纵坐标数字
    legend.title = element_text(size = 12, fontface = "bold"),
    legend.text  = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p)


ggsave("global_map_hypoxia_intensity.png", 
       p, width = 12, height = 6, dpi = 600, bg = "white")

#================= 计算全球和不同土地利用的占比
# 统计每类土地利用中，不同DO分级的占比
landuse_stats_do <- df_sf %>%
  group_by(landuse_group, DO_bin) %>%
  tally() %>%
  group_by(landuse_group) %>%
  mutate(percentage = n / sum(n) * 100)

# 打印结果
print(landuse_stats_do)


global_stats_do <- df_sf %>%
  group_by(DO_bin) %>%
  tally() %>%
  mutate(percentage = n / sum(n) * 100)

# 打印结果
print(global_stats_do)
