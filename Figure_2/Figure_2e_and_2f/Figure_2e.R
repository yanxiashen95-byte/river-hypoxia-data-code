library(sf)
library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# 2. Read data
# =====================================================
in_csv  <- "hypoxia_day_landuse.csv"

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
    Events = as.numeric(n_events)
  ) %>%
  filter(!is.na(Events), Events >= 0) # 0 次也保留，作为基准

# =====================================================
# 5. 重新分级 (按你要求的 0-1, 1-10, 10-100, >100)
# =====================================================
df_sf <- df_sf %>%
  mutate(
    Event_bin = case_when(
      Events >= 0  & Events <= 1   ~ "0–1",    # 统一用 "0–1"
      Events > 1   & Events <= 20  ~ "1–20",
      Events > 20  & Events <= 50 ~ "20–50",
      Events > 50                 ~ "> 50",
      TRUE ~ NA_character_
    )
  )

# 🏆 必须重新设置因子顺序，否则图例会乱
df_sf$Event_bin <- factor(
  df_sf$Event_bin,
  levels = c("0–1", "1–20", "20–50", "> 50")
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
fill_values_events <- c(
  "0–1"      = "#deebf7", # 对应上面的 "0–1"
  "1–20"     = "#9ecae1", 
  "20–50"   = "#4292c6", 
  "> 50"    = "#084594"
)

# =====================================================
# 9. 作图
# =====================================================
p <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2) +
  
  # 🏆 排序：让大于 100 的点（最关键的）排在最上面
  geom_sf(
    data = df_sf %>% arrange(Event_bin),  
    aes(shape = landuse_group, fill = Event_bin),
    size = 5.8,           # 稍微调大点
    color = "grey60",      # 投 Nature 建议用黑边，点多了才不糊
    stroke = 0.7
  ) +
  
  scale_shape_manual(values = shape_values, labels = shape_labels, name = "Land use") +
  scale_fill_manual(values = fill_values_events, name = "hypoxia prevalence (%)") +
  
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
    axis.text = element_text(size = 18, color = "black"), # 调大坐标轴文字
    legend.title = element_text(size = 12, fontface = "bold"),
    legend.text  = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p)

ggsave("global_map_hypoxia_frequency.png", 
       p, width = 12, height = 6, dpi = 600, bg = "white")


#=================== 统计结果 =================== 
landuse_stats_events <- df_sf %>%
  filter(Events > 0) %>%  # 去掉n_events为0的数据
  group_by(landuse_group, Event_bin) %>%
  tally() %>%
  group_by(landuse_group) %>%
  mutate(percentage = n / sum(n) * 100)

# 打印结果
print(landuse_stats_events)