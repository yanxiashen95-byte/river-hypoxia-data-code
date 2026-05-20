# === 读取世界地图与站点数据 ===
world <- st_read("世界地图.shp")  # 修改为实际路径
df <- read_csv("global_hypoxia_with_coords.csv")      # 修改为实际路径

df <- df %>%
  mutate(
    category = ifelse(Class == 0, "non-hypoxia", "hypoxia")
  )

# === 转换为 sf 点图层 ===
df_sf <- st_as_sf(df, coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)

# 提取经纬度用于绘图（ggplot不直接支持 sf 点加填充）
df_pts <- df_sf %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  as.data.frame()

# === 设置颜色映射 ===
color_map <- c("non-hypoxia" = "#4575b4",      # 蓝色
               "hypoxia" = "#f46d43")   # 橙色

# === 绘图 ===
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70") +
  geom_point(
    data = df_pts,
    aes(x = lon, y = lat, fill = category),
    shape = 21,
    size = 3,
    color = "white",
    stroke = 0.1
  ) +
  scale_fill_manual(values = color_map, name = "Slope Type") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  labs(
    title = "Global Hypoxia Slope Classification",
    subtitle = ""
  )

ggsave("hyp.png",
       width = 14, height = 10, dpi = 600,
       bg = "white")

