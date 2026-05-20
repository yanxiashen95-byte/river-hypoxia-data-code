#=========================== hypoxia_occurance_landuse_世界地图 =========================
#=========================== hypoxia_occurance_landuse_世界地图 =========================
#=========================== hypoxia_occurance_landuse_世界地图 =========================

# =====================================================
# 1. Load packages
# =====================================================
library(sf)
library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# 2. Read data
# =====================================================
in_csv  <- "hypoxia_occurance_landuse.csv"

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
    hypoxia_pct = hypoxia_fraction
  )

# =====================================================
# 5. 分级（注意这里要用 hypoxia_pct，不是 hypoxia_fraction）
# =====================================================
df_sf <- df_sf %>%
  mutate(
    hypoxia_bin = case_when(
      hypoxia_pct == 0 ~ "No hypoxia",
      hypoxia_pct > 0  & hypoxia_pct <= 5  ~ "0–5",
      hypoxia_pct > 5 & hypoxia_pct <= 10  ~ "5–10",
      hypoxia_pct > 10 & hypoxia_pct <= 20  ~ "10–20",
      hypoxia_pct > 20 & hypoxia_pct <= 100 ~ "20–100",
      TRUE ~ NA_character_
    )
  )

df_sf$hypoxia_bin <- factor(
  df_sf$hypoxia_bin,
  levels = c("No hypoxia", "0–5", "5–10", "10–20", "20–100")
)

# =====================================================
# 6. 土地利用分组顺序（可选，但建议固定）
# =====================================================
df_sf$landuse_group <- factor(
  df_sf$landuse_group,
  levels = c("Ag-dominated", "Natural-dominated", "Mixed", "Urban-dominated")
)

# =====================================================
# 7. Shape 设置
# =====================================================
shape_values <- c(
  "Ag-dominated"      = 22,
  "Natural-dominated" = 24,
  "Mixed"             = 21,
  "Urban-dominated"   = 23
)

shape_labels <- c(
  "Ag-dominated"      = "AG",
  "Natural-dominated" = "NT",
  "Mixed"             = "MX",
  "Urban-dominated"   = "UB"
)

# =====================================================
# 8. 颜色分级
# =====================================================
fill_values <- c(
  "No hypoxia" = "#4575b4",
  "0–5"       = "#fee8c8",
  "5–10"      = "#fdbb84",
  "10–20"      = "#fc8d59",
  "20–100"     = "#d7301f"
)

# =====================================================
# 9. 作图
# =====================================================
p <- ggplot() +
  
  geom_sf(
    data = world,
    fill = "grey95",
    color = "grey70",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = df_sf,
    aes(shape = landuse_group, fill = hypoxia_bin),
    size = 4,
    color = "grey70",
    stroke = 0.3
  ) +
  
  scale_shape_manual(
    values = shape_values,
    labels = shape_labels,
    name = "Land use"
  ) +
  
  scale_fill_manual(
    values = fill_values,
    name = "Hypoxia (%)"
  ) +
  
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30)
  ) +
  
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60)
  ) +
  
  coord_sf(expand = FALSE) +
  
  guides(
    shape = guide_legend(
      override.aes = list(fill = "white", color = "black", size = 3)
    ),
    fill = guide_legend(
      override.aes = list(shape = 21, color = "black", size = 3)
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    axis.text = element_text(size = 15, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p)

ggsave(
  filename = "Map_landuse_hypoxia.png",
  plot     = p,
  width    = 10,
  height   = 6,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)


#=========================== 画缺氧和非缺氧的占比图 ========================
#=========================== 画缺氧和非缺氧的占比图 ========================
#=========================== 画缺氧和非缺氧的占比图 ========================
library(dplyr)
library(ggplot2)
library(binom)

# 1. 计算每类土地利用中缺氧站点比例
summary_df <- df %>%
  mutate(
    hypoxic = if_else(hypoxia_fraction > 0, 1, 0)
  ) %>%
  group_by(landuse_group) %>%
  summarise(
    n = n(),
    hypoxic_n = sum(hypoxic),
    prop = hypoxic_n / n,
    .groups = "drop"
  )

# 2. 计算 95% CI
ci <- binom.confint(summary_df$hypoxic_n, summary_df$n, methods = "wilson")

summary_df <- bind_cols(
  summary_df,
  ci %>% select(lower, upper)
)

# 3. 固定顺序：Natural → Mixed → Ag → Urban
summary_df$landuse_group <- factor(
  summary_df$landuse_group,
  levels = c("Natural-dominated", "Mixed", "Ag-dominated", "Urban-dominated")
)

# 4. 作图
p2 <- ggplot(summary_df, aes(x = landuse_group, y = prop * 100)) +
  geom_col(
    width = 0.65,
    fill = "#fdae61",
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = lower * 100, ymax = upper * 100),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(prop * 100, 1), "%\n(n=", n, ")")),
    vjust = -0.8,
    size = 6
  ) +
  scale_x_discrete(
    labels = c(
      "Natural-dominated" = "Undeveloped",
      "Mixed" = "Mixed",
      "Ag-dominated" = "Agriculture",
      "Urban-dominated" = "Urban"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Hypoxic sites (%)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 20, angle = 0, hjust = 0.5),
    axis.text.y  = element_text(size = 20),
    legend.position = "none",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )
print(p2)

ggsave(
  filename = "pro_landuse_hypoxia.png",
  plot     = p2,
  width    = 7,
  height   = 5,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)
