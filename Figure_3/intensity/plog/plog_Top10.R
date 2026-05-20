library(readr)
library(dplyr)
library(ggplot2)

# =========================================
# 1. 读入重要性结果
# =========================================
importance_file <- "all_site.csv"
top_n <- 10

importance_summary <- read_csv(importance_file, show_col_types = FALSE) %>%
  rename_with(trimws)

# 计算百分比
importance_summary <- importance_summary %>%
  mutate(
    mean_gain_pct = mean_gain / sum(mean_gain, na.rm = TRUE) * 100,
    sd_gain_pct   = sd_gain / sum(mean_gain, na.rm = TRUE) * 100
  )

# =========================================
# 2. 变量全称映射
# =========================================
label_map <- c(
  "DO_P25" = "Baseline DO",
  "hdi_ix_cav" = "Human Dev Index",
  "solar_p75" = "Solar Radiation",
  "pre_mm_uyr" = "Mean Precip",
  "WT_p75" = "Water Temp",
  "slp_dg_uav" = "Stream Slope",
  "Q_p25" = "Discharge",
  "tmp_dc_uyr" = "Mean Air Temp",
  "ele_mt_cav" = "Mean Elevation",
  "rdd_mk_uav" = "Road Density",
  "UPLAND_SKM" = "Upstream Area",
  "ari_ix_uav" = "Aridity Index",
  "ppd_pk_uav" = "Population Density",
  "daylength_p75" = "Daylength",
  "for_pc_use" = "Forest Cover",
  "crp_pc_use" = "Watershed %Crop",
  "wet_pc_ug2" = "Wetland Cover",
  "ORD_STRA" = "Stream Order"
)

# =========================================
# 3. 四类变量分组
# =========================================
group_map <- c(
  "DO_P25" = "Hydroclimate",
  "solar_p75" = "Hydroclimate",
  "daylength_median" = "Hydroclimate",
  "pre_mm_uyr" = "Hydroclimate",
  "WT_p75" = "Hydroclimate",
  "Q_p25" = "Hydroclimate",
  "tmp_dc_uyr" = "Hydroclimate",
  "ari_ix_uav" = "Hydroclimate",
  "daylength_p75" = "Hydroclimate",
  
  "slp_dg_uav" = "Physiography",
  "ele_mt_cav" = "Physiography",
  "UPLAND_SKM" = "Physiography",
  "ORD_STRA" = "Physiography",
  
  "hdi_ix_cav" = "Human Activity",
  "rdd_mk_uav" = "Human Activity",
  "ppd_pk_uav" = "Human Activity",
  
  "for_pc_use" = "Land Cover",
  "crp_pc_use" = "Land Cover",
  "wet_pc_ug2" = "Land Cover"
)

group_colors <- c(
  "Hydroclimate" = "#b3cde3",
  "Physiography" = "#fed9a6",
  "Human Activity" = "#fbb4ae",
  "Land Cover" = "#ccebc5"
)

# =========================================
# 4. 整理作图数据
# =========================================
plot_df <- importance_summary %>%
  slice(1:min(top_n, n())) %>%
  mutate(
    Feature_label = recode(Feature, !!!label_map, .default = Feature),
    Group = recode(Feature, !!!group_map, .default = "Other")
  ) %>%
  arrange(desc(mean_gain_pct)) %>%
  mutate(
    Feature_label = factor(Feature_label, levels = rev(Feature_label)),
    rank_id = row_number(),
    label_y = if_else(rank_id <= 1, mean_gain_pct * 0.55, mean_gain_pct + 3),
    label_hjust = if_else(rank_id <= 1, 0.7, 0),
    label_color = if_else(rank_id <= 1, "black", "black")
  )

xmax <- max(plot_df$mean_gain_pct + plot_df$sd_gain_pct) + 4

# =========================================
# 5. 作图
# =========================================
p1 <- ggplot(
  plot_df,
  aes(
    x = Feature_label,
    y = mean_gain_pct,
    fill = Group
  )
) +
  geom_col(width = 0.75) +
  geom_errorbar(
    aes(
      ymin = pmax(mean_gain_pct - sd_gain_pct, 0),
      ymax = mean_gain_pct + sd_gain_pct
    ),
    width = 0.2,
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      y = label_y,
      label = Feature_label,
      hjust = label_hjust
    ),
    size = 7
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    limits = c(0, xmax),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Feature importance (Gain, %)",
    fill = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x  = element_text(size = 26),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin  = margin(t = 10, r = 80, b = 10, l = 10)
  )

print(p1)

ggsave(
  "AG_site_top10.png",
  p1, width = 9, height = 6.5, dpi = 300
)