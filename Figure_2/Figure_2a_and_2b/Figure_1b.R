#=========================== hypoxia_duration_landuse =========================
#=========================== hypoxia_duration_landuse =========================
#=========================== hypoxia_duration_landuse =========================
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(scales)
library(purrr)

in_csv  <- "hypoxia_duration_landuse.csv"
df <- read_csv(in_csv, show_col_types = FALSE)

# -------------------------
# 1) Clean data
# -------------------------
df_plot0 <- df %>%
  mutate(
    median_hours  = suppressWarnings(as.numeric(median_hours)),
    landuse_group = as.character(landuse_group)
  ) %>%
  filter(!is.na(median_hours), median_hours > 0) %>%
  filter(landuse_group %in% c(
    "Natural-dominated", "Ag-dominated", "Urban-dominated", "Mixed"
  ))

# -------------------------
# 2) 按中位数从小到大排序
# -------------------------
group_order <- df_plot0 %>%
  group_by(landuse_group) %>%
  summarise(med = median(median_hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(med)

ordered_levels <- group_order$landuse_group

# x 轴显示标签：按排序后的真实顺序自动匹配
label_map <- c(
  "Natural-dominated" = "Undeveloped",
  "Ag-dominated"      = "Agriculture",
  "Urban-dominated"   = "Urban",
  "Mixed"             = "Mixed"
)

df_plot <- df_plot0 %>%
  mutate(
    landuse_group = factor(landuse_group, levels = ordered_levels),
    group_num = as.numeric(landuse_group)
  )

# -------------------------
# 3) Build right-half density polygons
# -------------------------
density_data <- tibble()

for (g in levels(df_plot$landuse_group)) {
  vals <- df_plot$median_hours[df_plot$landuse_group == g]
  vals <- vals[is.finite(vals) & vals > 0]
  
  if (length(vals) < 10 || length(unique(vals)) < 2) next
  
  logv <- log10(vals)
  dens <- stats::density(logv, bw = 0.12, adjust = 1.0, na.rm = TRUE)
  
  base_x <- which(levels(df_plot$landuse_group) == g)
  x0     <- base_x + 0.25
  width  <- 0.42
  
  df_dens <- tibble(
    landuse_group = g,
    x = x0 + dens$y / max(dens$y) * width,
    y = 10^(dens$x)
  )
  
  df_dens <- bind_rows(
    df_dens,
    tibble(landuse_group = g, x = x0, y = max(df_dens$y)),
    tibble(landuse_group = g, x = x0, y = min(df_dens$y))
  )
  
  density_data <- bind_rows(density_data, df_dens)
}

# -------------------------
# 4) Piecewise transformation
# -------------------------
cut <- 72
a   <- 2.5
b   <- 0.4

trans_piece <- scales::trans_new(
  name = "piece_log10",
  transform = function(y) {
    y <- pmax(y, 1e-6)
    ly <- log10(y)
    lcut <- log10(cut)
    ifelse(y <= cut, a * ly, a * lcut + b * (ly - lcut))
  },
  inverse = function(z) {
    lcut <- log10(cut)
    ly <- ifelse(z <= a * lcut, z / a, lcut + (z - a * lcut) / b)
    10^ly
  }
)

# -------------------------
# 5) Colors
# -------------------------
group_colors <- c(
  "Natural-dominated" = "#fdd49e",
  "Ag-dominated"      = "#fd8d3c",
  "Urban-dominated"   = "#feb24c",
  "Mixed"             = "#ec7014"
)

# -------------------------
# 6) Median labels
# -------------------------
stats_labels <- df_plot %>%
  group_by(landuse_group, group_num) %>%
  summarise(
    med   = median(median_hours, na.rm = TRUE),
    lower = quantile(median_hours, 0.25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label_text = sprintf("%.1f", med),
    y_pos = lower / 1.8
  )

# -------------------------
# 7) 只做相邻组比较（更清爽）
# -------------------------
ref_group <- ordered_levels[1]
other_groups <- ordered_levels[-1]

# 构建以 Undeveloped 为核心的配对
base_pairs <- tibble(
  group1 = ref_group,
  group2 = other_groups
)

sig_df <- base_pairs %>%
  mutate(
    x1 = match(group1, ordered_levels),
    x2 = match(group2, ordered_levels)
  ) %>%
  rowwise() %>%
  mutate(
    # 进行 Wilcoxon 检验
    p = wilcox.test(
      df_plot$median_hours[df_plot$landuse_group == group1],
      df_plot$median_hours[df_plot$landuse_group == group2],
      exact = FALSE
    )$p.value
  ) %>%
  ungroup() %>%
  mutate(
    # 多重比较校正
    p_adj = p.adjust(p, method = "BH"),
    sig_label = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ ""
    )
  )

# -------------------------
# 8) 显著性括号放到框外边
# -------------------------
# 1. 找到所有数据（包含盒图和密度图）在变换后的最高点
y_data_max <- max(df_plot$median_hours, na.rm = TRUE)
y_max_t <- trans_piece$transform(y_data_max)

# 2. **关键修改**：定义一个远高于绘图区的“天花板高度”
# base_t 是显著性横线的起始高度
base_t  <- y_max_t + 0.45  # 原来是 0.10，大大调高
step_t  <- 0.27           # 增加多层括号之间的间距
tip_t   <- 0.3          # 垂线长度
star_t  <- 0.28          # 星号距离横线的垂直距离

sig_df <- sig_df %>%
  mutate(
    level    = row_number(),
    # 在变换后的空间里进行物理隔离
    y_t      = base_t + (level - 1) * step_t,
    y_tip_t  = y_t - tip_t,
    star_y_t = y_t + star_t,
    # 反向变换回原始坐标值，供 ggplot 绘图使用
    y        = trans_piece$inverse(y_t),
    y_tip    = trans_piece$inverse(y_tip_t),
    star_y   = trans_piece$inverse(star_y_t)
  )
# -------------------------
# 9) Plot
# -------------------------
p <- ggplot() +
  # 右半密度
  geom_polygon(
    data = density_data,
    aes(x = x, y = y, fill = landuse_group),
    alpha = 0.4,
    color = NA
  ) +
  # 盒图
  geom_boxplot(
    data = df_plot,
    aes(x = group_num, y = median_hours, group = group_num, fill = landuse_group),
    width = 0.32,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.9
  ) +
  # 中位数标签
  geom_text(
    data = stats_labels,
    aes(x = group_num, y = y_pos, label = label_text, color = landuse_group),
    size = 10,
    vjust = 1,
    fontface = "bold"
  ) +
  # 括号横线
  geom_segment(
    data = sig_df,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.9,
    color = "black"
  ) +
  # 左竖线
  geom_segment(
    data = sig_df,
    aes(x = x1, xend = x1, y = y_tip, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.9,
    color = "black"
  ) +
  # 右竖线
  geom_segment(
    data = sig_df,
    aes(x = x2, xend = x2, y = y_tip, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.9,
    color = "black"
  ) +
  # 只有显著的画星号
  geom_text(
    data = sig_df %>% filter(sig_label != ""),
    aes(x = (x1 + x2) / 2, y = star_y, label = sig_label),
    inherit.aes = FALSE,
    size = 12,
    fontface = "bold"
  ) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(
    name = NULL,
    breaks = 1:4,
    labels = unname(label_map[ordered_levels])
  ) +
  scale_y_continuous(
    trans  = trans_piece,
    breaks = c(1, 3, 6, 12, 24, 72),
    labels = c("1", "3", "6", "12", "24", "72"),
    # 扩展范围控制在顶部 5% 左右，防止轴显得太长
    expand = expansion(mult = c(0, 0.0))
  ) +
  labs(
    x = NULL,
    y = "hypoxia duration (h)"
  ) +
  coord_cartesian(ylim = c(NA, trans_piece$inverse(y_max_t + 0.1)), clip = "off") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    axis.text.x  = element_text(size = 26, color = "black", angle = 8, hjust = 0.5),
    axis.text.y  = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 26),
    # plot.margin 微调：核心！顶部至少留出 100 像素
    plot.margin  = margin(t = 70, r = 10, b = 10, l = 10) # 调大顶部边距
  )

print(p)

# 可选：查看显著性检验结果
print(sig_df)


ggsave(
  filename = "boxviolin_hypoxia_with_medians.png", 
  plot = p, 
  width = 10, 
  height = 7, 
  dpi = 300,
  bg = "white"  # 确保背景是纯白而不是透明
)
