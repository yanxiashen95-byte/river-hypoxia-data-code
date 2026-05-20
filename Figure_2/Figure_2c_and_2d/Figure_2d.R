library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(scales)
library(tidyverse)

in_csv <- "hypoxia_intensity_landuse.csv" 
df <- read_csv(in_csv, show_col_types = FALSE)

# -------------------------
# 1) 数据清洗
# -------------------------
df_plot0 <- df %>%
  mutate(
    Hypoxic_Mean_DO = suppressWarnings(as.numeric(Hypoxic_Mean_DO)),
    landuse_group = as.character(landuse_group)
  ) %>%
  filter(!is.na(Hypoxic_Mean_DO), Hypoxic_Mean_DO >= 0) %>%
  filter(landuse_group %in% c("Natural-dominated", "Ag-dominated", "Urban-dominated", "Mixed"))

# -------------------------
# 2) 按中位数排序
# -------------------------
group_order <- df_plot0 %>%
  group_by(landuse_group) %>%
  summarise(med = median(Hypoxic_Mean_DO, na.rm = TRUE), .groups = "drop") %>%
  arrange(med)

ordered_levels <- group_order$landuse_group
label_map <- c("Natural-dominated" = "Undeveloped", "Ag-dominated" = "Agriculture", 
               "Urban-dominated" = "Urban", "Mixed" = "Mixed")

df_plot <- df_plot0 %>%
  mutate(
    landuse_group = factor(landuse_group, levels = ordered_levels),
    group_num = as.numeric(landuse_group)
  )

# -------------------------
# 3) 构建分段坐标变换 (放大 0-1, 压缩 1-3)
# -------------------------
cut_point <- 1.5  
a <- 2.0         # 0-1 放大倍数
b <- 2         # 1-3 压缩倍数

trans_piece <- scales::trans_new(
  name = "piece_log",
  transform = function(y) {
    y <- pmax(y, 0)
    lcut <- log10(cut_point)
    ifelse(y <= cut_point, a * y, a * cut_point + b * (log10(y) - lcut))
  },
  inverse = function(z) {
    zcut <- a * cut_point
    ly_cut <- log10(cut_point)
    ifelse(z <= zcut, z / a, 10^(ly_cut + (z - zcut) / b))
  }
)

# -------------------------
# 4) 构建右半小提琴密度数据 (应用分段变换)
# -------------------------
y_upper_limit <- 3.0
density_data <- tibble()
for (g in levels(df_plot$landuse_group)) {
  vals <- df_plot$Hypoxic_Mean_DO[df_plot$landuse_group == g]
  if (length(vals) < 5) next
  dens <- stats::density(vals, bw = "nrd0", adjust = 1.2, na.rm = TRUE)
  base_x <- which(levels(df_plot$landuse_group) == g)
  x0 <- base_x + 0.25
  width <- 0.42
  df_dens <- tibble(
    landuse_group = g,
    x = x0 + dens$y / max(dens$y) * width,
    y = pmin(dens$x, y_upper_limit)
  )
  df_dens <- bind_rows(df_dens,
                       tibble(landuse_group = g, x = x0, y = pmin(max(df_dens$y), y_upper_limit)),
                       tibble(landuse_group = g, x = x0, y = pmax(min(df_dens$y), 0))
  )
  density_data <- bind_rows(density_data, df_dens)
}

# -------------------------
# 5) 颜色、标签与显著性计算
# -------------------------
group_colors <- c("Natural-dominated" = "#e5f5e0", "Ag-dominated" = "#006d2c", 
                  "Urban-dominated" = "#a1d99b", "Mixed" = "#41ab5d")

stats_labels <- df_plot %>%
  group_by(landuse_group, group_num) %>%
  summarise(med = median(Hypoxic_Mean_DO, na.rm = TRUE),
            lower = quantile(Hypoxic_Mean_DO, 0.25, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_text = sprintf("%.2f", med), y_pos = lower - 0.12)

ref_group <- "Natural-dominated" 
other_groups <- setdiff(ordered_levels, ref_group)
sig_df <- tibble(group2 = other_groups) %>%
  mutate(group1 = ref_group, x1 = match(group1, ordered_levels), x2 = match(group2, ordered_levels)) %>%
  rowwise() %>%
  mutate(p = wilcox.test(df_plot$Hypoxic_Mean_DO[df_plot$landuse_group == group1],
                         df_plot$Hypoxic_Mean_DO[df_plot$landuse_group == group2], exact = FALSE)$p.value) %>%
  ungroup() %>%
  mutate(p_adj = p.adjust(p, method = "BH"),
         sig_label = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ ""))

# 计算括号在变换空间的高度
y_max_t <- trans_piece$transform(max(df_plot$Hypoxic_Mean_DO, na.rm = TRUE))
base_t <- y_max_t + 0.2
sig_df <- sig_df %>%
  mutate(level = row_number(), y_t = base_t + (level - 1) * 0.2,
         y = trans_piece$inverse(y_t), y_tip = trans_piece$inverse(y_t - 0.2),
         star_y = trans_piece$inverse(y_t + 0.10))

# -------------------------
# 6) 绘图 (补全了所有缺失层)
# -------------------------
p <- ggplot() +
  # 1. 右半小提琴
  geom_polygon(data = density_data, aes(x = x, y = y, fill = landuse_group), alpha = 0.4) +
  # 2. 盒图
  geom_boxplot(data = df_plot, aes(x = group_num, y = Hypoxic_Mean_DO, fill = landuse_group),
               width = 0.3, outlier.shape = NA, color = "black", linewidth = 0.8) +
  # 3. 中位数数字
  geom_text(data = stats_labels, aes(x = group_num, y = y_pos, label = label_text, color = landuse_group),
            size = 10, fontface = "bold", vjust = 1) +
  # 4. 显著性括号与星号
  geom_segment(data = sig_df, aes(x = x1, xend = x2, y = y, yend = y), linewidth = 1.0) +
  geom_segment(data = sig_df, aes(x = x1, xend = x1, y = y_tip, yend = y), linewidth = 1.0) +
  geom_segment(data = sig_df, aes(x = x2, xend = x2, y = y_tip, yend = y), linewidth = 1.0) +
  geom_text(data = sig_df %>% filter(sig_label != ""), 
            aes(x = (x1 + x2) / 2, y = star_y, label = sig_label), size = 16, fontface = "bold") +
  
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = 1:4, labels = unname(label_map[ordered_levels])) +
  scale_y_continuous(trans = trans_piece, breaks = c(0, 0.5, 1, 1.5, 3), 
                     labels = c("0", "0.5", "1", "1.5","3"),
                     expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 3.0), clip = "off") +
  labs(x = NULL, y = expression(paste("hypoxia intensity (mg/L)"))) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text.x = element_text(size = 26, color = "black", angle = 8, hjust = 0.5),
        axis.text.y = element_text(size = 26, color = "black"),
        axis.title.y = element_text(size = 26),
        plot.margin = margin(t = 80, r = 20, b = 10, l = 10))

print(p)

ggsave("Hypoxic_intensity.png", p, width = 10, height = 7, dpi = 300)