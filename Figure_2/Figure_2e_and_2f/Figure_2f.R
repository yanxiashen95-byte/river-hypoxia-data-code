#=========================== hypoxia_events_landuse =========================
#=========================== hypoxia_events_landuse =========================
#=========================== hypoxia_events_landuse =========================
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(scales)

in_csv  <- "hypoxia_day_landuse.csv"
df <- read_csv(in_csv, show_col_types = FALSE)

# -------------------------
# 1) 数据清洗：去掉 Global，严格剔除 0
# -------------------------
df_plot0 <- df %>%
  mutate(
    n_events = suppressWarnings(as.numeric(n_events)),
    landuse_group = as.character(landuse_group)
  ) %>%
  filter(!is.na(n_events), n_events > 0) %>%
  # 剔除 Global 组，只保留四大类
  filter(landuse_group %in% c("Natural-dominated", "Ag-dominated", "Urban-dominated", "Mixed"))

# -------------------------
# 2) 按中位数从小到大排序
# -------------------------
group_order <- df_plot0 %>%
  group_by(landuse_group) %>%
  summarise(med = median(n_events, na.rm = TRUE), .groups = "drop") %>%
  arrange(med)

ordered_levels <- group_order$landuse_group

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
# 3) 构建右半小提琴密度数据 (适配 log10 空间)
# -------------------------
y_upper_limit <- 100 # 定义封顶线
density_data <- tibble()

for (g in levels(df_plot$landuse_group)) {
  vals <- df_plot$n_events[df_plot$landuse_group == g]
  if (length(vals) < 5) next
  
  logv <- log10(vals)
  dens <- stats::density(logv, bw = 0.15, adjust = 1.2, na.rm = TRUE)
  
  base_x <- which(levels(df_plot$landuse_group) == g)
  x0     <- base_x + 0.25
  width  <- 0.42
  
  df_dens <- tibble(
    landuse_group = g,
    x = x0 + dens$y / max(dens$y) * width,
    # 核心：在这里把颜色数据的 Y 轴值封顶在 100
    y = pmin(10^(dens$x), y_upper_limit)
  )
  
  # 闭合多边形，底部和顶部也要封顶处理
  df_dens <- bind_rows(
    df_dens,
    tibble(landuse_group = g, x = x0, y = pmin(max(df_dens$y), y_upper_limit)),
    tibble(landuse_group = g, x = x0, y = min(df_dens$y))
  )
  density_data <- bind_rows(density_data, df_dens)
}

# -------------------------
# 4) 颜色与中位数标签
# -------------------------
group_colors <- c(
  "Natural-dominated" = "#deebf7",
  "Ag-dominated"      = "#4292c6",
  "Urban-dominated"   = "#9ecae1",
  "Mixed"             = "#084594"
)

stats_labels <- df_plot %>%
  group_by(landuse_group, group_num) %>%
  summarise(
    med   = median(n_events, na.rm = TRUE),
    lower = quantile(n_events, 0.25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label_text = sprintf("%.1f", med),
    # 在 log10 比例下，y 轴位置调整
    y_pos = lower / 1.5
  )

# -------------------------
# 5) 显著性检验：以 Undeveloped 为基准
# -------------------------
ref_group <- ordered_levels[1] # 排序后中位数最小的作为基准
other_groups <- ordered_levels[-1]

base_pairs <- tibble(group1 = ref_group, group2 = other_groups)

sig_df <- base_pairs %>%
  mutate(
    x1 = match(group1, ordered_levels),
    x2 = match(group2, ordered_levels)
  ) %>%
  rowwise() %>%
  mutate(
    p = wilcox.test(
      df_plot$n_events[df_plot$landuse_group == group1],
      df_plot$n_events[df_plot$landuse_group == group2],
      exact = FALSE
    )$p.value
  ) %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    sig_label = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ ""
    )
  )

# -------------------------
# 6) 显著性高度计算 (保持逻辑，微调参数)
# -------------------------
# 注意：即使数据被封顶，我们也基于 100 来计算括号高度
base_t  <- log10(100) + 0.2  # 从 100 往上一点点开始画
step_t  <- 0.2               # 括号间距
tip_t   <- 0.2              # 垂线向下扎的深度
star_t  <- 0.02              # 星号高度

sig_df <- sig_df %>%
  mutate(
    level    = row_number(),
    y_t      = base_t + (level - 1) * step_t,
    y_tip_t  = y_t - tip_t,
    star_y_t = y_t + star_t,
    y        = 10^y_t,
    y_tip    = 10^y_tip_t,
    star_y   = 10^star_y_t
  )

# -------------------------
# 7) 绘图 (关键修改点)
# -------------------------
p <- ggplot() +
  geom_polygon(data = density_data, aes(x = x, y = y, fill = landuse_group), alpha = 0.4) +
  geom_boxplot(data = df_plot, aes(x = group_num, y = n_events, fill = landuse_group),
               width = 0.3, outlier.shape = NA, linewidth = 0.8) +
  geom_text(data = stats_labels, aes(x = group_num, y = y_pos, label = label_text, color = landuse_group),
            size = 10, fontface = "bold", vjust = 1) +
  
  # 括号和星号
  geom_segment(data = sig_df, aes(x = x1, xend = x2, y = y, yend = y), linewidth = 0.8) +
  geom_segment(data = sig_df, aes(x = x1, xend = x1, y = y_tip, yend = y), linewidth = 0.8) +
  geom_segment(data = sig_df, aes(x = x2, xend = x2, y = y_tip, yend = y), linewidth = 0.8) +
  geom_text(data = sig_df %>% filter(sig_label != ""), 
            aes(x = (x1 + x2) / 2, y = star_y, label = sig_label), 
            size = 12, fontface = "bold") +
  
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = 1:4, labels = unname(label_map[ordered_levels])) +
  
  # 重要：不要在这里写 limits = c(0.1, 100)！！！
  scale_y_log10(
    breaks = c(0.01,0.05, 0.2, 0.5, 1, 2, 5, 50, 100),
    labels = c("0.01", "0.05", "0.2", "0.5", "1", "2", "5", "50", "100"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(0.1, 100), clip = "off") +
  
  # 重要：在这里写 ylim，控制黑框范围，并关闭剪裁
  coord_cartesian(ylim = c(0.01, 100), clip = "off") +
  
  labs(x = NULL, y = "hypoxia prevalence (%)") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.x  = element_text(size = 26, color = "black", angle = 8, hjust = 0.5),
    axis.text.y  = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 26),
    plot.margin  = margin(t = 80, r = 20, b = 10, l = 10) # 顶部留白
  )

print(p)

ggsave(
  filename = "hypoxia frequency.png", 
  plot = p, 
  width = 10, 
  height = 7, 
  dpi = 300,
  bg = "white"  # 确保背景是纯白而不是透明
)
