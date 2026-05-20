library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(scales)

# -------------------------
# 0) Settings & Data
# -------------------------
in_csv <- "hypoxia_onset_landuse.csv"

df <- read_csv(in_csv, show_col_types = FALSE)

# 文字大小你自己改这里
label_size <- 10

# -------------------------
# 1) Clean & Add Global Group
# -------------------------
df_clean <- df %>%
  mutate(
    start_median_shift = suppressWarnings(as.numeric(start_median_shift)),
    landuse_group = as.character(landuse_group)
  ) %>%
  filter(!is.na(start_median_shift), start_median_shift >= 12, start_median_shift <= 36)

df_global <- df_clean %>%
  mutate(landuse_group = "Global")

target_levels <- c("Global", "Natural-dominated", "Ag-dominated", "Urban-dominated", "Mixed")

df_use <- bind_rows(df_global, df_clean) %>%
  mutate(
    landuse_group = factor(landuse_group, levels = target_levels)
  ) %>%
  filter(!is.na(landuse_group)) %>%
  mutate(group_num = as.numeric(landuse_group))

# -------------------------
# 2) Build density polygons
# -------------------------
density_data <- tibble()

for (i in seq_along(target_levels)) {
  g <- target_levels[i]
  vals <- df_use$start_median_shift[df_use$landuse_group == g]
  vals <- vals[is.finite(vals)]
  
  if (length(vals) < 5) next
  
  dens <- stats::density(
    vals,
    bw = 1.0,
    adjust = 1.0,
    na.rm = TRUE,
    from = 12,
    to = 36
  )
  
  x0 <- i + 0.23
  width <- 0.42
  
  df_dens <- tibble(
    landuse_group = factor(g, levels = target_levels),
    x = x0 + (dens$y / max(dens$y)) * width,
    y = dens$x
  ) %>%
    filter(y >= 12, y <= 36)
  
  df_dens <- bind_rows(
    df_dens,
    tibble(landuse_group = factor(g, levels = target_levels), x = x0, y = max(df_dens$y)),
    tibble(landuse_group = factor(g, levels = target_levels), x = x0, y = min(df_dens$y))
  )
  
  density_data <- bind_rows(density_data, df_dens)
}

if (nrow(density_data) == 0) {
  stop("错误：density_data 为空，请检查数据样本量是否足够计算密度曲线。")
}

# -------------------------
# 3) 计算统计量 + 中位数标注
#    标在 0.25 分位数位置
# -------------------------
shift_to_clock_label <- function(x) {
  x2 <- ifelse(x >= 24, x - 24, x)
  h <- floor(x2)
  m <- round((x2 - h) * 60)
  
  if (m == 60) {
    h <- h + 1
    m <- 0
  }
  
  h <- h %% 24
  sprintf("%02d:%02d", h, m)
}

stats_labels <- df_use %>%
  group_by(landuse_group, group_num) %>%
  summarise(
    med = median(start_median_shift, na.rm = TRUE),
    q1  = quantile(start_median_shift, 0.25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label_text = sapply(med, shift_to_clock_label),
    x_pos = group_num,
    y_pos = q1-0.8
  )

# -------------------------
# 4) 颜色与绘图
# -------------------------
group_colors <- c(
  "Global" = "#d9d9d9",
  "Natural-dominated" = "#66C2A5",
  "Ag-dominated" = "#FFD92F",
  "Urban-dominated" = "#FC8D62",
  "Mixed" = "#8DA0CB"
)

p <- ggplot() +
  geom_polygon(
    data = density_data,
    aes(x = x, y = y, fill = landuse_group),
    alpha = 0.45,
    color = NA
  ) +
  geom_boxplot(
    data = df_use,
    aes(x = group_num, y = start_median_shift, fill = landuse_group),
    width = 0.32,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.85
  ) +
  geom_text(
    data = stats_labels,
    aes(x = x_pos, y = y_pos, label = label_text, color = landuse_group),
    size = label_size,
    fontface = "bold",
    vjust = 1.3
  ) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(
    name = NULL,
    breaks = 1:5,
    labels = c("Global", "Undeveloped", "Agriculture", "Urban", "Mixed")
  ) +
  scale_y_continuous(
    limits = c(12, 36),
    breaks = c(12, 16, 20, 24, 28, 32, 36),
    labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00"),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(y = "Hypoxia onset time") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    axis.text.x = element_text(size = 26, color = "black",angle = 10, hjust = 0.5),
    axis.text.y = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 26)
  )

print(p)

ggsave(
  filename = "boxviolin_starttime.png", 
  plot = p, 
  width = 10, 
  height = 7, 
  dpi = 300,
  bg = "white"
)