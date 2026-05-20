library(readr)
library(dplyr)
library(ggplot2)
library(mgcv)

# =========================
# 0. 可调参数
# =========================
point_color <- "grey70"   # 点颜色
line_color  <- "#74add1"   # 拟合线颜色
ribbon_fill <- "#74add1"   # 置信区间填充颜色

point_size  <- 5.2         # 点大小
point_alpha <- 0.55        # 点透明度

line_width  <- 6.3         # 拟合线粗细
ribbon_alpha <- 0.18       # 置信区间透明度

border_color <- "black"    # 边框颜色
border_width <- 0.8        # 边框粗细

# =========================
# 1. 读入数据
# =========================
df <- read_csv("dataset_after_filtering.csv", show_col_types = FALSE) %>%
  rename_with(trimws)

# =========================
# 2. 只保留需要的列
# =========================
plot_df <- df %>%
  select(WT_p75, mean_duration) %>%
  mutate(
    WT_p75 = as.numeric(WT_p75),
    mean_duration = as.numeric(mean_duration)
  ) %>%
  filter(!is.na(WT_p75), !is.na(mean_duration))

# =========================
# 3. 拟合 GAM
# =========================
gam_mod <- gam(
  mean_duration ~ s(WT_p75, k = 5),
  data = plot_df,
  method = "REML"
)

summary(gam_mod)

# =========================
# 4. 构造预测线
# =========================
newdat <- data.frame(
  WT_p75 = seq(
    min(plot_df$WT_p75, na.rm = TRUE),
    max(plot_df$WT_p75, na.rm = TRUE),
    length.out = 300
  )
)

pred <- predict(gam_mod, newdata = newdat, se.fit = TRUE)

newdat <- newdat %>%
  mutate(
    fit = pred$fit,
    se = pred$se.fit,
    lwr = fit - 1.96 * se,
    upr = fit + 1.96 * se
  )

# =========================
# 5. 提取显著性信息
# =========================
sm <- summary(gam_mod)
p_val <- sm$s.table[1, "p-value"]
r2_val <- round(sm$r.sq, 3)

p_txt <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3)))

# =========================
# 6. 画图
# =========================
p1 <- ggplot(plot_df, aes(x = WT_p75, y = mean_duration)) +
  geom_point(
    color = point_color,
    size = point_size,
    alpha = point_alpha
  ) +
  geom_ribbon(
    data = newdat,
    aes(x = WT_p75, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = ribbon_fill,
    alpha = ribbon_alpha
  ) +
  geom_line(
    data = newdat,
    aes(x = WT_p75, y = fit),
    inherit.aes = FALSE,
    color = line_color,
    linewidth = line_width
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = paste0("R² = ", r2_val, ", ", p_txt),
    hjust = 1.8, vjust = 1.5, size = 9
  ) +
  scale_x_continuous(
    limits = c(8, 30),
    breaks = seq(8, 30, by = 4)
  ) +
  scale_y_continuous(
    breaks = seq(0, 24, by = 4)
  ) +
  labs(
    x = "Water temperature (°C)",
    y = "Duration (h)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    panel.border = element_rect(
      colour = border_color,
      fill = NA,
      linewidth = border_width
    ),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x  = element_text(size = 25),
    axis.text.y  = element_text(size = 25)
  )

print(p1)

ggsave("GAM_WT_mean_duration.png", p1, width = 7.5, height = 5.8, dpi = 300)