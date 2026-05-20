library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("D:/Python/R/5 hourly_and_daily_DO/revised Figure 1/分类绘图")

# 1) 读入数据（改成你的路径）
# 1) 读取数据
df <- read_csv("plog.csv", show_col_types = FALSE)

# 2) 宽表 -> 长表
df_raw <- df %>%
  pivot_longer(
    cols = everything(),
    names_to = "region",
    values_to = "value"
  ) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# 3) 创建 Global 副本并合并
df_global <- df_raw %>%
  mutate(region = "Global")

df_combined <- bind_rows(df_global, df_raw)

# --- [核心修改] 手动设置 X 轴顺序与缩放名 ---
# 确保顺序是：Global, Australia, China, Europe, Japan, USA
df_combined <- df_combined %>%
  mutate(region = factor(region, levels = c("Global", "Australia", "China", "Europe", "Japan", "the United States")))

# 4) 绘图
p <- ggplot(df_combined, aes(x = region, y = value, fill = (region == "Global"))) +
  geom_boxplot(
    width = 0.6,
    color = "black",       
    linewidth = 0.8,
    alpha = 0.85,          # ✅ 添加透明度设置
    outlier.shape = 16,
    outlier.size = 1.6
  ) +
  # --- 设置 X 轴缩写 ---
  scale_x_discrete(
    labels = c(
      "Global"    = "Global",
      "Australia" = "Australia",
      "China"     = "China",
      "Europe"    = "Europe",
      "Japan"     = "Japan",
      "the United States" = "United \nStates"
    )
  ) +
  # --- 设置颜色：Global 灰色，其余橙色 ---
  scale_fill_manual(
    values = c("TRUE" = "grey85", "FALSE" = "#fdae61"), 
    guide = "none"
  ) +
  labs(x = NULL, y = "Underestimation (%)") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 24, color = "black"),
    axis.text.y = element_text(size = 24, color = "black"),
    axis.title.y = element_text(size = 24),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2)
  )

print(p)


ggsave(
  filename = "underestimated ratio1.png",
  plot     = p,
  width    = 8,     # 英寸：按需要改
  height   = 5,
  dpi      = 600,
  bg       = "white"
)