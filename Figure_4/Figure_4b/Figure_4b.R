library(ggplot2)
library(sf)
library(dplyr)
library(readr)

# 读取世界地图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp", quiet = TRUE)

# 读取数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# ---- 分箱：per=0 单独一类，其余按区间划分 ----
# 如果 per 不在 [0,100]，下面会设为 NA（可按需修改）
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转为 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 颜色：0=蓝，其余四档红系渐变
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 5, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(-60, 90, 30)) +
  scale_x_continuous(breaks = seq(-180, 180, 60)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  labs(title = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.y.left = element_text(size = 15, color = "black"),
    axis.text.x.bottom = element_text(size = 15, color = "black"),
    axis.ticks.y.left = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

# 保存图像
ggsave("世界.png",plot = p, width = 15, height = 6, dpi = 600, bg = "white")



#============================各个国家的，日本======================
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# 读日本底图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/Japan.shp", quiet = TRUE) |>
  st_transform(4326)

# 读数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# —— 分箱：与世界图一致（per=0 单独一类，其余四档）——
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 与世界图一致的离散配色
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图（日本范围与世界图风格一致）
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 8, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(30, 45, 5)) +
  scale_x_continuous(breaks = seq(130, 146, 5)) +
  coord_sf(xlim = c(130, 146), ylim = c(30, 45), expand = FALSE) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom",                 # 与世界图一致（如需隐藏改为 "none"）
    panel.grid = element_blank(),
    axis.text.y.left    = element_text(size = 20, color = "black"),
    axis.text.x.bottom  = element_text(size = 20, color = "black"),
    axis.ticks.y.left   = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

ggsave("Japan.png", plot = p, width = 10, height = 6, dpi = 600, bg = "white")

#=====================中国=====================
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# 读日本底图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/Chiant.shp", quiet = TRUE) |>
  st_transform(4326)

# 读数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# —— 分箱：与世界图一致（per=0 单独一类，其余四档）——
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 与世界图一致的离散配色
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图（日本范围与世界图风格一致）
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 8, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(15, 55, 10)) +
  scale_x_continuous(breaks = seq(73, 138, 15)) +
  coord_sf(xlim = c(73, 138), ylim = c(15, 57), expand = FALSE) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom",                 # 与世界图一致（如需隐藏改为 "none"）
    panel.grid = element_blank(),
    axis.text.y.left    = element_text(size = 18, color = "black"),
    axis.text.x.bottom  = element_text(size = 18, color = "black"),
    axis.ticks.y.left   = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)
ggsave("Chiant.png", plot = p, width = 10, height = 6, dpi = 600, bg = "white")

#===================美国====================
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# 读日本底图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/US.shp", quiet = TRUE) |>
  st_transform(4326)

# 读数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# —— 分箱：与世界图一致（per=0 单独一类，其余四档）——
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 与世界图一致的离散配色
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图（日本范围与世界图风格一致）
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 8, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(23, 52, 10)) +
  scale_x_continuous(breaks = seq(-127, -65, 15)) +
  coord_sf(xlim = c(-127, -65), ylim = c(23, 52), expand = FALSE) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom",                 # 与世界图一致（如需隐藏改为 "none"）
    panel.grid = element_blank(),
    axis.text.y.left    = element_text(size = 18, color = "black"),
    axis.text.x.bottom  = element_text(size = 18, color = "black"),
    axis.ticks.y.left   = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

ggsave("US.png", plot = p, width = 11, height = 6, dpi = 600, bg = "white")

#==============澳大利亚================
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# 读日本底图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/澳大利亚.shp", quiet = TRUE) |>
  st_transform(4326)

# 读数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# —— 分箱：与世界图一致（per=0 单独一类，其余四档）——
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 与世界图一致的离散配色
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图（日本范围与世界图风格一致）
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 8, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(-40, -27, 5)) +
  scale_x_continuous(breaks = seq(135, 155, 5)) +
  coord_sf(xlim = c(135, 155), ylim = c(-40, -27), expand = FALSE) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "bottom",                 # 与世界图一致（如需隐藏改为 "none"）
    panel.grid = element_blank(),
    axis.text.y.left    = element_text(size = 18, color = "black"),
    axis.text.x.bottom  = element_text(size = 18, color = "black"),
    axis.ticks.y.left   = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

ggsave("澳大利亚.png", plot = p, width = 10, height = 6, dpi = 600, bg = "white")

#==============欧洲================
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# 读日本底图
world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/欧洲.shp", quiet = TRUE) |>
  st_transform(4326)

# 读数据
data <- read_csv("global_1_3.csv", show_col_types = FALSE)

# —— 分箱：与世界图一致（per=0 单独一类，其余四档）——
data <- data %>%
  mutate(
    per_bin = case_when(
      is.na(per) ~ NA_character_,
      per == 0 ~ "0",
      per > 0  & per <= 25  ~ "0–25",
      per > 25 & per <= 50  ~ "25–50",
      per > 50 & per <= 75  ~ "50–75",
      per > 75 & per <= 100 ~ "75–100",
      TRUE ~ NA_character_
    ),
    per_bin = factor(per_bin, levels = c("0", "0–25", "25–50", "50–75", "75–100"))
  )

# 转 sf 点
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# 与世界图一致的离散配色
cols_fill <- c(
  "0"       = "#4575b4",  # 蓝
  "0–25"    = "#fee8c8",  # 浅红
  "25–50"   = "#fdbb84",
  "50–75"   = "#fc8d59",
  "75–100"  = "#d73027"   # 深红
)

# 画图（日本范围与世界图风格一致）
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(
    data = points_sf,
    aes(fill = per_bin),
    shape = 21, size = 8, stroke = 0.7, color = "white"
  ) +
  scale_fill_manual(
    values = cols_fill,
    drop = FALSE,
    na.translate = FALSE,
    name = "Percent"
  ) +
  scale_y_continuous(breaks = seq(40, 60, 5)) +
  scale_x_continuous(breaks = seq(-10, 20, 10)) +
  coord_sf(xlim = c(-10, 20), ylim = c(40, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "") +
  theme(
    legend.position = "right",                 # 与世界图一致（如需隐藏改为 "none"）
    panel.grid = element_blank(),
    axis.text.y.left    = element_text(size = 18, color = "black"),
    axis.text.x.bottom  = element_text(size = 18, color = "black"),
    axis.ticks.y.left   = element_line(color = "black"),
    axis.ticks.x.bottom = element_line(color = "black"),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p)

ggsave("欧洲.png", plot = p, width = 10, height = 6, dpi = 600, bg = "white")