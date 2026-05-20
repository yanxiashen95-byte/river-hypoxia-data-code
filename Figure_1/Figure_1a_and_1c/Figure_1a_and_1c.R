#========================= 挑选代表性站点 ===========================
library(readr)
library(dplyr)
library(lubridate)

# ===== 你的数据路径 =====
data_dir <- "D:/博后/论文/2/数据资料/法国/DO和WT的最大最小值/clean data"   # ← 改这里
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

low_th  <- 3
high_th <- 5

for (f in files) {
  df <- read_csv(f, show_col_types = FALSE)
  
  if (!all(c("timestamp", "DO") %in% names(df))) next
  
  # ===== 修正：更加稳健的时间解析 =====
  df <- df %>%
    mutate(
      # ymd_hm 可以自动识别 2020-7-28 或 2020-07-28
      datetime = ymd_hm(timestamp, tz = "UTC"), 
      date = as.Date(datetime)
    ) %>%
    filter(!is.na(date), !is.na(DO))
  
  # 如果转换后没数据，打印个提示帮你排查
  if (nrow(df) == 0) {
    cat("文件", basename(f), "解析后数据为空，请检查时间格式\n")
    next
  }
  
  # ===== 逻辑判断保持不变 =====
  hit_days <- df %>%
    group_by(date) %>%
    summarise(
      has_low  = any(DO < low_th,  na.rm = TRUE),
      has_high = any(DO > high_th, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(has_low & has_high)
  
  if (nrow(hit_days) > 0) {
    cat("找到符合条件的天数！文件:", basename(f), "\n")
    for (d in hit_days$date) {
      day_data <- df %>% filter(date == d)
      
      # 保存文件
      out_name <- paste0("hit_", tools::file_path_sans_ext(basename(f)), "_", d, ".csv")
      write_csv(day_data, out_name)
    }
  }
}

#======================== 代表性站点绘图 ===========================
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

setwd("D:/Python/R/5 hourly_and_daily_DO/revised_Figrue 2/global hypoxia/sub_hypoxia")
# ===== 读数据 =====
hf <- read_csv("China.csv", show_col_types = FALSE)
lf <- read_csv("China-LF.csv",  show_col_types = FALSE)

hf <- hf %>%
  mutate(
    timestamp = as.POSIXct(timestamp,
                           format = "%Y/%m/%d %H:%M",
                           tz = "UTC")
  )

lf <- lf %>%
  mutate(
    timestamp = as.POSIXct(timestamp,
                           format = "%Y/%m/%d",
                           tz = "UTC") + hours(14)   # 放到当天中午
  )

hf_shade <- hf %>%
  mutate(
    DO_lower = ifelse(DO < 3, DO, NA),
    DO_upper = ifelse(DO < 3, 3, NA)
  )


scale_coeff = 10
p <- ggplot() +
  # --- 1. 缺氧阴影层 ---
  geom_ribbon(
    data = hf_shade,
    aes(x = timestamp, ymin = DO_lower, ymax = DO_upper),
    fill  = "#fdae61", alpha = 0.7
  ) +
  
  # --- 2. DO_percent 曲线层 (右侧轴对应线) ---
  geom_line(
    data = hf,
    aes(x = timestamp, y = DO_percent / scale_coeff), 
    # ↓↓↓ 这里修改为你喜欢的颜色 ↓↓↓
    color = "#cbd5e8",      # 例如: "red", "darkgreen", 或 "#2b8cbe"
    linewidth = 2.5,
    linetype = "twodash"
  ) +
  
  # --- 3. 高频 DO 曲线 (左侧轴) ---
  geom_line(
    data = hf,
    aes(x = timestamp, y = DO),
    color = "grey70", linewidth = 2.5
  ) +
  
  # --- 4. 低频 DO 线与点 ---
  geom_line(data = lf, aes(x = timestamp, y = DO), color = "grey35", linewidth = 2) +
  geom_point(data = lf, aes(x = timestamp, y = DO), color = "grey35", size = 3) +
  
  # --- 5. 缺氧阈值线 ---
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey40", linewidth = 1) +
  
  # --- 6. 双 Y 轴坐标设置 ---
  scale_y_continuous(
    name = "DO (mg/L)",
    limits = c(0, 14), 
    breaks = seq(0, 14, 2),
    expand = expansion(mult = c(0, 0.05)),
    
    sec.axis = sec_axis(
      trans = ~ . * scale_coeff, 
      name = expression(DO[sat]~"%"),
      breaks = seq(0, 140, 20)
    )
  ) +
  
  # --- 7. 时间轴与主题 (保持不变) ---
  scale_x_datetime(
    name = "Date",
    breaks = seq(min(hf$timestamp, na.rm = TRUE), max(hf$timestamp, na.rm = TRUE), by = "48 hours"),
    labels = scales::date_format("%b %d", locale = "C"),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.title.y.right = element_text(size = 20, angle = 90, vjust = 1.5,color = "black"), # 让轴标题颜色和线一致
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    axis.text.y.right = element_text(size = 20,color = "black"), # 让轴刻度颜色和线一致
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.margin = margin(t = 15, r = 60, b = 15, l = 20, unit = "pt")
  )

print(p)


ggsave(
  filename = "HF_vs_LF_DO_China2.png",
  plot     = p,
  width    = 8,
  height   = 4.5,
  units    = "in",
  dpi      = 600
)

