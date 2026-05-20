library(suncalc)
library(dplyr)
library(readr)
library(lubridate)

calc_daylength <- function(date, lat, lon) {
  
  sun <- getSunlightTimes(
    date = date,
    lat  = lat,
    lon  = lon,
    keep = c("sunrise", "sunset"),
    tz   = "UTC"   # ⚠️ 统一用 UTC，避免时区坑
  )
  
  as.numeric(difftime(
    sun$sunset,
    sun$sunrise,
    units = "hours"
  ))
}

# 经纬度表
site_tbl <- read_csv("lon_and_lat.csv")

# 时间序列文件夹
data_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/日照时长/DO"
out_dir  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/日照时长/output_with_daylength"

for (i in 1:nrow(site_tbl)) {
  
  site_id <- site_tbl$file_name[i]
  lat     <- site_tbl$latitude[i]
  lon     <- site_tbl$longitude[i]
  
  file_in <- file.path(data_dir, paste0(site_id, ".csv"))
  
  if (!file.exists(file_in)) {
    message("❌ File not found: ", site_id)
    next
  }
  
  df <- read_csv(file_in, show_col_types = FALSE)
  
  # ---- 统一时间 ----
  df <- df %>%
    rename_with(trimws) %>%
    mutate(
      timestamp = parse_date_time(
        timestamp,
        orders = c(
          "Y/m/d H:M",
          "Y-m-d H:M",
          "Y-m-d H:M:S"
        ),
        tz = "UTC"
      ),
      date = as.Date(timestamp)
    )
  
  # ---- 计算每个 date 的日照时长 ----
  day_tbl <- df %>%
    distinct(date) %>%
    mutate(
      daylength = calc_daylength(date, lat, lon)
    )
  
  # ---- 合并回原表 ----
  df_out <- df %>%
    left_join(day_tbl, by = "date")
  
  # ---- 保存 ----
  write_csv(
    df_out,
    file.path(out_dir, paste0(site_id, "_with_daylength.csv"))
  )
  
  message("✅ Finished: ", site_id)
}
dir.create(out_dir, showWarnings = FALSE)


#================= 提取时间长度 ========================
library(readr)
library(dplyr)

data_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/日照时长/DO"
out_dir  <- file.path(data_dir, "daily_daylength")

dir.create(out_dir, showWarnings = FALSE)

files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

for (f in files) {
  
  df <- read_csv(f, show_col_types = FALSE)
  
  df_day <- df %>%
    filter(!is.na(date), !is.na(daylength)) %>%  # 去掉 NA 行
    distinct(date, daylength) %>%                 # 每天只留 1 行
    arrange(date)
  
  write_csv(
    df_day,
    file.path(out_dir, basename(f))
  )
  
  message("✅ Daily file created: ", basename(f))
}

#========================= 匹配日照长度和DO差值 ================================
library(readr)
library(dplyr)

# ===== 路径 =====
do_dir  <- "G:/DO-WT/机制分析/SHAP分析/高频/slope_full_day"
dl_dir  <- "G:/download_Q/Europe"
out_dir <- "G:/download_Q/paired_Q_and_DO"

dir.create(out_dir, showWarnings = FALSE)

# ===== 列出 DO 文件 =====
do_files <- list.files(do_dir, pattern = "\\.csv$", full.names = TRUE)

for (f in do_files) {
  
  fname <- basename(f)               # 例如 613.csv
  dl_f  <- file.path(dl_dir, fname)  # 对应的日照时长文件
  
  if (!file.exists(dl_f)) {
    message("❌ No matching daylength file for: ", fname)
    next
  }
  
  # ---- 读取 ----
  df_do <- read_csv(f, show_col_types = FALSE)
  df_dl <- read_csv(dl_f, show_col_types = FALSE)
  
  # ---- 统一 date 类型（非常重要）----
  df_do <- df_do %>% mutate(date = as.Date(date))
  df_dl <- df_dl %>% mutate(date = as.Date(date))
  
  # ---- 合并 ----
  df_merged <- df_do %>%
    left_join(df_dl, by = "date")
  
  # ---- 保存 ----
  write_csv(
    df_merged,
    file.path(out_dir, fname)
  )
  
  message("✅ Merged: ", fname)
}

#================================== 画图 ===================================
#===================== 这里也画代表性站点的好了 ============================
#========================= 每个区域选一个站点 ==============================
library(tidyverse)

in_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/rep_sites_by_country_gt_50"

files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

# =========================
# 1. 读取 + 合并
# =========================
df_all <- map_dfr(files, function(f) {
  
  site_id <- tools::file_path_sans_ext(basename(f))
  
  df <- read_csv(f, show_col_types = FALSE) %>%
    rename_with(trimws)
  
  if (!all(c("daylength", "DO_min_night") %in% names(df))) {
    message("❌ 缺列，跳过: ", site_id)
    return(NULL)
  }
  
  df %>%
    select(daylength, DO_min_night) %>%
    mutate(site = site_id)
})

# =========================
# 2. 数据清洗（关键修改）
# =========================
df_all <- df_all %>%
  filter(
    !is.na(daylength),
    !is.na(DO_min_night),
    DO_min_night > 1,        # 删掉等于 0 的点
    DO_min_night <= 15,
    daylength <16
    #WT_mean_nighttime > 0,
    #WT_mean_nighttime <= 38,
  )

# =========================
# 3. 线性模型 & R²
# =========================
fit <- lm(DO_min_night ~ daylength, data = df_all)
r2  <- summary(fit)$r.squared

# =========================
# 4. 作图
# =========================
# ======================
# 可调参数（你之后只改这里）
# ======================
pt_size    <- 2      # 点大小
pt_alpha   <- 1     # 点透明度
pt_stroke  <- 0.5     # 点边框粗细

axis_title_size <- 13
axis_text_size  <- 11

r2_size <- 4.8

# R2 标注位置（相对坐标，更稳）
x_r2 <- quantile(df_all$daylength, 0.95, na.rm = TRUE)
y_r2 <- quantile(df_all$DO_min_night, 0.95, na.rm = TRUE)

# ======================
# 作图
# ======================
p <- ggplot(df_all, aes(x = daylength, y = DO_min_night)) +
  
  geom_point(
    shape = 21,               # 关键：有边框的点
    fill  = "red",
    color = "white",
    stroke = 1.5,
    size  = 5
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 2
  ) +
  
  annotate(
    "text",
    x = max(df_all$daylength, na.rm = TRUE) * 0.95,   # 👈 手动控制位置
    y = max(df_all$DO_min_night, na.rm = TRUE) * 0.95,
    label = paste0("R² = ", round(r2, 2)),
    hjust = 1,
    vjust = 1,
    size = 10
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01))   # 👈 左右各留 5%
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.10))   # 👈 下 5%，上 10%
  ) +
  
  labs(
    x = "WT (°C)",
    y = expression("Nighttime minimum DO (mg/L)")
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    
    # ===== 坐标轴标题 =====
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    
    # ===== 坐标轴刻度 =====
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    
    # ===== 四周边框（panel border）粗细 =====
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3)
  )

print(p)

ggsave(
  filename = "daylength_DO_gt_50.png",  # 修改为你的路径
  plot     = p,
  width    = 7,      # 英寸
  height   = 5,
  dpi      = 600     # 论文级分辨率
)

#=============================================================================
#=======================画那个全球每个站点一条灰线，全球的总线================

# =========================
# 0. 加载库
# =========================
library(tidyverse)

# =========================
# 1. 文件夹路径（改成你自己的）
# =========================
dir_lt30  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_lt_30"
dir_3050  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_30_50"
dir_gt50  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_gt_50"

# =========================
# 2. 读取一个文件夹里的所有 CSV
# =========================
read_folder <- function(dir_path, group_name) {
  
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  df_list <- lapply(files, function(f) {
    
    read_csv(f, show_col_types = FALSE) %>%
      select(DO_min_night, WT_mean_nighttime) %>%
      filter(
        !is.na(DO_min_night),
        !is.na(WT_mean_nighttime),
        DO_min_night > 1,
        DO_min_night <= 15,
        WT_mean_nighttime >= 0,
        WT_mean_nighttime <= 38
        #daylength < 24
      ) %>%
      mutate(
        site  = basename(f),
        group = group_name
      )
  })
  
  bind_rows(df_list)
}

df_lt30 <- read_folder(dir_lt30, "lt30")
df_3050 <- read_folder(dir_3050, "30_50")
df_gt50 <- read_folder(dir_gt50, "gt50")

# 全球
df_all <- bind_rows(df_lt30, df_3050, df_gt50)


# =========================
# 3. 画图
# =========================
p <- ggplot() +
# ---------
# 3.1 灰色：每个 CSV 一条趋势线
# ---------
geom_smooth(
  data = df_all,
  aes(
    x = WT_mean_nighttime,
    y = DO_min_night,
    group = site
  ),
  method = "lm",
  se = FALSE,
  color = "grey70",
  linewidth = 0.6,
  alpha = 0.6
) +
  
  # ---------
# 3.2 全球总体趋势线（红色）
# ---------
geom_smooth(
  data = df_all,
  aes(
    x = WT_mean_nighttime,
    y = DO_min_night
  ),
  method = "lm",
  se = FALSE,
  color = "red",
  linewidth = 3
) +
  
  # ---------
# 3.3 不同纬度带的总体趋势线
# ---------
geom_smooth(
  data = df_lt30,
  aes(x = WT_mean_nighttime, y = DO_min_night),
  method = "lm",
  se = FALSE,
  color = "#abdda4",
  linewidth = 4
) +
  
  geom_smooth(
    data = df_3050,
    aes(x = WT_mean_nighttime, y = DO_min_night),
    method = "lm",
    se = FALSE,
    color = "#fee090",
    linewidth = 4
  ) +
  
  geom_smooth(
    data = df_gt50,
    aes(x = WT_mean_nighttime, y = DO_min_night),
    method = "lm",
    se = FALSE,
    color = "#abd9e9",
    linewidth = 4
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01))   # 👈 左右各留 5%
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.10))   # 👈 下 5%，上 10%
  ) +
  # ---------
# 3.4 坐标轴 & 主题
# ---------
labs(
  x = expression(bar(T)[night]~"("*degree*C*")"),
  y = expression("Minimum nighttime DO (mg/L)")
) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    
    # ===== 坐标轴标题 =====
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    
    # ===== 坐标轴刻度 =====
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3)
  )

print(p)

ggsave(
  filename = "WT_min_DO_global.png",  # 修改为你的路径
  plot     = p,
  width    = 7,      # 英寸
  height   = 5,
  dpi      = 600     # 论文级分辨率
)

#================== 画盒图 =====================================================
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tibble)

dir_lt30  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_lt_30"
dir_3050  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_30_50"
dir_gt50  <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 5/太阳辐射/lat_split/lat_gt_50"

read_folder <- function(dir_path, group_name) {
  
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  df_list <- lapply(files, function(f) {
    
    df <- read_csv(f, show_col_types = FALSE)
    
    # ===== 关键 1：清理列名 =====
    names(df) <- trimws(names(df))
    
    # ===== 关键 2：检查必须列 =====
    need_cols <- c(
      "DO_min_night",
      "ALLSKY_SFC_SW_DWN",
      "WT_mean_nighttime",
      "daylength"
    )
    
    if (!all(need_cols %in% names(df))) {
      stop(paste("缺少列：", f))
    }
    
    df %>%
      select(all_of(need_cols)) %>%
      filter(
        !is.na(DO_min_night),
        !is.na(ALLSKY_SFC_SW_DWN),
        !is.na(WT_mean_nighttime),
        !is.na(daylength),
        DO_min_night > 1,
        DO_min_night <= 15,
        WT_mean_nighttime >= 0,
        WT_mean_nighttime <= 38,
        daylength < 24
      ) %>%
      mutate(
        site  = basename(f),
        group = group_name
      )
  })
  
  bind_rows(df_list)
}

# ===== 真正生成数据 =====
df_lt30 <- read_folder(dir_lt30, "<30")
df_3050 <- read_folder(dir_3050, "30–50")
df_gt50 <- read_folder(dir_gt50, ">50")

df_all <- bind_rows(df_lt30, df_3050, df_gt50)

calc_site_r2 <- function(df, xvar, yvar) {
  
  df %>%
    group_by(site) %>%
    summarise(
      R2 = {
        d <- cur_data()
        if (nrow(d) < 5) {
          NA_real_
        } else {
          summary(lm(reformulate(xvar, yvar), data = d))$r.squared
        }
      },
      .groups = "drop"
    )
}

drivers <- tribble(
  ~driver,      ~xvar,
  "DO – Nighttime WT",  "WT_mean_nighttime",
  "DO – dayl",  "daylength",
  "DO – srad",  "ALLSKY_SFC_SW_DWN"
)

r2_all <- map_dfr(1:nrow(drivers), function(i) {
  
  drv  <- drivers$driver[i]
  xvar <- drivers$xvar[i]
  
  bind_rows(
    calc_site_r2(df_all,  xvar, "DO_min_night") %>% mutate(group = "Global", driver = drv),
    calc_site_r2(df_lt30, xvar, "DO_min_night") %>% mutate(group = "<30",   driver = drv),
    calc_site_r2(df_3050, xvar, "DO_min_night") %>% mutate(group = "30–50", driver = drv),
    calc_site_r2(df_gt50, xvar, "DO_min_night") %>% mutate(group = ">50",   driver = drv)
  )
})

cols <- c(
  "Global" = "red",
  "<30"    = "#abdda4",
  "30–50"  = "#fee090",
  ">50"    = "#abd9e9"
)

r2_all <- r2_all %>%
  mutate(
    driver = factor(
      driver,
      levels = c("DO – Nighttime WT", "DO – dayl", "DO – srad")
    ),
    
    # =========================
    # 2. 固定 group 的顺序（每组内部）
    # =========================
    group = factor(
      group,
      levels = c("Global", "<30", "30–50", ">50")
    )
  )

p_r2 <- ggplot(r2_all, aes(x = driver, y = R2, fill = group, color = group)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.6,
    outlier.shape = NA,
    linewidth = 0.9,
    median.colour = "white",
    median.linewidth = 1.4
  ) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols, guide = "none") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_discrete(labels = c(
    "DO – Nighttime WT" = expression("DO \u2013 " * bar(T)[night]),
    "DO – dayl"  = "DO \u2013 dayl",
    "DO – srad"  = "DO \u2013 srad"
  )) +
  labs(x = NULL, y = expression(R^2)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3)
  )

print(p_r2)


ggsave(
  filename = "R2_min_DO_global.png",  # 修改为你的路径
  plot     = p_r2,
  width    = 16,      # 英寸
  height   = 4,
  dpi      = 600     # 论文级分辨率
)
