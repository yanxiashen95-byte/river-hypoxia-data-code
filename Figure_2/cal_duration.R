#=================== 计算每天的缺氧时长 ========================
#=================== 计算每天的缺氧时长 ========================
#=================== 计算每天的缺氧时长 ========================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(purrr)
})

# =========================
# 0) 参数：路径 & 阈值
# =========================
in_dir  <- "D:/博后/论文/2/数据资料/日本/DO和WT最大最小值/clean data"
out_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised_Figure_last/US"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DO_TH <- 3   # 缺氧阈值（mg/L）

# =========================
# 1) 计算“每站点-每天”的缺氧持续时间
#    - 每天缺氧持续 = 当天(DO < 阈值)点数 * step_hours
#    - step_hours 自动由时间列推断（兜底1小时）
# =========================
calc_daily_hypoxia_duration <- function(df, dt_col = NULL, do_col = "avg_DO", do_th = 3,
                                        min_valid_hours = 20) {
  
  # 自动找时间列：优先 timestamp，其次 datetime/dateTime
  if (is.null(dt_col)) {
    dt_col <- if ("timestamp" %in% names(df)) {
      "timestamp"
    } else if ("datetime" %in% names(df)) {
      "datetime"
    } else if ("dateTime" %in% names(df)) {
      "dateTime"
    } else {
      NA_character_
    }
  }
  if (is.na(dt_col)) stop("找不到时间列：需要存在 timestamp / datetime / dateTime")
  
  # 解析时间 + DO
  df2 <- df %>%
    mutate(
      dt = as.character(.data[[dt_col]]),
      dt = str_trim(dt),
      datetime = suppressWarnings(parse_date_time(
        dt,
        orders = c(
          # 带时间
          "Y/m/d H:M:S","Y/m/d H:M","Y/m/d H",
          "Y-m-d H:M:S","Y-m-d H:M","Y-m-d H",
          "m/d/Y H:M:S","m/d/Y H:M","m/d/Y H",
          # ✅ 纯日期（万一仍然出现）
          "Y/m/d","Y-m-d","m/d/Y"
        )
      )),
      DO = suppressWarnings(as.numeric(.data[[do_col]]))
    ) %>%
    filter(!is.na(datetime), !is.na(DO)) %>%
    arrange(datetime)
  
  if (nrow(df2) == 0) return(tibble())
  
  # 推断时间步长（秒）→ 转小时
  dt_diff <- as.numeric(diff(df2$datetime), units = "secs")
  step_sec <- suppressWarnings(stats::median(dt_diff[dt_diff > 0], na.rm = TRUE))
  if (!is.finite(step_sec) || is.na(step_sec)) step_sec <- 3600  # 兜底：1小时
  step_hours <- step_sec / 3600
  
  # 按天汇总
  out <- df2 %>%
    mutate(
      date = as.Date(datetime),
      hyp  = (DO < do_th)
    ) %>%
    group_by(date) %>%
    summarise(
      n_valid = sum(!is.na(DO)),
      n_hyp   = sum(hyp, na.rm = TRUE),
      # 有效观测时长(小时)
      valid_hours = n_valid * step_hours,
      # 当天缺氧持续时间(小时)：缺氧点数 * step_hours
      hypoxia_hours = n_hyp * step_hours,
      .groups = "drop"
    ) %>%
    # 质量控制：至少有足够的有效观测（小时尺度通常>=20小时）
    filter(valid_hours >= min_valid_hours) %>%
    # 防止因为步长推断误差导致>24，做个硬截断
    mutate(
      hypoxia_hours = pmin(hypoxia_hours, 24)
    ) %>%
    arrange(date)
  
  out
}

# =========================
# 2) 扫描所有 CSV（递归）
#    站点ID：优先父文件夹名；如果父文件夹就是 in_dir，则用文件名
# =========================
csv_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
if (length(csv_files) == 0) stop("in_dir 下没有找到任何 CSV 文件")

get_site_id <- function(path) {
  parent <- basename(dirname(path))
  root   <- basename(normalizePath(in_dir, winslash = "/"))
  if (parent == root) {
    tools::file_path_sans_ext(basename(path))
  } else {
    parent
  }
}

site_tbl <- tibble(file = csv_files, site_id = map_chr(csv_files, get_site_id))

# =========================
# 3) 每站点合并CSV -> 计算“每天缺氧持续时间” -> 导出（每站点1个csv）
# =========================
site_tbl %>%
  group_by(site_id) %>%
  summarise(files = list(file), .groups = "drop") %>%
  pwalk(function(site_id, files) {
    
    message("Processing site: ", site_id, " (", length(files), " files)")
    
    df_all <- map(files, ~ read_csv(.x, show_col_types = FALSE)) %>%
      bind_rows()
    
    daily_tbl <- calc_daily_hypoxia_duration(
      df_all,
      dt_col = NULL,
      do_col = "DO",
      do_th  = DO_TH,
      min_valid_hours = 20   # 小时数据建议20；如果15分钟数据可改成 20 也行（valid_hours 仍是小时）
    )
    
    out_path <- file.path(out_dir, paste0(site_id, "_daily_hypoxia_duration.csv"))
    write_csv(daily_tbl, out_path)
  })

message("Done! 输出目录：", out_dir)

#====================== 中国的用下面这个算========================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(tibble)
})

# =========================
# 0) 路径 & 阈值
# =========================
in_dir  <- "D:/博后/论文/2/数据资料/中国/DO和WT最大最小值/数据清洗"
out_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised Figure 1/缺氧持续时间/China"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ✅ 只处理当前文件夹 CSV（不进子文件夹）
csv_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
if (length(csv_files) == 0) stop("in_dir 下没有找到任何 CSV 文件")

DO_TH <- 3

# =========================
# 1) 计算“每站点-每天”的缺氧持续时间（小时）
#    思路：每条观测代表一个 interval（到下一条/到当天结束），并用典型步长做上限截断
# =========================
calc_daily_hypoxia_duration_varstep <- function(df,
                                                dt_col = NULL,
                                                do_col = "DO",
                                                do_th = 3,
                                                min_valid_hours = 12) {
  
  # 自动找时间列
  if (is.null(dt_col)) {
    dt_col <- if ("timestamp" %in% names(df)) {
      "timestamp"
    } else if ("datetime" %in% names(df)) {
      "datetime"
    } else if ("dateTime" %in% names(df)) {
      "dateTime"
    } else {
      NA_character_
    }
  }
  if (is.na(dt_col)) stop("找不到时间列：需要存在 timestamp / datetime / dateTime")
  if (!(do_col %in% names(df))) stop("找不到 DO 列：", do_col)
  
  # 解析时间 + DO
  df2 <- df %>%
    mutate(
      dt = as.character(.data[[dt_col]]),
      dt = str_trim(dt),
      # 纯日期补 00:00:00
      dt = ifelse(
        str_detect(dt, "^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}$"),
        paste0(dt, " 00:00:00"),
        dt
      ),
      datetime = suppressWarnings(parse_date_time(
        dt,
        orders = c(
          "Y/m/d H:M:S","Y/m/d H:M","Y/m/d H",
          "Y-m-d H:M:S","Y-m-d H:M","Y-m-d H",
          "m/d/Y H:M:S","m/d/Y H:M","m/d/Y H",
          "Y/m/d","Y-m-d","m/d/Y"
        )
      )),
      DO = suppressWarnings(as.numeric(.data[[do_col]]))
    ) %>%
    filter(!is.na(datetime), !is.na(DO)) %>%
    arrange(datetime)
  
  if (nrow(df2) == 0) return(tibble())
  
  # 估计该站点“典型步长”（秒）：用时间差的众数（更适合你这种 1/2/3/4 小时混合）
  diffs_sec <- as.numeric(diff(df2$datetime), units = "secs")
  diffs_sec <- diffs_sec[diffs_sec > 0 & is.finite(diffs_sec)]
  if (length(diffs_sec) == 0) diffs_sec <- 4 * 3600  # 实在推不出就兜底 4小时
  
  # 四舍五入到分钟，便于取众数
  diffs_min <- round(diffs_sec / 60) * 60
  typical_step_sec <- as.numeric(names(sort(table(diffs_min), decreasing = TRUE)[1]))
  if (!is.finite(typical_step_sec) || is.na(typical_step_sec)) typical_step_sec <- 4 * 3600
  typical_step_h <- typical_step_sec / 3600
  
  # 为每条记录分配 interval（小时）
  # interval_end = min(下一条时间, 当天24:00)
  # interval = interval_end - 当前时间
  # interval 最多算 typical_step_h（避免缺测长间隔被算进去）
  df3 <- df2 %>%
    mutate(
      date = as.Date(datetime),
      hyp  = (DO < do_th),
      next_time = lead(datetime),
      day_end   = as.POSIXct(date + 1),   # 当天24:00
      # 到下一条或到当天结束
      raw_end = if_else(is.na(next_time), day_end, pmin(next_time, day_end)),
      raw_int_h = as.numeric(difftime(raw_end, datetime, units = "hours")),
      raw_int_h = if_else(raw_int_h < 0, 0, raw_int_h),
      # 用典型步长做上限截断
      int_h = pmin(raw_int_h, typical_step_h)
    )
  
  # 按天汇总：缺氧时长 = sum(int_h where hyp==TRUE)
  out <- df3 %>%
    group_by(date) %>%
    summarise(
      # 有效覆盖时长（小时）
      valid_hours = sum(int_h, na.rm = TRUE),
      # 缺氧持续时长（小时）
      hypoxia_hours = sum(int_h[hyp], na.rm = TRUE),
      # 方便你检查
      n_obs = n(),
      .groups = "drop"
    ) %>%
    # 质量控制：有效覆盖太少的天丢掉（你可改阈值）
    filter(valid_hours >= min_valid_hours) %>%
    mutate(
      hypoxia_hours = pmin(hypoxia_hours, 24),
      valid_hours   = pmin(valid_hours, 24)
    ) %>%
    arrange(date)
  
  out
}

# =========================
# 2) 每个 CSV 当作一个站点（站点ID=文件名）
# =========================
get_site_id <- function(path) tools::file_path_sans_ext(basename(path))

site_tbl <- tibble(file = csv_files, site_id = map_chr(csv_files, get_site_id))

# =========================
# 3) 逐站点计算并导出：每站点1个“每日缺氧时长”CSV
# =========================
pwalk(site_tbl, function(file, site_id) {
  
  message("Processing site: ", site_id)
  
  df_all <- read_csv(file, show_col_types = FALSE)
  
  daily_tbl <- calc_daily_hypoxia_duration_varstep(
    df_all,
    dt_col = NULL,
    do_col = "DO",          # ✅ 如果你的列名不是 DO，在这里改
    do_th  = DO_TH,
    # 覆盖小时阈值：你的典型步长多为4小时，那一天理论上约6个点
    # 这里给 12 小时是比较宽松的（至少覆盖半天）。如果你想更严格，可改 20 或 24
    min_valid_hours = 12
  )
  
  out_path <- file.path(out_dir, paste0(site_id, "_daily_hypoxia_duration.csv"))
  write_csv(daily_tbl, out_path)
})

message("Done! 输出目录：", out_dir)