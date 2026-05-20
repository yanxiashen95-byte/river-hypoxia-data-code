
#=================统计小于3的天数============================
library(tidyverse)
library(lubridate)

# 设置你的 CSV 文件夹路径（请修改）
folder_path <- "D:/博后/论文/2/数据资料/中国/DO和WT最大最小值/数据清洗"  # ⚠️ 请替换为实际路径
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# 初始化结果表格
result_summary <- data.frame(
  file = character(),
  total_days = numeric(),
  days_with_DO_below_3 = numeric(),
  days_11am_DO_below_3 = numeric(),
  stringsAsFactors = FALSE
)

# 遍历每个文件
for (file in file_list) {
  df <- read_csv(file)
  
  # 标准化时间格式
  df <- df %>%
    mutate(
      # 如果 timestamp 已经是时间类型，可以直接使用
      date = as.Date(timestamp),
      hour = hour(timestamp)
    )
  
  # 1. 统计总天数
  total_days <- n_distinct(df$date)
  
  # 2. 统计 DO<3 的天数（任意时间）
  days_with_DO_below_3 <- df %>%
    filter(DO < 3) %>%
    distinct(date) %>%
    nrow()
  
  # 3. 统计 11 点 DO<3 的天数
  days_11am_DO_below_3 <- df %>%
    filter(hour == 12, DO < 3) %>%
    distinct(date) %>%
    nrow()
  
  # 加入结果
  result_summary <- rbind(result_summary, data.frame(
    file = basename(file),
    total_days = total_days,
    days_with_DO_below_3 = days_with_DO_below_3,
    days_11am_DO_below_3 = days_11am_DO_below_3
  ))
}

# 保存为 CSV 文件
write_csv(result_summary, file.path("DO_summary_results.csv"))
