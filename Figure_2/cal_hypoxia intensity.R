library(tidyverse)
library(lubridate)

# 1. 获取所有 CSV 文件的路径
input_folder <-  "D:/博后/论文/2/数据资料/美国/全天24小时的DO_WT"

output_folder <- "D:/Python/R/5 hourly_and_daily_DO/revised_Figure_last/hypoxia intensity/Japan"    # 处理后的文件存放位置

# 如果输出文件夹不存在，则创建它
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 2. 获取所有 CSV 文件的完整路径
file_list <- list.files(path = input_folder, pattern = "*.csv", full.names = TRUE)

# 3. 循环处理每一个文件
for (file_path in file_list) {
  
  # 获取文件名（不带路径），用于生成输出文件名
  file_name <- basename(file_path)
  
  # 读取数据
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # 检查列名并统一为 DO
  if ("avg_DO" %in% names(df)) {
    df <- df %>% rename(DO = avg_DO)
  }
  
  # 如果文件里没有 DO 或 timestamp 列，则跳过该文件
  if (!("DO" %in% names(df)) | !("dateTime" %in% names(df))) {
    warning(paste("跳过文件：", file_name, "原因：缺少必要的列"))
    next
  }
  
  # 执行计算逻辑
  df_processed <- df %>%
    mutate(timestamp = as.POSIXct(dateTime),
           date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(
      # 如果当天有 DO <= 3，计算这些小时的均值，否则设为 NA
      low_do_mean = if(any(DO <= 3, na.rm = TRUE)) {
        mean(DO[DO <= 3], na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  # 4. 保存到新文件夹中，文件名保持一致（或者加个前缀，如 "daily_"）
  output_path <- file.path(output_folder, paste0("daily_", file_name))
  write_csv(df_processed, output_path)
  
  message(paste("处理完成并保存:", output_path))
}

message("所有文件处理完毕！")