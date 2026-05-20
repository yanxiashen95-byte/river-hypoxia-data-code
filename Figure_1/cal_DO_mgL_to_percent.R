
#==============================计算DO-sat====================================
# 加载必要的库
library(dplyr)
library(readr)

# 定义计算DO饱和度的函数
calculate_DO_sat_usgs <- function(avg_temp, elevation, salinity = 0) {
  DO_sat_sea_level <- 14.621 + (-0.436 * avg_temp) + (0.04321 * avg_temp^2) - (0.001366 * avg_temp^3)
  DO_sat_usgs <- DO_sat_sea_level * exp(-elevation / 760) * (1 - 0.00006 * salinity)
  return(DO_sat_usgs)
}

# 设置文件夹路径
folder_path <- "D:/Python/R/5 hourly_and_daily_DO/revised_Figrue 2/global hypoxia/sub_hypoxia/中国"  # 请替换为实际的文件夹路径

# 获取该文件夹下所有的 CSV 文件
files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# 遍历所有 CSV 文件
for (file in files) {
  # 读取 CSV 文件
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # 检查是否包含所需的列
  if ("WT" %in% colnames(data) & "DO" %in% colnames(data) & "Elevation" %in% colnames(data)) {
    data$WT <- as.numeric(data$WT)
    data$DO <- as.numeric(data$DO)
    data$Elevation <- as.numeric(data$Elevation)
    # 计算 DOsat
    data$DOsat <- mapply(calculate_DO_sat_usgs, data$WT, data$Elevation)
    
    # 计算 DO%（百分比）
    data$DO_percent <- (data$DO / data$DOsat) * 100
    
    # 保存计算后的结果到新的 CSV 文件
    write.csv(data, file, row.names = FALSE)
    cat(paste("Calculated DOsat and DO% and saved to", file, "\n"))
    
  } else {
    warning(paste("File", file, "is missing one of the required columns: avg_temp, avg_DO, or Elevation"))
  }
}

cat("DOsat and DO% calculation is completed for all files.\n")

