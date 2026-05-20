#========================每个国家的日平均 ====================
library(readr)
library(dplyr)
library(lubridate)
library(purrr)

data_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised_Figrue 2/global hypoxia/日内波动/Europe"
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

# ===== 1. 读入并合并所有 csv =====
all_data <- map_dfr(files, function(f) {
  
  df <- read_csv(f, show_col_types = FALSE)
  
  # ===== 必要列检查（改成 timestamp）=====
  if (!all(c("timestamp", "avg_DO", "wt") %in% names(df))) return(NULL)
  
  df %>%
    mutate(
      datetime = ymd_hm(timestamp),
      hour     = hour(datetime)
    ) %>%
    filter(!is.na(hour), !is.na(avg_DO))
})

# ===== 2. 按小时计算均值 =====
diurnal_mean <- all_data %>%
  group_by(hour) %>%
  summarise(
    DO_mean = mean(avg_DO, na.rm = TRUE),
    DO_sd   = sd(avg_DO, na.rm = TRUE),
    WT_mean = mean(wt, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  ) %>%
  arrange(hour)

print(diurnal_mean)

# ===== 3. 保存结果（强烈建议）=====
write_csv(
  diurnal_mean,
  file.path(data_dir, "diurnal_mean_DO_WT_Europe.csv")
)

#=================================== 绘图 ======================================
#================================== 绘图 =======================================

setwd("D:/Python/R/5 hourly_and_daily_DO/revised_Figrue 2/global hypoxia/日内波动")

data_dir <- "D:/Python/R/5 hourly_and_daily_DO/revised_Figrue 2/global hypoxia/日内波动"   # ← 改这里
files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

stopifnot(length(files) > 1)

df_all <- map_dfr(
  files,
  function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      mutate(
        group = tools::file_path_sans_ext(basename(f))
      )
  }
)

p_DO <- ggplot(
  df_all,
  aes(x = hour, y = DO_mean, color = group)
) +
  
  # error bar（±1 SD）
  geom_errorbar(
    aes(
      ymin = DO_mean - 0.04*DO_sd,
      ymax = DO_mean + 0.04*DO_sd
    ),
    width = 0.8,
    linewidth = 0.5,
    alpha = 0.85
  ) +
  
  # mean line
  geom_line(
    linewidth = 2.6
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    position = "right",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_color_manual(
    values = c(
      "Australia" = "#fb8072",
      "China"     = "#4393c3",
      "Europe"    = "#fdb462",
      "Japan"     = "#b3de69",
      "US"        = "#fccde5"
    ),
    labels = c(
      "Australia" = "Australia (157 sites)",
      "China"     = "China (1991 sites)",
      "Europe"    = "Europe (116 sites)",
      "Japan"     = "Japan (140 sites)",
      "US"        = "US (676 sites)"
    )
  ) +
  labs(
    x = "Hours (h)",
    y = "DO (mg/L)",
    color = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    axis.text.x  = element_text(size = 18),
    axis.text.y  = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 16)
  )
print(p_DO)

ggsave(
  filename = "p_DO_diurnal_cycle.png",
  plot = p_DO,
  width = 8.2,
  height = 5,
  units = "in",
  dpi = 600
)


