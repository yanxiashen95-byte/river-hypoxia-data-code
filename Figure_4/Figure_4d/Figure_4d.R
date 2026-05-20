#=========================== underestimation_landuse_landuse =========================
#=========================== underestimation_landuse_landuse =========================
#=========================== underestimation_landuse_landuse =========================
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

in_csv  <- "underestimation_landuse.csv"
out_png <- "box_per_by_landuse_group_halfviolin1.png"

# -------------------------
# 1) Read & clean
# -------------------------
df <- read_csv(in_csv, show_col_types = FALSE)

df_use <- df %>%
  mutate(
    per = suppressWarnings(as.numeric(per)),
    landuse_group = as.character(landuse_group)
  ) %>%
  filter(!is.na(per), !is.na(landuse_group), landuse_group != "") %>%
  # per: if 0–100 convert to 0–1; if already 0–1 keep
  mutate(
    per = ifelse(per > 1, per / 100, per),
    per = pmin(pmax(per, 0), 1),
    landuse_group = factor(
      landuse_group,
      levels = c("Natural-dominated", "Ag-dominated", "Urban-dominated", "Mixed")
    )
  ) %>%
  filter(!is.na(landuse_group)) %>%
  mutate(
    group_num = as.numeric(landuse_group)   # 1..4
  )

# -------------------------
# 2) Build right-half density polygons
# -------------------------
density_data <- dplyr::tibble()

for (g in levels(df_use$landuse_group)) {
  vals <- df_use$per[df_use$landuse_group == g]
  vals <- vals[is.finite(vals)]
  
  # skip if too few unique values
  if (length(vals) < 10 || length(unique(vals)) < 2) next
  
  # density on [0,1]; bw can be tuned
  dens <- stats::density(vals, bw = 0.04, adjust = 1.0, from = 0, to = 1, na.rm = TRUE)
  
  base_x <- unique(df_use$group_num[df_use$landuse_group == g])[1]
  x0     <- base_x + 0.25          # ✅ put the half-violin to the RIGHT
  width  <- 0.42                    # ✅ half-violin max width (tune 0.30~0.50)
  
  df_dens <- tibble(
    landuse_group = g,
    x = x0 + dens$y / max(dens$y) * width,
    y = dens$x
  )
  
  # close polygon back to baseline x0
  df_dens <- bind_rows(
    df_dens,
    tibble(landuse_group = g, x = x0, y = max(dens$x)),
    tibble(landuse_group = g, x = x0, y = min(dens$x))
  )
  
  density_data <- bind_rows(density_data, df_dens)
}

# -------------------------
# 3) Colors (you can change)
# -------------------------
fill_cols <- c(
  "Natural-dominated" = "#fdae61",
  "Ag-dominated"      = "#fdae61",
  "Urban-dominated"   = "#fdae61",
  "Mixed"             = "#fdae61"
)

# -------------------------
# 4) Plot
# -------------------------
p <- ggplot() +
  # right half-violin (polygon)
  geom_polygon(
    data = density_data,
    aes(x = x, y = y, fill = landuse_group),
    alpha = 0.30,
    color = NA
  ) +
  # centered boxplot (✅ no tilt)
  geom_boxplot(
    data = df_use,
    aes(x = group_num, y = per, fill = landuse_group),
    width = 0.32,
    alpha = 0.85,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.85
  ) +
  scale_fill_manual(values = fill_cols) +
  scale_x_continuous(
    name = NULL,
    breaks = 1:4,
    labels = c("Undeveloped", "Agriculture", "Urban", "Mixed")
  ) +
  scale_y_continuous(
    name = "Underestimation (%)",
    labels = function(x) x * 100,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    axis.text.x = element_text(size = 22, angle = 10, hjust = 0.5),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 22)
  )

print(p)

ggsave(out_png, p, width = 7, height = 4.5, dpi = 600, bg = "white")
message("Saved: ", out_png)


#========= 输出中位数和偏差
stat_tbl <- df_use %>%
  group_by(landuse_group) %>%
  summarise(
    n = n(),
    median_per = median(per, na.rm = TRUE),
    sd_per     = sd(per, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    median_pct = 100 * median_per,
    sd_pct     = 100 * sd_per
  )

print(stat_tbl)

cat("\nMedian ± SD underestimation by land-use group:\n")
for (i in seq_len(nrow(stat_tbl))) {
  cat(
    as.character(stat_tbl$landuse_group[i]),
    ": n=", stat_tbl$n[i],
    ", median ± SD = ",
    sprintf("%.1f ± %.1f%%", stat_tbl$median_pct[i], stat_tbl$sd_pct[i]),
    "\n",
    sep = ""
  )
}
