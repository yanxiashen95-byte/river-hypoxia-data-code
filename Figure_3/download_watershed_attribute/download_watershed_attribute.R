library(sf)
library(dplyr)
library(readr)

# 1. 读取站点经纬度
pts_df <- read_csv("HF_DO_sites.csv")   # 需要有 id, lon, lat
pts <- st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)

# 2. 读取 RiverATLAS
riv <- st_read("D:/Python/R/5 hourly_and_daily_DO/revised_Figure_last/静态属性下载/RiverATLAS_Data_v10_shp/RiverATLAS_v10_shp/RiverATLAS_v10_au.shp", quiet = TRUE)

# 3. 只保留你关心的字段
riv_sub <- riv %>%
  select(
    HYRIV_ID,
    UPLAND_SKM,
    ORD_STRA,
    dis_m3_pyr,
    ele_mt_cav,
    slp_dg_uav,
    tmp_dc_uyr,
    pre_mm_uyr,
    ari_ix_uav,
    wet_pc_ug2,
    for_pc_use,
    crp_pc_use,
    urb_pc_use,
    ppd_pk_uav,
    rdd_mk_uav,
    hdi_ix_cav
  )

# 4. 转到投影坐标系，便于算最近距离
pts_m <- st_transform(pts, 3857)
riv_m <- st_transform(riv_sub, 3857)

# 5. 最近河段匹配
idx <- st_nearest_feature(pts_m, riv_m)
dist_m <- st_distance(pts_m, riv_m[idx, ], by_element = TRUE)

# 6. 合并结果
out <- bind_cols(
  st_drop_geometry(pts),
  st_drop_geometry(riv_m[idx, ]),
  snap_dist_m = as.numeric(dist_m)
)

write_csv(out, "Australia.csv")