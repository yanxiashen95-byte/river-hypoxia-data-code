library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)
library(tibble)
library(purrr)
library(parallel)

# =========================================
# 1. 读取总表
# =========================================
in_file <- "hypoxia_intensity2.csv"
out_dir <- "BRT_results"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

df <- read_csv(in_file, show_col_types = FALSE) %>%
  rename_with(trimws)

# =========================================
# 2. 参数设置
# =========================================
id_col <- "gauge_id"

response_var <- "mean_intensity"

# 手动删除变量（按你当前设置保留）
manual_drop <- c(
  "WT_median",
  "WT_mean",
  "dayl_p75",
  "solar_median",
  "Q_median"
)


na_thresh <- 0.30
corr_cutoff <- 0.90

# -------- 轻量版设置：适合 323站点 × 21变量 --------
tune_seeds <- 1:5
importance_runs <- 500
nfold_cv <- 5
nrounds_max <- 5000
early_stop_round <- 120



# 多线程
nthread_use <- max(1, detectCores() - 1)

# =========================================
# 3. 基本检查
# =========================================
if (!id_col %in% names(df)) {
  stop("找不到站点ID列，请检查 id_col")
}

if (!response_var %in% names(df)) {
  stop("找不到响应变量列，请检查 response_var")
}

dup_n <- sum(duplicated(df[[id_col]]))
cat("重复站点数:", dup_n, "\n")

if (dup_n > 0) {
  stop("当前数据中 gauge_id 有重复，请先整理成每站一行再建模。")
}

# =========================================
# 4. 删除手动排除变量
# =========================================
manual_drop <- manual_drop[manual_drop %in% names(df)]

dat0 <- df %>%
  select(-any_of(manual_drop))

# =========================================
# 5. 提取候选自变量
# =========================================
exclude_cols <- c(id_col, response_var)
predictor_cols <- setdiff(names(dat0), exclude_cols)

is_num <- sapply(dat0[predictor_cols], is.numeric)
predictor_cols <- predictor_cols[is_num]

dat1 <- dat0 %>%
  select(all_of(c(id_col, response_var, predictor_cols))) %>%
  filter(!is.na(.data[[response_var]]))

cat("初始候选变量数:", length(predictor_cols), "\n")

# =========================================
# 6. 去掉缺失率过高变量
# =========================================
na_rate <- sapply(dat1[predictor_cols], function(x) mean(is.na(x)))
predictor_cols <- names(na_rate[na_rate <= na_thresh])

dat2 <- dat1 %>%
  select(all_of(c(id_col, response_var, predictor_cols)))

cat("去掉高缺失后变量数:", length(predictor_cols), "\n")

# =========================================
# 7. 中位数填补缺失
# =========================================
for (v in predictor_cols) {
  medv <- median(dat2[[v]], na.rm = TRUE)
  dat2[[v]][is.na(dat2[[v]])] <- medv
}

# =========================================
# 8. 去掉近零方差变量
# =========================================
nzv_idx <- nearZeroVar(dat2[predictor_cols])
if (length(nzv_idx) > 0) {
  predictor_cols <- predictor_cols[-nzv_idx]
}

dat3 <- dat2 %>%
  select(all_of(c(id_col, response_var, predictor_cols)))

cat("去掉近零方差后变量数:", length(predictor_cols), "\n")

# =========================================
# 9. 去掉高度相关变量
# =========================================
if (length(predictor_cols) >= 2) {
  corr_mat <- cor(dat3[predictor_cols], use = "pairwise.complete.obs")
  high_corr_idx <- findCorrelation(corr_mat, cutoff = corr_cutoff, names = FALSE)
  
  if (length(high_corr_idx) > 0) {
    predictor_cols <- predictor_cols[-high_corr_idx]
  }
}

dat4 <- dat3 %>%
  select(all_of(c(id_col, response_var, predictor_cols)))

cat("去掉高相关后变量数:", length(predictor_cols), "\n")

write_csv(dat4, file.path(out_dir, "dataset_after_filtering.csv"))

# =========================================
# 10. 标准化：所有输入变量 mean = 0, sd = 1
# =========================================
scale_one <- function(x) {
  s <- sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

dat5 <- dat4
dat5[predictor_cols] <- lapply(dat5[predictor_cols], scale_one)

write_csv(dat5, file.path(out_dir, "dataset_after_scaling.csv"))

# =========================================
# 11. xgboost 输入
# =========================================
X <- as.matrix(dat5[predictor_cols])
y <- dat5[[response_var]]

dtrain <- xgb.DMatrix(data = X, label = y)

# =========================================
# 12. 轻量版顺序调参
# =========================================
base_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 5,
  min_child_weight = 2,
  subsample = 0.8,
  colsample_bytree = 0.8,
  gamma = 0,
  lambda = 1,
  alpha = 0,
  nthread = nthread_use
)

score_one_setting <- function(params, dtrain, seeds, nfold = 5,
                              nrounds = 1000, early_stop = 30) {
  res <- map_dfr(seeds, function(s) {
    set.seed(s)
    
    cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      nfold = nfold,
      early_stopping_rounds = early_stop,
      verbose = 0
    )
    
    eval_log <- cv$evaluation_log
    
    # 有些版本/情况下 cv$best_iteration 可能是 NULL
    best_iter_i <- cv$best_iteration
    if (is.null(best_iter_i) || length(best_iter_i) == 0 || is.na(best_iter_i)) {
      best_iter_i <- which.min(eval_log$test_rmse_mean)
    }
    
    best_rmse_i <- min(eval_log$test_rmse_mean, na.rm = TRUE)
    
    tibble(
      seed = s,
      best_iter = as.integer(best_iter_i),
      best_rmse = as.numeric(best_rmse_i)
    )
  })
  
  res %>%
    summarise(
      mean_rmse = mean(best_rmse, na.rm = TRUE),
      sd_rmse = sd(best_rmse, na.rm = TRUE),
      mean_best_iter = round(mean(best_iter, na.rm = TRUE))
    )
}

tune_one_param <- function(param_name, grid_values, current_params, dtrain, seeds) {
  out <- map_dfr(grid_values, function(val) {
    test_params <- current_params
    test_params[[param_name]] <- val
    
    sc <- score_one_setting(
      params = test_params,
      dtrain = dtrain,
      seeds = seeds,
      nfold = nfold_cv,
      nrounds = nrounds_max,
      early_stop = early_stop_round
    )
    
    tibble(
      param = param_name,
      value = val,
      mean_rmse = sc$mean_rmse,
      sd_rmse = sc$sd_rmse,
      mean_best_iter = sc$mean_best_iter
    )
  }) %>%
    arrange(mean_rmse)
  
  best_val <- out$value[1]
  current_params[[param_name]] <- best_val
  
  list(
    best_params = current_params,
    best_iter = out$mean_best_iter[1],
    results = out
  )
}

# 缩小网格
eta_grid <- c(0.03, 0.05, 0.08)
depth_grid <- c(4, 5, 6, 8)
minchild_grid <- c(1, 2, 3, 5)
subsample_grid <- c(0.7, 0.8, 0.9, 1.0)
colsample_grid <- c(0.7, 0.8, 0.9, 1.0)
gamma_grid <- c(0, 0.01, 0.05, 0.1, 0.3)
lambda_grid <- c(0, 0.5, 1, 2, 5)
alpha_grid <- c(0, 0.01, 0.1, 0.5, 1)

res_depth <- tune_one_param("max_depth", depth_grid, base_params, dtrain, tune_seeds)
params1 <- res_depth$best_params

res_minchild <- tune_one_param("min_child_weight", minchild_grid, params1, dtrain, tune_seeds)
params2 <- res_minchild$best_params

res_subsample <- tune_one_param("subsample", subsample_grid, params2, dtrain, tune_seeds)
params3 <- res_subsample$best_params

res_colsample <- tune_one_param("colsample_bytree", colsample_grid, params3, dtrain, tune_seeds)
params4 <- res_colsample$best_params

res_gamma <- tune_one_param("gamma", gamma_grid, params4, dtrain, tune_seeds)
params5 <- res_gamma$best_params

res_lambda <- tune_one_param("lambda", lambda_grid, params5, dtrain, tune_seeds)
params6 <- res_lambda$best_params

res_alpha <- tune_one_param("alpha", alpha_grid, params6, dtrain, tune_seeds)
params7 <- res_alpha$best_params

res_eta <- tune_one_param("eta", eta_grid, params7, dtrain, tune_seeds)
best_params <- res_eta$best_params

final_cv <- score_one_setting(
  params = best_params,
  dtrain = dtrain,
  seeds = tune_seeds,
  nfold = nfold_cv,
  nrounds = nrounds_max,
  early_stop = early_stop_round
)

best_nrounds <- final_cv$mean_best_iter

cat("最终最佳参数：\n")
print(best_params)
cat("最终平均最优树数:", best_nrounds, "\n")
cat("最终CV平均RMSE:", final_cv$mean_rmse, "\n")

write_csv(res_eta$results,       file.path(out_dir, "tuning_eta.csv"))
write_csv(res_depth$results,     file.path(out_dir, "tuning_max_depth.csv"))
write_csv(res_minchild$results,  file.path(out_dir, "tuning_min_child_weight.csv"))
write_csv(res_subsample$results, file.path(out_dir, "tuning_subsample.csv"))
write_csv(res_colsample$results, file.path(out_dir, "tuning_colsample_bytree.csv"))
write_csv(res_gamma$results,  file.path(out_dir, "tuning_gamma.csv"))
write_csv(res_lambda$results, file.path(out_dir, "tuning_lambda.csv"))

# =========================================
# 13. 多次重复建模，计算平均 Gain
# =========================================
importance_list <- vector("list", length = importance_runs)

for (i in seq_len(importance_runs)) {
  set.seed(i)
  
  fit_i <- xgb.train(
    params = best_params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  imp_i <- xgb.importance(
    feature_names = colnames(X),
    model = fit_i
  )
  
  if (nrow(imp_i) == 0) next
  
  importance_list[[i]] <- imp_i %>%
    as_tibble() %>%
    select(Feature, Gain) %>%
    mutate(run = i)
}

importance_all <- bind_rows(importance_list)

importance_summary <- importance_all %>%
  group_by(Feature) %>%
  summarise(
    mean_gain = mean(Gain, na.rm = TRUE),
    sd_gain = sd(Gain, na.rm = TRUE),
    selection_freq = n() / importance_runs,
    .groups = "drop"
  ) %>%
  arrange(desc(mean_gain)) %>%
  mutate(
    mean_gain_pct = mean_gain / sum(mean_gain) * 100,
    cum_gain_pct = cumsum(mean_gain_pct),
    rank = row_number()
  )

write_csv(importance_summary, file.path(out_dir, "BRT_mean_gain_importance.csv"))

# =========================================
# 14. 5-fold out-of-fold 性能评估
# =========================================
set.seed(123)
y_group <- cut(
  y,
  breaks = quantile(y, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)

folds <- createFolds(y_group, k = 5, list = TRUE, returnTrain = FALSE)

oof_pred <- rep(NA_real_, length(y))

for (i in seq_along(folds)) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_along(y), test_idx)
  
  dtr <- xgb.DMatrix(data = X[train_idx, , drop = FALSE], label = y[train_idx])
  dte <- xgb.DMatrix(data = X[test_idx, , drop = FALSE], label = y[test_idx])
  
  fit_cv <- xgb.train(
    params = best_params,
    data = dtr,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  oof_pred[test_idx] <- predict(fit_cv, dte)
}

rmse_val <- sqrt(mean((oof_pred - y)^2, na.rm = TRUE))
mae_val  <- mean(abs(oof_pred - y), na.rm = TRUE)
ss_res   <- sum((y - oof_pred)^2, na.rm = TRUE)
ss_tot   <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
r2_val   <- 1 - ss_res / ss_tot

perf_df <- tibble(
  Metric = c("RMSE", "MAE", "R2"),
  Value  = c(rmse_val, mae_val, r2_val)
)

write_csv(perf_df, file.path(out_dir, "BRT_CV_performance.csv"))

pred_df <- tibble(
  gauge_id = dat5[[id_col]],
  observed = y,
  predicted = oof_pred,
  residual = y - oof_pred
)

write_csv(pred_df, file.path(out_dir, "BRT_oof_predictions.csv"))

# =========================================
# 15. 画图
# =========================================
top_n <- min(20, nrow(importance_summary))

p1 <- importance_summary %>%
  slice(1:top_n) %>%
  ggplot(aes(x = reorder(Feature, mean_gain_pct), y = mean_gain_pct)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean Gain (%)",
    title = paste0("Top ", top_n, " drivers of ", response_var)
  ) +
  theme_classic(base_size = 14)

print(p1)

ggsave(
  file.path(out_dir, paste0("Top", top_n, "_gain_importance.png")),
  p1, width = 8, height = 6, dpi = 300
)

p2 <- ggplot(pred_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = paste0("5-fold CV: Observed vs Predicted (", response_var, ")"),
    subtitle = paste0(
      "RMSE = ", round(rmse_val, 3),
      ", MAE = ", round(mae_val, 3),
      ", R2 = ", round(r2_val, 3)
    )
  ) +
  theme_classic(base_size = 14)

print(p2)

ggsave(
  file.path(out_dir, "BRT_observed_vs_predicted.png"),
  p2, width = 6.5, height = 5.5, dpi = 300
)
