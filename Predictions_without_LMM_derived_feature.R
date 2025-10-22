# ============================================================
# Repeated CV: With vs Without LLM features
# Main: threshold-free (AUC) comparison only
# Supplement (optional): 95% specificity sensitivity + Decision curve
# ============================================================

# -----------------------------
# 0) Packages
# -----------------------------
req_pkgs <- c("tidymodels","themis","future","boot","dplyr","purrr","tibble","tidyr")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(tidymodels)
  library(themis)     # step_smote
  library(future)     # parallel plan
  library(boot)       # bootstrap CI
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

has_gg <- requireNamespace("ggplot2", quietly = TRUE)
if (has_gg) library(ggplot2)

# -----------------------------
# 1) Config
# -----------------------------
path_with    <- "/Users/estellepfitzer/Desktop/PhD/1. Papers/3. Data Model/df_clean_w_LLM.csv"
path_without <- "/Users/estellepfitzer/Desktop/PhD/1. Papers/3. Data Model/df_clean_wo_LLM.csv"

N_SEEDS    <- 50
V_FOLDS    <- 10
GRID_SIZE  <- 5

# Sensitivity analyses (supplement), OFF by default
DO_SENS_ANALYSIS <- FALSE    # 95% specificity analysis
DO_DCA           <- FALSE    # decision curve analysis
TARGET_SPEC      <- 0.95
PT_GRID          <- seq(0.05, 0.50, by = 0.01)

# parallelize within tune_grid across resamples
plan(multisession, workers = max(1, parallel::detectCores() - 1))

# -----------------------------
# 2) Utilities
# -----------------------------
# Choose smallest threshold achieving >= target specificity from ROC
threshold_for_spec <- function(df_preds, target_spec = 0.95) {
  roc <- yardstick::roc_curve(df_preds, truth, .pred, event_level = "first") %>%
    dplyr::filter(is.finite(.threshold))
  if (nrow(roc) == 0) return(0.5)
  cand <- roc %>%
    mutate(gap = specificity - target_spec) %>%
    filter(gap >= 0) %>%
    arrange(gap, .threshold)
  if (nrow(cand) > 0) cand$.threshold[1] else roc %>% arrange(desc(specificity)) %>% .$`.threshold`[1]
}

metrics_at_threshold <- function(df_preds, thr) {
  data <- df_preds %>%
    mutate(.pred_class = factor(if_else(.pred >= thr, "Success", "No Success"),
                                levels = c("Success","No Success")))
  tibble(
    sens = yardstick::sens_vec(data$truth, data$.pred_class, estimator = "binary", event_level = "first"),
    spec = yardstick::spec_vec(data$truth, data$.pred_class, estimator = "binary", event_level = "first"),
    ppv  = yardstick::precision_vec(data$truth, data$.pred_class, estimator = "binary", event_level = "first")
  )
}

net_benefit_once <- function(df_preds, pt) {
  data <- df_preds %>%
    mutate(.pred_class = if_else(.pred >= pt, 1L, 0L),
           y = if_else(truth == "Success", 1L, 0L))
  N  <- nrow(data); if (N == 0) return(NA_real_)
  TP <- sum(data$.pred_class == 1L & data$y == 1L)
  FP <- sum(data$.pred_class == 1L & data$y == 0L)
  (TP / N) - (FP / N) * (pt / (1 - pt))
}

# -----------------------------
# 3) Core runner – returns OOF preds for RF+XGB ensemble
# -----------------------------
run_ensemble_oof <- function(df_path, seed, v = 10, grid_size = 5) {
  df <- read.csv(df_path)
  df$status <- factor(df$status, levels = c("Success","No Success"))
  
  set.seed(seed)
  spl   <- initial_split(df, prop = 0.80, strata = status)
  train <- training(spl)
  folds <- vfold_cv(train, v = v, strata = status)
  
  # conditionally label-encode vars present only in w_LLM dataset
  to_label <- c("hq_global_region","hq_country_territory_region",
                "first_financing_deal_class","dht_category","investor_tier")
  present <- intersect(to_label, names(train))
  
  rec <- recipe(status ~ ., data = train)
  if (length(present) > 0) {
    rec <- rec %>% step_mutate_at(all_of(present), fn = ~ as.numeric(factor(.)))
  }
  rec <- rec %>%
    step_zv(all_predictors()) %>%
    step_smote(status)
  
  rf_spec <- rand_forest(mtry = tune(), trees = 100, min_n = tune()) %>%
    set_engine("ranger") %>% set_mode("classification")
  
  xgb_spec <- boost_tree(trees = 100,
                         mtry = tune(), min_n = tune(),
                         learn_rate = tune(), loss_reduction = tune()) %>%
    set_engine("xgboost") %>% set_mode("classification")
  
  rf_wf  <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)
  xgb_wf <- workflow() %>% add_recipe(rec) %>% add_model(xgb_spec)
  
  ctrl <- control_grid(save_pred = TRUE, parallel_over = "resamples")
  
  set.seed(seed)
  rf_res  <- tune_grid(rf_wf,  resamples = folds, grid = grid_size,
                       metrics = metric_set(roc_auc), control = ctrl)
  set.seed(seed)
  xgb_res <- tune_grid(xgb_wf, resamples = folds, grid = grid_size,
                       metrics = metric_set(roc_auc), control = ctrl)
  
  best_rf  <- select_best(rf_res,  metric = "roc_auc")
  best_xgb <- select_best(xgb_res, metric = "roc_auc")
  
  rf_preds  <- collect_predictions(rf_res,  parameters = best_rf)
  xgb_preds <- collect_predictions(xgb_res, parameters = best_xgb)
  
  stopifnot(max(dplyr::count(rf_preds,  id, .row)$n)  == 1)
  stopifnot(max(dplyr::count(xgb_preds, id, .row)$n) == 1)
  
  ens_preds <- rf_preds %>%
    select(id, .row, truth = status, rf_Success = .pred_Success) %>%
    inner_join(
      xgb_preds %>% select(id, .row, xgb_Success = .pred_Success),
      by = c("id", ".row")
    ) %>%
    mutate(.pred = 0.7 * rf_Success + 0.3 * xgb_Success) %>%
    select(id, .row, truth, .pred)
  
  ens_preds
}

# -----------------------------
# 4) Run all seeds, both datasets
# -----------------------------
seeds <- seq_len(N_SEEDS)

get_preds_all <- function(path, seeds, v = V_FOLDS, grid_size = GRID_SIZE, label = c("with","without")) {
  label <- match.arg(label)
  map_dfr(seeds, function(s) {
    preds <- run_ensemble_oof(path, s, v = v, grid_size = grid_size)
    preds %>% mutate(seed = s, method = label, .before = 1)
  })
}

message("Warmup (timing one seed each to estimate total time)…")
t_with  <- system.time({ warm_with  <- run_ensemble_oof(path_with,    seed = 777, v = V_FOLDS, grid_size = GRID_SIZE) })["elapsed"]
t_wo    <- system.time({ warm_wo    <- run_ensemble_oof(path_without, seed = 888, v = V_FOLDS, grid_size = GRID_SIZE) })["elapsed"]
eta_min <- (t_with + t_wo) * N_SEEDS / 60
cat(sprintf("Warmup AUCs (OOF): with=%.3f  without=%.3f\n", 
            yardstick::roc_auc(warm_with, truth, .pred, event_level = "first")$.estimate,
            yardstick::roc_auc(warm_wo,   truth, .pred, event_level = "first")$.estimate))
cat(sprintf("Estimated runtime for %d seeds: ~%.1f minutes\n\n", N_SEEDS, eta_min))

preds_with    <- get_preds_all(path_with,    seeds, label = "with")
preds_without <- get_preds_all(path_without, seeds, label = "without")
preds_all     <- bind_rows(preds_with, preds_without)

# -----------------------------
# 5) PRIMARY ANALYSIS (threshold-free): AUC stats paired by seed
# -----------------------------
auc_by_seed <- preds_all %>%
  group_by(method, seed, id) %>%
  summarise(fold_auc = yardstick::roc_auc_vec(truth, .pred, estimator = "binary", event_level = "first"),
            .groups = "drop") %>%
  group_by(method, seed) %>%
  summarise(mean_auc = mean(fold_auc), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = method, values_from = mean_auc, names_prefix = "auc_") %>%
  mutate(diff = auc_with - auc_without)

# Paired t-test on per-seed mean AUC
t_res <- t.test(auc_by_seed$auc_with, auc_by_seed$auc_without, paired = TRUE)
print(t_res)

# Bootstrap CI for mean diff
boot_mean <- function(x, i) mean(x[i])
boot_out  <- boot(data = auc_by_seed$diff, statistic = boot_mean, R = 5000)
print(boot.ci(boot_out, type = "perc"))

# Permutation p-value (sign flips)
set.seed(42)
obs_diff   <- mean(auc_by_seed$diff)
n_perms    <- 5000
perm_diffs <- replicate(n_perms, mean(auc_by_seed$diff * sample(c(-1,1), N_SEEDS, replace = TRUE)))
p_perm     <- mean(abs(perm_diffs) >= abs(obs_diff))
cat(sprintf("Permutation p-value: %.4f\n", p_perm))

cat(sprintf("\nMean AUC (with):     %.4f\n", mean(auc_by_seed$auc_with)))
cat(sprintf("Mean AUC (without):  %.4f\n", mean(auc_by_seed$auc_without)))
cat(sprintf("Mean diff (with-wo): %.4f\n\n", mean(auc_by_seed$diff)))

# # -----------------------------
# # 6) OPTIONAL SENSITIVITY (Supplement): 95% specificity & PPV
# #     Threshold derived on baseline (WITHOUT LLM) per seed,
# #     then applied to BOTH models for fair comparison.
# # -----------------------------
# if (DO_SENS_ANALYSIS) {
#   # thresholds from baseline only (by seed)
#   thr_tbl <- preds_all %>%
#     filter(method == "without") %>%
#     group_by(seed) %>%
#     group_modify(~ tibble(thr = threshold_for_spec(.x, target_spec = TARGET_SPEC))) %>%
#     ungroup()
#   
#   sens_spec_tbl <- preds_all %>%
#     inner_join(thr_tbl, by = "seed") %>%
#     group_by(method, seed) %>%
#     group_modify(~ metrics_at_threshold(.x, .y$thr[1])) %>%
#     ungroup()
#   
#   sens_wide <- sens_spec_tbl %>%
#     select(method, seed, sens, spec, ppv) %>%
#     tidyr::pivot_wider(names_from = method, values_from = c(sens, spec, ppv), names_sep = "_")
#   
#   t_sens <- t.test(sens_wide$sens_with, sens_wide$sens_without, paired = TRUE)
#   t_ppv  <- t.test(sens_wide$ppv_with,  sens_wide$ppv_without,  paired = TRUE)
#   
#   cat(sprintf("--- Sensitivity @ %.2f specificity (threshold picked on baseline) ---\n", TARGET_SPEC))
#   cat(sprintf("Mean sens (with):    %.4f\n", mean(sens_wide$sens_with)))
#   cat(sprintf("Mean sens (without): %.4f\n", mean(sens_wide$sens_without)))
#   cat(sprintf("Mean diff (with-wo): %.4f\n", mean(sens_wide$sens_with - sens_wide$sens_without)))
#   print(t_sens)
#   
#   cat(sprintf("\n--- PPV @ same operating point ---\n"))
#   cat(sprintf("Mean PPV (with):     %.4f\n", mean(sens_wide$ppv_with)))
#   cat(sprintf("Mean PPV (without):  %.4f\n", mean(sens_wide$ppv_without)))
#   cat(sprintf("Mean diff (with-wo): %.4f\n\n", mean(sens_wide$ppv_with - sens_wide$ppv_without)))
#   print(t_ppv)
# }
# 
# # -----------------------------
# # 7) OPTIONAL SENSITIVITY (Supplement): Decision Curve
# # -----------------------------
# if (DO_DCA) {
#   nb_tbl <- tidyr::expand_grid(method = c("with","without"), seed = seeds, pt = PT_GRID) %>%
#     group_by(method, seed, pt) %>%
#     group_modify(~ {
#       dfm <- preds_all %>% filter(method == .y$method, seed == .y$seed)
#       tibble(NB = net_benefit_once(dfm, .y$pt))
#     }) %>% ungroup()
#   
#   nb_summary <- nb_tbl %>%
#     group_by(method, pt) %>%
#     summarise(
#       mean_NB = mean(NB, na.rm = TRUE),
#       se_NB   = sd(NB,  na.rm = TRUE) / sqrt(sum(!is.na(NB))),
#       lo      = mean_NB - 1.96*se_NB,
#       hi      = mean_NB + 1.96*se_NB,
#       .groups = "drop"
#     )
#   
#   print(head(nb_summary, 6))
#   
#   if (has_gg) {
#     ggplot(nb_summary, aes(x = pt, y = mean_NB, ymin = lo, ymax = hi, linetype = method, fill = method)) +
#       geom_line() +
#       geom_ribbon(alpha = 0.15, colour = NA) +
#       labs(title = "Decision Curve: Mean Net Benefit across seeds",
#            x = "Threshold probability", y = "Net Benefit") +
#       theme_minimal()
#   }
# }
