# ============================================================
# Digital Health Companies Success Prediction - Final
# ============================================================

# -------------------------------
# 0. Load Required Libraries
# -------------------------------
library(tidymodels)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(smotefamily)
library(themis)
library(doParallel)
library(future)
library(yardstick)
library(rlang)
library(tidyr)
library(knitr)
library(shapviz)

# -----------------------------------------
# 1. Train/Test Split & CV Setup
# -----------------------------------------
df <- read.csv("/Users/estellepfitzer/Desktop/PhD/1. Papers/3. Data Model/df_clean_w_LLM.csv")


set.seed(123)
data_split <- initial_split(df, prop = 0.80, strata = status)
train_data  <- training(data_split)
test_data   <- testing(data_split)
df_folds <- vfold_cv(train_data, v = 10, strata = status)

# -----------------------------------------
# 2. Recipes
# -----------------------------------------
model_recipe_label <- recipe(status ~ ., data = train_data) %>%
  update_role(row_id, new_role = "id") %>%  # Exclude from predictors
  step_mutate_at(
    c("hq_global_region", "hq_country_territory_region",
      "first_financing_deal_class","dht_category","investor_tier"),
    fn = ~ as.numeric(factor(.))
  ) %>%
  step_zv(all_predictors()) %>%
  step_smote(status)

model_recipe_one_hot <- recipe(status ~ ., data = train_data) %>%
  step_string2factor(all_nominal_predictors(), -all_outcomes()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_lincomb(all_predictors()) %>%
  step_zv(all_predictors())

# -----------------------------------------
# 3. Random Forest – CV and Metrics
# -----------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)
ctrl <- control_grid(save_pred = TRUE, parallel_over = "resamples")

rf_spec <- rand_forest(mtry = tune(), trees = 100, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(rf_spec)

set.seed(123)
rf_tune_res <- tune_grid(
  rf_wf,
  resamples = df_folds,
  grid      = 5,
  metrics   = metric_set(accuracy, roc_auc),
  control   = ctrl
)

# >>> FIX: pick best params, then collect ONLY those OOF predictions
best_rf_params <- select_best(rf_tune_res, metric = "roc_auc")

rf_cv_preds <- collect_predictions(
  rf_tune_res,
  parameters = best_rf_params
) %>%
  mutate(
    status      = factor(status, levels = c("Success", "No Success")),
    .pred_class = factor(.pred_class, levels = c("Success", "No Success"))
  )

# Safety: ensure one row per (fold, row)
stopifnot(max(dplyr::count(rf_cv_preds, id, .row)$n) == 1)

rf_fold_metrics <- rf_cv_preds %>%
  group_by(id) %>%
  summarise(
    auc      = roc_auc_vec(truth = status, estimate = .pred_Success, estimator = "binary", event_level = "first"),
    accuracy = accuracy_vec(truth = status, estimate = .pred_class),
    f1       = f_meas_vec(truth = status, estimate = .pred_class, estimator = "macro"),
    sens_pos = sens_vec(truth = status, estimate = .pred_class, estimator = "binary", event_level = "first"),
    spec_pos = spec_vec(truth = status, estimate = .pred_class, estimator = "binary", event_level = "first"),
    .groups  = "drop"
  )

rf_summary <- rf_fold_metrics %>%
  summarise(
    across(
      where(is.numeric),
      list(mean   = mean, median = median, sd = sd),
      .names = "{.col}_{.fn}"
    )
  )

# -----------------------------------------
# 3.1 Finalize & Fit RF on Full Training Data
# -----------------------------------------
final_rf_wf  <- finalize_workflow(rf_wf, best_rf_params)
final_rf_fit <- fit(final_rf_wf, data = train_data)
rf_model     <- extract_fit_engine(final_rf_fit)

# -----------------------------------------
# 4. XGBoost – CV and Metrics 
# -----------------------------------------
xgb_spec <- boost_tree(
  trees          = 100,
  mtry           = tune(),
  min_n          = tune(),
  learn_rate     = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(xgb_spec)

xgb_grid <- extract_parameter_set_dials(xgb_spec) %>%
  finalize(train_data) %>%
  grid_random(size = 5)

set.seed(123)
xgb_tune_res <- tune_grid(
  xgb_wf,
  resamples = df_folds,
  grid      = xgb_grid,
  metrics   = metric_set(accuracy, roc_auc),
  control   = ctrl
)

best_xgb_params <- select_best(xgb_tune_res, metric = "roc_auc")

xgb_cv_preds <- collect_predictions(
  xgb_tune_res,
  parameters = best_xgb_params
) %>%
  mutate(
    status      = factor(status, levels = c("Success", "No Success")),
    .pred_class = factor(.pred_class, levels = c("Success", "No Success"))
  )

# Safety: ensure one row per (fold, row)
stopifnot(max(dplyr::count(xgb_cv_preds, id, .row)$n) == 1)

xgb_fold_metrics <- xgb_cv_preds %>%
  group_by(id) %>%
  summarise(
    auc      = roc_auc_vec(status, .pred_Success, estimator = "binary", event_level = "first"),
    accuracy = accuracy_vec(status, .pred_class),
    f1       = f_meas_vec(status, .pred_class, estimator = "macro"),
    sens_pos = sens_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    spec_pos = spec_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    .groups  = "drop"
  )

xgb_summary <- xgb_fold_metrics %>%
  summarise(
    across(
      where(is.numeric),
      list(mean   = mean, median = median, sd = sd),
      .names = "{.col}_{.fn}"
    )
  )

# -----------------------------------------
# 4.1 Finalize & Fit XGBoost on Full Training Data
# -----------------------------------------
final_xgb_wf  <- finalize_workflow(xgb_wf, best_xgb_params)
final_xgb_fit <- fit(final_xgb_wf, data = train_data)
xgb_model     <- extract_fit_engine(final_xgb_fit)

# -----------------------------------------
# 5. Logistic Regression – CV and Metrics
# -----------------------------------------
log_spec <- multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

log_wf <- workflow() %>%
  add_recipe(model_recipe_one_hot) %>%
  add_model(log_spec)

log_grid <- crossing(
  penalty = 10^seq(-6, 1, length.out = 8),
  mixture = seq(0, 1, by = 0.25)
)

set.seed(123)
log_tune_res <- tune_grid(
  log_wf,
  resamples = df_folds,
  grid      = log_grid,
  metrics   = metric_set(accuracy, roc_auc),
  control   = ctrl
)

log_cv_preds <- collect_predictions(log_tune_res) %>%
  mutate(
    status      = factor(status, levels = c("Success", "No Success")),
    .pred_class = factor(.pred_class, levels = c("Success", "No Success"))
  )

log_fold_metrics <- log_cv_preds %>%
  group_by(id) %>%
  summarise(
    accuracy = accuracy_vec(status, .pred_class),
    f1       = f_meas_vec(status, .pred_class, estimator = "macro"),
    sens_pos = sens_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    spec_pos = spec_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    auc      = roc_auc_vec(status, .pred_Success, estimator = "binary", event_level = "first"),
    .groups  = "drop"
  )

log_summary <- log_fold_metrics %>%
  summarise(
    across(
      where(is.numeric),
      list(mean   = mean, median = median, sd = sd),
      .names = "{.col}_{.fn}"
    )
  )


# -----------------------------------------
# 6. RF + XGB Ensemble – CV and Metrics
# -----------------------------------------
rf_cv_preds  <- rf_cv_preds  %>% rename_with(~ str_replace_all(.x, " ", "_"))
xgb_cv_preds <- xgb_cv_preds %>% rename_with(~ str_replace_all(.x, " ", "_"))

ensemble_cv_preds <- rf_cv_preds %>%
  select(id, .row, status,
         rf_Success    = .pred_Success,
         rf_No_Success = .pred_No_Success) %>%
  inner_join(
    xgb_cv_preds %>%
      select(id, .row,
             xgb_Success    = .pred_Success,
             xgb_No_Success = .pred_No_Success),
    by = c("id", ".row")
  ) %>%
  mutate(
    status          = factor(status, levels = c("Success", "No Success")),
    .pred_Success   = 0.7 * rf_Success    + 0.3 * xgb_Success,
    .pred_No_Success= 0.7 * rf_No_Success + 0.3 * xgb_No_Success,
    .pred_class     = factor(
      if_else(.pred_Success >= .pred_No_Success,
              "Success", "No Success"),
      levels = c("Success", "No Success")
    )
  )

ensemble_fold_metrics <- ensemble_cv_preds %>%
  group_by(id) %>%
  summarise(
    auc      = roc_auc_vec(status, .pred_Success, estimator = "binary", event_level = "first"),
    accuracy = accuracy_vec(status, .pred_class),
    f1       = f_meas_vec(status, .pred_class, estimator = "macro"),
    sens_pos = sens_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    spec_pos = spec_vec(status, .pred_class, estimator = "binary", event_level = "first"),
    .groups  = "drop"
  )

ensemble_summary <- ensemble_fold_metrics %>%
  summarise(
    across(
      where(is.numeric),
      list(mean   = mean, median = median, sd = sd),
      .names = "{.col}_{.fn}"
    )
  )

# -----------------------------------------
# 7. Print All Summary Tables
# -----------------------------------------
print("✅ RF Fold Metrics:"); print(rf_fold_metrics)
print("📊 RF Summary Stats:"); print(rf_summary)
print("✅ XGB Fold Metrics:"); print(xgb_fold_metrics)
print("📊 XGB Summary Stats:"); print(xgb_summary)
print("✅ LOG Fold Metrics:"); print(log_fold_metrics)
print("📊 LOG Summary Stats:"); print(log_summary)
print("✅ ENS Fold Metrics:"); print(ensemble_fold_metrics)
print("📊 ENS Summary Stats:"); print(ensemble_summary)


# -----------------------------------------
# 8. Feature Importance (Corrected SHAP)
# -----------------------------------------

# 8.1 Prep processed test set
prepped_recipe   <- prep(model_recipe_label)
X_test_processed <- bake(prepped_recipe, new_data = test_data, all_predictors())

# 8.2 RF SHAP via ranger engine
set.seed(123)
rf_shap_success <- fastshap::explain(
  object       = rf_model,
  X            = X_test_processed,
  pred_wrapper = function(model, newdata) {
    predict(model, data = newdata, type = "response")$predictions[, "Success"]
  },
  nsim   = 10,
  adjust = TRUE
)

rf_shap_viz <- shapviz(as.matrix(rf_shap_success), X_test_processed)
sv_importance(rf_shap_viz, kind = "beeswarm", max_display = 20)

# 8.3 XGBoost SHAP via xgboost engine
set.seed(123)
xgb_shap_success <- fastshap::explain(
  object       = xgb_model,
  X            = X_test_processed,
  pred_wrapper = function(model, newdata) {
    predict(model, newdata = as.matrix(newdata))
  },
  nsim   = 10,
  adjust = TRUE
)

xgb_shap_viz <- shapviz(as.matrix(xgb_shap_success), X_test_processed)
sv_importance(xgb_shap_viz, kind = "beeswarm", max_display = 20)

# 8.4 Ensemble SHAP (90% RF + 10% XGB)
ensemble_shap_success <- 0.7 * rf_shap_success + 0.3 * xgb_shap_success
ensemble_shap_viz     <- shapviz(as.matrix(ensemble_shap_success), X_test_processed)
sv_importance(ensemble_shap_viz, kind = "beeswarm", max_display = 20)




# -----------------------------------------
# 14. Benchmarking: Fixed 8 Companies (Given List)
# -----------------------------------------
library(dplyr)
library(tidyr)
library(knitr)
library(yardstick)

# Use exactly these 8 companies (order kept)
selected_companies <- c(
  "OneMedNet (NAS: ONMD)",
  "Cara Care",
  "Nurse-1-1",
  "openDoctor",
  "87%",
  "Aprenda Systems",
  "Elma",
  "Medtep"
)

# STEP 1: Pull rows from df_raw for those companies
sampled_raw_info <- df_raw %>%
  filter(companies %in% selected_companies) %>%
  distinct(companies, .keep_all = TRUE) %>%               # guard against duplicates
  mutate(companies = factor(companies, levels = selected_companies)) %>%
  arrange(companies) %>%
  select(row_id, companies)

# STEP 2: Retrieve corresponding true labels and predictions
sampled_results <- sampled_raw_info %>%
  left_join(test_data %>% select(row_id, status), by = "row_id") %>%
  left_join(
    ensemble_preds_weighted %>% select(row_id, pred_class, pred_success, pred_no_success),
    by = "row_id"
  ) %>%
  mutate(
    status = factor(status, levels = c("Success", "No Success")),
    pred_class = factor(pred_class, levels = c("Success", "No Success")),
    sample_id = LETTERS[1:n()]
  )

# STEP 3: Force exactly 4 predictions of each class by highest probability
k <- 4

top_success_ids <- sampled_results %>%
  arrange(desc(pred_success)) %>%
  slice(1:k) %>%
  pull(row_id)

top_no_success_ids <- sampled_results %>%
  arrange(desc(pred_no_success)) %>%
  slice(1:k) %>%
  pull(row_id)

sampled_results <- sampled_results %>%
  mutate(
    forced_pred_class = case_when(
      row_id %in% top_success_ids    ~ "Success",
      row_id %in% top_no_success_ids ~ "No Success",
      TRUE                           ~ NA_character_
    ),
    forced_pred_class = factor(forced_pred_class, levels = c("Success", "No Success"))
  )

# STEP 4: Display and compute accuracy
cat("📊 Benchmarking Set: 8 Fixed Companies (Given List)\n\n")

sampled_results %>%
  select(sample_id, companies, status, pred_class, forced_pred_class, pred_success, pred_no_success) %>%
  knitr::kable(
    digits = 3,
    col.names = c("ID", "Company", "Actual", "Predicted", "Forced Predicted", "P(Success)", "P(No Success)")
  ) %>%
  print()

benchmark_accuracy <- yardstick::accuracy(
  sampled_results,
  truth    = status,
  estimate = forced_pred_class
)
print(benchmark_accuracy)
