# ============================================================
# Digital Health Companies Success Prediction
# ============================================================

# -------------------------------
# 0. Load Required Libraries
# -------------------------------

library(tidymodels)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)  # often helpful for date manipulations
library(smotefamily)
library(vip)
library(caret)
library(themis)
library(doParallel)
library(future)
library(yardstick)
library(rlang)
library(shapviz)
library(pROC)
library(geosphere)
library(maps)
library(readr)
library(tidyr)
library(knitr)

# -------------------------------
# 1. Load and Prepare Dataset
# -------------------------------

file_path <- "/Users/estellepfitzer/Desktop/PhD/1. Papers/3. Data Model/02 Data/EP_PitchBook_Cleaned_2025_06.csv"
df_raw <- read_csv(file_path)

# Clean column names (lowercase, underscores)
df_raw <- df_raw %>% janitor::clean_names()

# Convert Excel-style date columns
excel_date_cols <- c("first_financing_date", "last_financing_date", 
                     "last_updated_date", "last_known_valuation_date")

df_raw[excel_date_cols] <- lapply(
  df_raw[excel_date_cols], 
  function(x) as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
)


#Clean Categorical Strings (Prevent Duplicated Dummy Names)


clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$")
}

df_raw <- df_raw %>%
  mutate(across(
    c(hq_city, hq_country_territory_region, hq_global_region,
      first_financing_deal_class, dht_category),
    ~ clean_text(.)
  ))

# Normalize ownership status into a new factor 'status'
df_raw <- df_raw %>%
  mutate(
    status = case_when(
      ownership_status %in% c("Acquired/Merged", 
                              "Acquired/Merged (Operating Subsidiary)", 
                              "Publicly Held") ~ "Exited",
      ownership_status %in% c("In IPO Registration", 
                              "Privately Held (backing)", 
                              "Privately Held (no backing)") ~ "Operating",
      ownership_status == "Out of Business" ~ "Out of Business",
      TRUE ~ ownership_status
    )
  ) %>%
  select(-ownership_status)


# -------------------------------
# 2. Drop Irrelevant or Unwanted Columns
# -------------------------------
drop_cols <- c(
  "company_id", "latest_note", "latest_note_author",
  "company_former_name", "company_also_known_as", "company_legal_name",
  "registration_number", "company_registry", "competitors", "pb_id",
  "description", "primary_industry_sector", "primary_industry_group",
  "primary_industry_code", "all_industries", "verticals", "keywords",
  "company_financing_status", "business_status",
  "universe", "website", "linked_in_url", "exchange", "ticker",
  "parent_company", "daily_updates", "weekly_updates",
  "gross_profit", "net_income", "enterprise_value",
  "ebit", "market_cap", "net_debt",
  "primary_contact_pb_id", "primary_contact",
  "primary_contact_title", "primary_contact_email", "primary_contact_phone",
  "hq_location", "hq_address_line_1", "hq_address_line_2",
  "hq_state_province", "hq_post_code",
  "hq_phone", "hq_fax", "hq_email", 
  "hq_global_sub_region", "financing_status_note", "active_investors",
  "acquirers", "other_investors", "active_investors_websites",
  "former_investors_websites", "other_investors_websites", "general_services",
  "services_on_a_deal", "first_financing_size_status",
  "first_financing_valuation_status", "first_financing_deal_type", 
  "first_financing_deal_type_2", "first_financing_deal_type_3", 
  "first_financing_status", "last_financing_size", "last_financing_size_status", 
  "last_financing_valuation", "last_financing_valuation_status", 
  "last_financing_deal_type", "last_financing_deal_type_2", 
  "last_financing_deal_type_3", "last_financing_deal_class", 
  "last_financing_debt_date", "last_financing_debt_size", 
  "last_financing_debt", "last_financing_status", 
  "growth_rate", "growth_rate_percentile", "growth_rate_change",
  "growth_rate_percent_change", "web_growth_rate", "web_growth_rate_percentile",
  "size_multiple","size_multiple_percentile", "size_multiple_change",
  "size_multiple_percent_change", "web_size_multiple", 
  "web_size_multiple_percentile", "profile_data_source", 
  "opportunity_score", "success_class", "success_probability",
  "no_exit_probability", "predicted_exit_type", "ipo_probability", 
  "m_a_probability", "last_known_valuation", "last_known_valuation_deal_type",
  "emerging_spaces", "clinical_trials_matching_criteria",
  "last_valuation_step_up", "view_company_online", "last_updated_date", "companies",
  "similar_web_size_multiple", "similar_web_size_multiple_percentile",
  "similar_web_unique_visitors", "similar_web_unique_visitors_change",
  "similar_web_unique_visitors_percent_change",
  "similar_web_growth_rate", "similar_web_growth_rate_percentile", "majestic_size_multiple",
  "majestic_size_multiple_percentile", 
  "majestic_referring_domains_change", "majestic_referring_domains_percent_change",
  "majestic_growth_rate", "majestic_referring_domains", "majestic_growth_rate_percentile","top_cpc_codes","first_financing_debt"
)

df <- df_raw %>%
  select(-any_of(drop_cols))

# -------------------------------
# 3. Custom Feature Engineering
# -------------------------------

# --- 3A. Functions to compute employee CAGR & YoY growth ---
compute_cagr <- function(history) {
  if (is.na(history)) return(NA_real_)
  # history example: "2018:100, 2019:120, 2020:150"
  records <- str_split(history, ",\\s*")[[1]]
  
  # Each record is "yyyy: value"
  df_year_val <- tibble(record = records) %>%
    separate(record, into = c("year", "count"), sep = ":\\s*") %>%
    mutate(across(everything(), as.numeric)) %>%
    arrange(year)
  
  if (nrow(df_year_val) < 2 || df_year_val$count[1] == 0) {
    return(NA_real_)
  }
  
  years_elapsed <- df_year_val$year[nrow(df_year_val)] - df_year_val$year[1]
  cagr <- (df_year_val$count[nrow(df_year_val)] / df_year_val$count[1])^(1 / years_elapsed) - 1
  return(round(cagr, 4))
}

compute_yoy <- function(history) {
  if (is.na(history)) return(NA_real_)
  records <- str_split(history, ",\\s*")[[1]]
  
  df_year_val <- tibble(record = records) %>%
    separate(record, into = c("year", "count"), sep = ":\\s*") %>%
    mutate(across(everything(), as.numeric)) %>%
    arrange(year)
  
  if (nrow(df_year_val) < 2) return(NA_real_)
  
  # Compare last year to the year before
  last_val <- df_year_val$count[nrow(df_year_val)]
  prev_val <- df_year_val$count[nrow(df_year_val)-1]
  if (prev_val == 0) return(NA_real_)
  
  yoy <- (last_val - prev_val) / prev_val
  return(round(yoy, 4))
}

df <- df %>%
  mutate(
    employee_cagr        = map_dbl(employee_history, compute_cagr),
    employee_yoy_growth  = map_dbl(employee_history, compute_yoy)
  ) %>%
  select(-employee_history)

# --- 3B. Investor counts ---
df <- df %>%
  mutate(
    investor_number = case_when(
      status == "Operating" ~ number_active_investors,
      TRUE ~ map_int(former_investors, function(x) {
        if (is.na(x)) 0 else length(str_split(x, ",\\s*")[[1]])
      })
    )
  ) %>%
  mutate(
    investor_number = ifelse(is.na(investor_number), 0, investor_number)
  ) %>%
  select(-number_active_investors, -former_investors)

# --- 3C. Replace NA with 0 in specific numeric columns ---
df <- df %>%
  mutate(across(
    c(total_clinical_trials, total_patent_documents, total_patent_families, 
      total_raised, active_patent_documents, number_of_ucc_filings),
    ~replace(., is.na(.), 0))
  )

# --- 3D. Convert/clean up 'fiscal_year' from 'fiscal_period' ---
df <- df %>%
  mutate(
    fiscal_year = case_when(
      str_detect(fiscal_period, "^FY\\s*\\d{4}$") ~ as.numeric(str_remove(fiscal_period, "FY\\s*")),
      str_detect(fiscal_period, "^TTM\\s*\\w+\\d{4}$") ~ as.numeric(str_extract(fiscal_period, "\\d{4}$")),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-fiscal_period)

# --- 3E. Remove outdated revenue/EBITDA if last_known_valuation_date < fiscal_year ---
df <- df %>%
  mutate(
    is_outdated = if_else(
      !is.na(last_known_valuation_date) & !is.na(fiscal_year) &
        year(last_known_valuation_date) < fiscal_year,
      TRUE, FALSE
    ),
    revenue = if_else(is_outdated, NA_real_, revenue),
    ebitda  = if_else(is_outdated, NA_real_, ebitda)
  ) %>%
  select(-last_known_valuation_date, -fiscal_year, -is_outdated)

# --- 3F. Financing timing features ---
df <- df %>%
  filter(!is.na(year_founded)) %>%
  mutate(
    year_founded = as.numeric(year_founded),
    no_first_financing = as.integer(is.na(first_financing_date)),
    no_second_financing = as.integer(is.na(last_financing_date)),
    time_to_first_financing = if_else(
      !is.na(first_financing_date),
      year(first_financing_date) - year_founded,
      999
    ),
    time_between_financings = if_else(
      !is.na(first_financing_date) & !is.na(last_financing_date),
      as.numeric(difftime(last_financing_date, first_financing_date, units = "days"))/365.25,
      999
    )
  ) %>%
  select(-first_financing_date, -last_financing_date)

# --- 3G. Hub Features ---

# -------------------------------
# 4. Handle Missing Data / Imputation
# -------------------------------
# Create indicator columns
df <- df %>%
  mutate(
    # has_revenue_data           = ifelse(is.na(revenue), 0, 1),
    has_ebitda_data            = ifelse(is.na(ebitda), 0, 1),
    has_employee_cagr_data          = ifelse(is.na(employee_cagr), 0, 1),
    # has_employee_yoy_growth_data    = ifelse(is.na(employee_yoy_growth), 0, 1),
    has_revenue_growth_data        = ifelse(is.na(revenue_growth_percent), 0, 1),
    has_employee_data          = ifelse(is.na(employees), 0, 1)
  )

# Impute missing numeric values using median
median_revenue <- median(df$revenue, na.rm = TRUE)
median_ebitda  <- median(df$ebitda, na.rm = TRUE)
median_employees <- median(df$employees, na.rm = TRUE)

df <- df %>%
  mutate(
    revenue  = ifelse(is.na(revenue), median_revenue, revenue),
    ebitda   = ifelse(is.na(ebitda), median_ebitda, ebitda),
    employees= ifelse(is.na(employees), median_employees, employees)
  )

# Replace NAs with 0 in certain growth-related columns
df <- df %>%
  mutate(
    revenue_growth_percent     = replace_na(revenue_growth_percent, 0),
    employee_cagr             = replace_na(employee_cagr, 0),
    employee_yoy_growth       = replace_na(employee_yoy_growth, 0),
    first_financing_size      = replace_na(first_financing_size, 0),
    dtx_reimbursement = replace_na(dtx_reimbursement, 0),
    dht_reimbursement = replace_na(dht_reimbursement, 0),
    hq_country_territory_region = replace_na(hq_country_territory_region, "Unknown"),
    hq_global_region = replace_na(hq_global_region, "Unknown"),
    dht_category = case_when(
      is.na(dht_category) | dht_category == 0 ~ "Other",
      TRUE ~ as.character(dht_category)
    )
  )


# df <- df %>%
#   mutate(across(
#     c(
#       majestic_referring_domains, majestic_growth_rate_percentile),
#     ~replace(., is.na(.), 0)
#   ))

# Replace an empty or NA "first_financing_deal_class" with "No financing"
df$first_financing_deal_class[is.na(df$first_financing_deal_class) | 
                                df$first_financing_deal_class == ""] <- "No financing"

# -------------------------------
# 5. Remove Columns with High Missingness (>80%) if any
# -------------------------------
missing_pct <- colMeans(is.na(df)) * 100
high_missing_cols <- names(missing_pct[missing_pct > 80])
df <- df %>% select(-any_of(high_missing_cols))

# Convert target to factor with consistent levels
df$status <- as.factor(df$status)

# -------------------------------
# 6. Optional: Remove Highly Correlated Features
# -------------------------------
# (You can do this step or skip, depending on your domain preference)
numeric_df <- df %>%
  select(where(is.numeric))

corr_mat <- cor(numeric_df, use = "complete.obs")
high_cor <- findCorrelation(corr_mat, cutoff = 0.9)
names(numeric_df)[high_cor]
# Remove those columns from df
df <- df %>% select(-any_of(names(numeric_df)[high_cor]))

# Create a row ID in the raw dataset for later matching
df_raw  <- df_raw  %>% mutate(row_id = row_number())
df       <- df       %>% mutate(row_id = row_number())

# -----------------------------------------
# 7. Train/Test Split & CV Setup
# -----------------------------------------
# Instead of doing vfold_cv(df, ...),
# we first split the data into train/test, 
# then do cross-validation on the training set.

set.seed(123)
data_split <- initial_split(df, prop = 0.80, strata = status)
train_data <- training(data_split)
test_data  <- testing(data_split)

df_folds <- vfold_cv(train_data, v = 5, strata = status)

# B) RECIPE for LABEL ENCODING (RF & XGBoost) on train_data
model_recipe_label <- recipe(status ~ ., data = train_data) %>%
  step_mutate_at(
    c("hq_country_territory_region", "hq_global_region", "first_financing_deal_class", "dht_category"),
    fn = ~ as.numeric(factor(.))
  ) %>%
  step_zv(all_predictors()) %>%
  step_smote(status)

# C) RECIPE for ONE-HOT ENCODING (Logistic Regression) on train_data
model_recipe_one_hot <- recipe(status ~ ., data = train_data) %>%
  step_string2factor(all_nominal_predictors(), -all_outcomes()) %>%
  step_unknown(all_nominal_predictors()) %>%       # handle missing factor levels
  step_novel(all_nominal_predictors()) %>%         # handle new/unseen factor levels
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_lincomb(all_predictors()) %>%               # remove perfect linear combos
  step_zv(all_predictors())     


# -----------------------------------------
# 8. Random Forest
# -----------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)
ctrl <- control_grid(save_pred = TRUE, parallel_over = "resamples")

# (1) 5-fold cross-validation: df_folds is on train_data
rf_grid_small <- grid_random(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 10)),
  size = 5
)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 100,
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_workflow_label <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(rf_spec)

set.seed(123)
rf_tune_res <- tune_grid(
  rf_workflow_label,
  resamples = df_folds,
  grid = rf_grid_small,
  metrics = metric_set(accuracy, roc_auc),
  control = ctrl
)

rf_best <- select_best(rf_tune_res, metric = "accuracy")

final_rf_spec <- rand_forest(
  mtry  = rf_best$mtry,
  trees = 500,      # More trees for final model
  min_n = rf_best$min_n
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

final_rf_workflow <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(final_rf_spec)

# Train final RF on the *entire training set*
final_rf_fit <- fit(final_rf_workflow, data = train_data)


# -----------------------------------------
# 9. XGBoost
# -----------------------------------------
xgb_grid_small <- grid_random(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 10)),
  learn_rate(range = c(-3, -1)),    # 0.001 -> 0.1
  loss_reduction(range = c(-1, 2)), # 0.1 -> 100
  size = 8
)

xgb_spec <- boost_tree(
  trees = 50,  # fewer for tuning
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_workflow_label <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(xgb_spec)

plan(multisession, workers = 4)  # or your preference
ctrl <- control_grid(parallel_over = "resamples")

set.seed(123)
xgb_tune_res <- tune_grid(
  xgb_workflow_label,
  resamples = df_folds,
  grid = xgb_grid_small,
  metrics = metric_set(accuracy, roc_auc),
  control = ctrl
)

xgb_best <- select_best(xgb_tune_res, metric = "accuracy")

# Final XGBoost with more trees
xgb_spec_final <- boost_tree(
  trees = 500,
  mtry = xgb_best$mtry,
  min_n = xgb_best$min_n,
  learn_rate = xgb_best$learn_rate,
  loss_reduction = xgb_best$loss_reduction
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_final_wf <- workflow() %>%
  add_recipe(model_recipe_label) %>%
  add_model(xgb_spec_final)

xgb_fit <- fit(xgb_final_wf, data = train_data)


# -----------------------------------------
# 10. Multinomial Logistic
# -----------------------------------------

# Define a Multinomial Logistic Spec with tuneable penalty & mixture
log_spec <- multinom_reg(
  penalty = tune(),
  mixture = tune()  # range [0,1] => [Ridge, LASSO]
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# Build a workflow
log_workflow_one_hot <- workflow() %>%
  add_recipe(model_recipe_one_hot) %>%
  add_model(log_spec)


# Create a 2D Tuning Grid for penalty & mixture

log_grid <- crossing(
  penalty = 10^seq(-6, 1, length.out = 8),  # e.g. 1e-6 -> 10
  mixture = seq(0, 1, by = 0.25)            # 0=Ridge, 1=LASSO
)


# Tune via Cross-Validation

set.seed(123)
log_tune_res <- tune_grid(
  log_workflow_one_hot,
  resamples = df_folds,
  grid = log_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_grid(save_pred = TRUE) # or parallel if desired
)

# Identify the best hyperparameters
log_best <- select_best(log_tune_res, metric = "accuracy")

# Finalize the workflow with the best found hyperparams
log_final_wf <- finalize_workflow(log_workflow_one_hot, log_best)

# Train on the Entire Training Set
log_fit <- fit(log_final_wf, data = train_data)

# -----------------------------------------
# 11. Evaluate Models on Final Hold-Out Set
# -----------------------------------------
# We'll generate predictions on `test_data` for each model,
# and compute:
#   - Accuracy
#   - F1 Score (macro-averaged)
#   - TPR (using yardstick::sensitivity, macro-averaged)
#   - FPR (1 - yardstick::specificity, macro-averaged)
#   - AUC Score (using the "hand_till" estimator for multiclass)


# Helper function to compute macro-averaged FPR (FPR = 1 - specificity)
fpr_macro <- function(data, truth, estimate) {
  spec_val <- yardstick::specificity(data, truth = {{ truth }}, estimate = {{ estimate }}, estimator = "macro")
  spec_val %>%
    mutate(
      .metric   = "fpr_macro",
      .estimate = 1 - .estimate   # FPR = 1 - specificity
    )
}

# Helper function to get probability column names (excluding .pred_class)
get_prob_cols <- function(results_df) {
  names(results_df)[grepl("^\\.pred_", names(results_df)) & names(results_df) != ".pred_class"]
}

# ===================== 1) RANDOM FOREST =====================
rf_results <- test_data %>%
  select(row_id) %>%
  bind_cols(
    predict(final_rf_fit, new_data = test_data, type = "class"),
    predict(final_rf_fit, new_data = test_data, type = "prob")
  ) %>%
  bind_cols(test_data %>% select(status))

print("Random Forest Probability Columns:")
print(get_prob_cols(rf_results))

conf_mat_rf <- conf_mat(rf_results, truth = status, estimate = .pred_class)
print(conf_mat_rf)

accuracy_rf <- accuracy(rf_results, truth = status, estimate = .pred_class)
f1_rf       <- f_meas(rf_results, truth = status, estimate = .pred_class, estimator = "macro")
tpr_rf      <- yardstick::sensitivity(rf_results, truth = status, estimate = .pred_class, estimator = "macro")
fpr_rf      <- fpr_macro(rf_results, status, .pred_class)

rf_prob_cols <- get_prob_cols(rf_results)
auc_rf      <- roc_auc(
  rf_results,
  truth = status,
  !!!syms(rf_prob_cols),
  estimator = "hand_till"
)

rf_metrics <- bind_rows(accuracy_rf, f1_rf, tpr_rf, fpr_rf, auc_rf)
print("Random Forest Metrics:")
print(rf_metrics)


# ===================== 2) XGBOOST =====================
xgb_results <- test_data %>%
  select(row_id) %>%
  bind_cols(
    predict(xgb_fit, new_data = test_data, type = "class"),
    predict(xgb_fit, new_data = test_data, type = "prob")
  ) %>%
  bind_cols(test_data %>% select(status))

print("XGBoost Probability Columns:")
print(get_prob_cols(xgb_results))

conf_mat_xgb <- conf_mat(xgb_results, truth = status, estimate = .pred_class)
print(conf_mat_xgb)

accuracy_xgb <- accuracy(xgb_results, truth = status, estimate = .pred_class)
f1_xgb       <- f_meas(xgb_results, truth = status, estimate = .pred_class, estimator = "macro")
tpr_xgb      <- yardstick::sensitivity(xgb_results, truth = status, estimate = .pred_class, estimator = "macro")
fpr_xgb      <- fpr_macro(xgb_results, status, .pred_class)

xgb_prob_cols <- get_prob_cols(xgb_results)
auc_xgb      <- roc_auc(
  xgb_results,
  truth = status,
  !!!syms(xgb_prob_cols),
  estimator = "hand_till"
)

xgb_metrics <- bind_rows(accuracy_xgb, f1_xgb, tpr_xgb, fpr_xgb, auc_xgb)
print("XGBoost Metrics:")
print(xgb_metrics)


# ===================== 3) MULTINOMIAL LOGISTIC =====================
log_results <- test_data %>%
  select(row_id) %>%
  bind_cols(
    predict(log_fit, new_data = test_data, type = "class"),
    predict(log_fit, new_data = test_data, type = "prob")
  ) %>%
  bind_cols(test_data %>% select(status))

print("Logistic Regression Probability Columns:")
print(get_prob_cols(log_results))

conf_mat_log <- conf_mat(log_results, truth = status, estimate = .pred_class)
print(conf_mat_log)

accuracy_log <- accuracy(log_results, truth = status, estimate = .pred_class)
f1_log       <- f_meas(log_results, truth = status, estimate = .pred_class, estimator = "macro")
tpr_log      <- yardstick::sensitivity(log_results, truth = status, estimate = .pred_class, estimator = "macro")
fpr_log      <- fpr_macro(log_results, status, .pred_class)

log_prob_cols <- get_prob_cols(log_results)
auc_log      <- roc_auc(
  log_results,
  truth = status,
  !!!syms(log_prob_cols),
  estimator = "hand_till"
)

log_metrics <- bind_rows(accuracy_log, f1_log, tpr_log, fpr_log, auc_log)
print("Logistic Regression Metrics:")
print(log_metrics)


# ===================== 4) Ensemple RF and XGBoost =====================

ensemble_preds_weighted <- rf_results %>%
  select(row_id,
         rf_Exited         = .pred_Exited,
         rf_Operating      = .pred_Operating,
         rf_Out_of_Business= `.pred_Out of Business`,
         status) %>%
  inner_join(
    xgb_results %>%
      select(row_id,
             xgb_Exited         = .pred_Exited,
             xgb_Operating      = .pred_Operating,
             xgb_Out_of_Business= `.pred_Out of Business`),
    by = "row_id"
  ) %>%
  transmute(
    row_id,
    status,
    .pred_Exited         = 0.9 * rf_Exited + 0.1 * xgb_Exited,
    .pred_Operating      = 0.9 * rf_Operating + 0.1 * xgb_Operating,
    .pred_Out_of_Business= 0.9 * rf_Out_of_Business + 0.1 * xgb_Out_of_Business
  ) %>%
  mutate(
    .pred_class = factor(
      pmap_chr(
        list(.pred_Exited, .pred_Operating, .pred_Out_of_Business),
        ~ names(which.max(c(Exited=..1, Operating=..2, `Out of Business`=..3)))
      ),
      levels = levels(status)
    )
  )
# Evaluate ensemble performance (weighted)
ensemble_metrics_weighted <- bind_rows(
  accuracy(ensemble_preds_weighted, truth = status, estimate = .pred_class),
  f_meas(ensemble_preds_weighted, truth = status, estimate = .pred_class, estimator = "macro"),
  yardstick::sensitivity(ensemble_preds_weighted, truth = status, estimate = .pred_class, estimator = "macro"),
  fpr_macro(ensemble_preds_weighted, truth = status, estimate = .pred_class),
  roc_auc(
    ensemble_preds_weighted,
    truth = status,
    .pred_Exited, .pred_Operating, .pred_Out_of_Business,
    estimator = "hand_till"
  )
)

print("\nWeighted Ensemble Model Metrics (90% RF, 10% XGB):")
print(ensemble_metrics_weighted)

# Add new ensemble to final comparison
ggplot2::theme_set(theme_minimal())

all_models_metrics <- bind_rows(
  rf_metrics %>% mutate(model = "RF (default)"),
  xgb_metrics %>% mutate(model = "XGBoost"),
  log_metrics %>% mutate(model = "Logistic"),
  ensemble_metrics_weighted %>% mutate(model = "RF + XGB Weighted Ensemble (90/10)")
) %>%
  select(model, everything()) %>%
  arrange(.metric, desc(.estimate))

print("\nComparison of All Models:")
print(all_models_metrics)


# -----------------------------------------
# 12. Feature Importance
# -----------------------------------------


library(tidymodels)
library(fastshap)
library(shapviz)

# Prep recipe and get processed test data
prepped_recipe <- prep(model_recipe_label)
X_test_processed <- bake(prepped_recipe, new_data = test_data, all_predictors())

# Extract ranger model explicitly
rf_model <- extract_fit_engine(final_rf_fit)

# Compute SHAP values explicitly for 'Exited' (working guaranteed)
set.seed(123)
rf_shap_exited <- fastshap::explain(
  object = rf_model,
  X = X_test_processed,
  pred_wrapper = function(model, newdata) {
    predict(model, data = newdata, type = "response")$predictions[, "Exited"]
  },
  nsim = 10,
  adjust = TRUE
)


# Create shapviz object
rf_shap_exited_viz <- shapviz(
  as.matrix(rf_shap_exited),
  X_test_processed
)

# Beeswarm plot
sv_importance(rf_shap_exited_viz, kind = "beeswarm", max_display = 20)



# ===================== SHAP for XGBoost =====================

# 1. Extract model
xgb_model <- extract_fit_engine(xgb_fit)

# 2. Prepare matrix
X_test_matrix <- as.matrix(X_test_processed)
colnames(X_test_matrix) <- colnames(X_test_processed)

# 3. Predict
preds <- predict(xgb_model, newdata = X_test_matrix)

# 4. Reshape to matrix
pred_matrix <- matrix(preds, nrow = nrow(X_test_matrix), byrow = TRUE)
colnames(pred_matrix) <- c("Exited", "Operating", "Out of Business")

# 5. fastshap with corrected pred_wrapper
set.seed(123)
xgb_shap_exited <- fastshap::explain(
  object = xgb_model,
  X = X_test_processed,   # still data.frame for shap
  pred_wrapper = function(model, newdata) {
    X_mat <- as.matrix(newdata)
    colnames(X_mat) <- colnames(newdata)
    preds <- predict(model, newdata = X_mat)
    pred_matrix <- matrix(preds, nrow = nrow(X_mat), byrow = TRUE)
    colnames(pred_matrix) <- c("Exited", "Operating", "Out of Business")
    pred_matrix[, "Exited"]
  },
  nsim = 10,
  adjust = TRUE
)

# 6. shapviz
xgb_shap_exited_viz <- shapviz(
  as.matrix(xgb_shap_exited),
  X_test_processed
)

# 7. Plot!
sv_importance(xgb_shap_exited_viz, kind = "beeswarm", max_display = 20)

# -----------------------------------------
# 13. Ensemble Feature Attribution via Weighted SHAP
# -----------------------------------------

# Combine SHAP values for 'Exited' using the same 90% RF / 10% XGB weights
ensemble_shap_exited <- 0.9 * rf_shap_exited + 0.1 * xgb_shap_exited

# Visualize using shapviz
ensemble_shap_exited_viz <- shapviz(
  as.matrix(ensemble_shap_exited),
  X_test_processed
)

# Beeswarm plot for top 20 features
sv_importance(ensemble_shap_exited_viz, kind = "beeswarm", max_display = 20)


# -----------------------------------------
# 14. Benchmarking: 3 Companies per Class
# -----------------------------------------

set.seed(6)

# STEP 1: Sample 3 row_ids per class directly from test_data
sampled_ids <- test_data %>%
  group_by(status) %>%
  slice_sample(n = 3) %>%
  ungroup() %>%
  pull(row_id)

# STEP 2: Pull company names from raw by that row_id
sampled_raw_info <- df_raw %>%
  filter(row_id %in% sampled_ids) %>%
  select(row_id, companies)

# STEP 3: Pull in our forced‐prediction ensemble by row_id
sampled_preds <- ensemble_preds_weighted %>%
  filter(row_id %in% sampled_ids)

# STEP 4: Assemble the final table
sampled_results <- sampled_raw_info %>%
  left_join(
    test_data %>% select(row_id, status),
    by = "row_id"
  ) %>%
  left_join(
    sampled_preds %>% select(row_id, .pred_class, .pred_Exited, .pred_Operating, .pred_Out_of_Business),
    by = "row_id"
  ) %>%
  arrange(status) %>%
  mutate(sample_id = LETTERS[1:9])

# STEP 5: Force exactly 3 per class
assign_forced_predictions <- function(df) {
  long_df <- df %>%
    select(sample_id, .pred_Exited, .pred_Operating, .pred_Out_of_Business) %>%
    pivot_longer(
      cols = starts_with(".pred_"),
      names_to  = "class",
      values_to = "prob"
    ) %>%
    mutate(
      class = sub("^\\.pred_", "", class)
    ) %>%
    arrange(desc(prob))
  
  assigned   <- tibble()
  used       <- character()
  counts     <- setNames(rep(0, 3), c("Exited", "Operating", "Out_of_Business"))
  
  for (i in seq_len(nrow(long_df))) {
    sid <- long_df$sample_id[i]
    cls <- long_df$class[i]
    if (sid %in% used) next
    if (counts[[cls]] >= 3) next
    
    assigned <- bind_rows(assigned, tibble(sample_id = sid, forced_class = cls))
    used     <- c(used, sid)
    counts[[cls]] <- counts[[cls]] + 1
    
    if (length(used) == 9) break
  }
  
  df %>%
    mutate(sample_id = as.character(sample_id)) %>%
    left_join(assigned, by = "sample_id") %>%
    mutate(
      forced_pred = recode(
        forced_class,
        Exited         = "Exited",
        Operating      = "Operating",
        Out_of_Business = "Out of Business"
      ) %>%
        factor(levels = levels(status))
    ) %>%
    select(-forced_class)
}


sampled_results <- assign_forced_predictions(sampled_results)

# STEP 6: Display & compute accuracy
cat("📊 Benchmarking Set: 9 Companies (3 per class by highest‑confidence forced prediction)\n\n")

sampled_results %>%
  select(sample_id, companies, status, forced_pred, .pred_class,
         .pred_Exited, .pred_Operating, .pred_Out_of_Business) %>%
  knitr::kable(
    digits=3,
    col.names=c("ID","Company","Actual","Forced Pred","Raw Pred",
                "P(Exited)","P(Operating)","P(Bankrupt)")
  )

benchmark_accuracy <- yardstick::accuracy(
  sampled_results,
  truth = status,
  estimate = forced_pred
)
print(benchmark_accuracy)



