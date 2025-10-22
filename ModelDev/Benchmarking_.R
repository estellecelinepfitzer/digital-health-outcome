# -----------------------------------------
#Benchmarking: 8 Companies (4 per class)
# -----------------------------------------

# Packages
library(dplyr)
library(tidyr)
library(knitr)
library(yardstick)
library(stringr)

# Reproducibility
set.seed(8)

# ---- User-configurable knobs ----
k_per_class <- 4  # number of companies to sample per class (total = 2*k_per_class)
#fte_min     <- 20 # if you want to select min fte
#fte_max     <- 50 # if you want to select max fte
#regions_ok  <- c("europe", "americas") # if you want to select regions


# ---- Safety checks on required objects/columns ----
.req_cols_test <- c("row_id", "status", "employees", "hq_global_region", "hq_country_territory_region")
.req_cols_raw  <- c("row_id", "companies")
.req_cols_pred <- c("row_id", "pred_class", "pred_success", "pred_no_success")

stopifnot(
  exists("test_data"), exists("df_raw"), exists("ensemble_preds_weighted"),
  all(.req_cols_test %in% names(test_data)),
  all(.req_cols_raw  %in% names(df_raw)),
  all(.req_cols_pred %in% names(ensemble_preds_weighted))
)

# ---- STEP 1: Filter eligible pool from test_data ----
eligible_pool <- test_data %>%
  filter(
    employees >= fte_min, employees <= fte_max,
    hq_global_region %in% regions_ok,
    if_else(
      hq_global_region == "americas",
      hq_country_territory_region %in% countries_ok_if_americas,
      TRUE
    )
  ) %>%
  # standardize status factor (target is 2-class)
  mutate(status = factor(status, levels = c("Success", "No Success")))

# Ensure we have enough per class
class_counts <- eligible_pool %>%
  count(status, name = "n")

need_ok <- all(class_counts$n[class_counts$status == "Success"]  >= k_per_class,
               class_counts$n[class_counts$status == "No Success"] >= k_per_class)

if (is.na(need_ok) || !need_ok) {
  msg <- paste0(
    "Not enough eligible rows after filtering to sample ", k_per_class,
    " per class.\nCounts:\n",
    paste0(capture.output(print(class_counts)), collapse = "\n")
  )
  stop(msg)
}

# ---- STEP 2: Sample k_per_class row_ids per class ----
sampled_ids <- eligible_pool %>%
  group_by(status) %>%
  slice_sample(n = k_per_class) %>%
  ungroup() %>%
  pull(row_id)

# ---- STEP 3: Pull company names (and optional ownership_status if present) ----
cols_from_raw <- intersect(c("row_id", "companies", "ownership_status"), names(df_raw))

sampled_raw_info <- df_raw %>%
  filter(row_id %in% sampled_ids) %>%
  select(all_of(cols_from_raw))

# Safety: ensure 1:1 mapping rows
if (n_distinct(sampled_raw_info$row_id) != length(sampled_ids)) {
  dupes <- sampled_raw_info %>%
    count(row_id) %>%
    filter(n > 1) %>% pull(row_id)
  stop("Duplicate row_id(s) in df_raw for sampled set: ", paste(dupes, collapse = ", "))
}

# ---- STEP 4: Get predictions for sampled ids ----
sampled_preds <- ensemble_preds_weighted %>%
  filter(row_id %in% sampled_ids) %>%
  select(row_id, pred_class, pred_success, pred_no_success)

# Safety: ensure predictions for all sampled ids
missing_pred <- setdiff(sampled_ids, sampled_preds$row_id)
if (length(missing_pred) > 0) {
  stop(
    "Predictions missing for row_id(s): ",
    paste(missing_pred, collapse = ", "),
    ". Ensure these companies are included in the prediction set."
  )
}

# ---- STEP 5: Join all info and tidy factors ----
sampled_results <- sampled_raw_info %>%
  left_join(test_data %>% select(row_id, status), by = "row_id") %>%
  left_join(sampled_preds, by = "row_id") %>%
  mutate(
    status     = factor(status, levels = c("Success", "No Success")),
    pred_class = factor(pred_class, levels = c("Success", "No Success"))
  ) %>%
  # Create stable display order: Success block then No Success block, alphabetically within
  left_join(df_raw %>% select(row_id, companies), by = "row_id", keep = FALSE, relationship = "many-to-one") %>%
  arrange(status, companies) %>%
  # Label rows A..H (or more generally first 2*k_per_class letters)
  mutate(sample_id = LETTERS[seq_len(n())]) %>%
  # Keep unique companies (should already be unique due to sampled row_ids)
  distinct(row_id, .keep_all = TRUE)

# ---- STEP 6: Force exactly k_per_class predictions per class (by highest prob) ----
top_success_ids    <- sampled_results %>%
  arrange(desc(pred_success)) %>%
  slice_head(n = k_per_class) %>%
  pull(row_id)

top_no_success_ids <- sampled_results %>%
  arrange(desc(pred_no_success)) %>%
  slice_head(n = k_per_class) %>%
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

# Safety: ensure we assigned all
if (any(is.na(sampled_results$forced_pred_class))) {
  leftover <- sampled_results %>% filter(is.na(forced_pred_class)) %>% pull(row_id)
  stop("Forced prediction could not be assigned for row_id(s): ", paste(leftover, collapse = ", "))
}

# ---- STEP 7: Display benchmarking table ----
cat(glue::glue("📊 Benchmarking Set: {2*k_per_class} Companies (",
               "{k_per_class} per class, {fte_min}–{fte_max} FTEs, Europe or US/Canada only)\n\n"))

display_cols <- c("sample_id", "companies",
                  intersect("ownership_status", names(sampled_results)),
                  "status", "pred_class", "forced_pred_class",
                  "pred_success", "pred_no_success")

sampled_results %>%
  select(all_of(display_cols)) %>%
  knitr::kable(
    digits = 3,
    col.names = c(
      "ID", "Company",
      if ("ownership_status" %in% names(sampled_results)) "Ownership" else NULL,
      "Actual", "Predicted", "Forced Pred.",
      "P(Success)", "P(No Success)"
    )
  ) %>%
  print()


