# -----------------------------------------
# 14. Benchmarking (fixed list of 8 companies)
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

# 1) Pull rows from df_raw for those companies
selected_raw <- df_raw %>%
  filter(companies %in% selected_companies) %>%
  distinct(companies, .keep_all = TRUE) %>%               # guard against duplicates
  mutate(companies = factor(companies, levels = selected_companies)) %>%
  arrange(companies) %>%
  select(row_id, companies, status)

# Safety: ensure all 8 found
missing <- setdiff(selected_companies, as.character(selected_raw$companies))
if (length(missing) > 0) {
  stop(paste0("Not found in df_raw$companies: ", paste(missing, collapse = ", ")))
}

# 2) Join status (from test_data) and predictions (from ensemble_preds_weighted)
selected_preds <- ensemble_preds_weighted %>%
  filter(row_id %in% selected_raw$row_id) %>%
  select(row_id, pred_class, pred_success, pred_no_success)

# Safety: ensure predictions exist for all 8
missing_pred <- setdiff(selected_raw$row_id, selected_preds$row_id)
if (length(missing_pred) > 0) {
  stop(paste0("Predictions missing for row_id(s): ",
              paste(missing_pred, collapse = ", "),
              ". Ensure these companies are in the prediction set."))
}

selected_results <- selected_raw %>%
  left_join(test_data %>% select(row_id, status), by = "row_id") %>%
  left_join(selected_preds, by = "row_id") %>%
  mutate(
    status     = factor(status, levels = c("Success", "No Success")),
    pred_class = factor(pred_class, levels = c("Success", "No Success"))
  )

# 3) Force exactly 4 predictions of each class by highest probability (as before)
k <- 4
top_success_ids    <- selected_results %>% arrange(desc(pred_success))    %>% slice(1:k) %>% pull(row_id)
top_no_success_ids <- selected_results %>% arrange(desc(pred_no_success)) %>% slice(1:k) %>% pull(row_id)

selected_results <- selected_results %>%
  mutate(
    forced_pred_class = case_when(
      row_id %in% top_success_ids    ~ "Success",
      row_id %in% top_no_success_ids ~ "No Success",
      TRUE                           ~ NA_character_
    ),
    forced_pred_class = factor(forced_pred_class, levels = c("Success", "No Success"))
  ) %>%
  mutate(sample_id = LETTERS[1:n()])

# 4) Show table and accuracy
cat("📊 Benchmarking Set: fixed 8 companies (order specified)\n\n")

selected_results %>%
  select(sample_id, companies, ownership_status, status,
         pred_class, forced_pred_class, pred_success, pred_no_success) %>%
  knitr::kable(
    digits = 3,
    col.names = c("ID", "Company", "Ownership", "Actual", "Predicted", "Forced Pred.", "P(Success)", "P(No Success)")
  ) %>%
  print()

benchmark_accuracy <- yardstick::accuracy(
  selected_results,
  truth    = status,
  estimate = forced_pred_class
)
print(benchmark_accuracy)
