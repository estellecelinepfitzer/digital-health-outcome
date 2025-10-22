# -----------------------------------------
# 14. Benchmarking: 3 Companies per Class (FTE 20–50 & EU or US/Canada only)
# -----------------------------------------
library(dplyr)
library(tidyr)
library(knitr)
library(yardstick)

set.seed(8)

# STEP 1: Sample 3 row_ids per class from test_data with:
# - 20 to 50 employees
# - in Europe or US/Canada
sampled_ids <- test_data %>%
  filter(
    employees >= 20, employees <= 50,
    hq_global_region == "europe" |
      (hq_global_region == "americas" & hq_country_territory_region %in% c("united_states", "canada"))
  ) %>%
  group_by(status) %>%
  filter(n() >= 3) %>%
  slice_sample(n = 3) %>%
  ungroup() %>%
  pull(row_id)

# STEP 2: Pull company names from raw
sampled_raw_info <- df_raw %>%
  filter(row_id %in% sampled_ids) %>%
  select(row_id, companies)

# STEP 3: Get predictions
sampled_preds <- ensemble_preds_weighted %>%
  filter(row_id %in% sampled_ids)

# STEP 4: Join all info
sampled_results <- sampled_raw_info %>%
  left_join(test_data %>% select(row_id, status), by = "row_id") %>%
  left_join(
    sampled_preds %>% select(row_id, .pred_class, .pred_Exited, .pred_Operating, .pred_Out_of_Business),
    by = "row_id"
  ) %>%
  arrange(status) %>%
  mutate(sample_id = LETTERS[1:9])

# STEP 5: Assign forced prediction by highest prob
assign_forced_predictions <- function(df) {
  long_df <- df %>%
    select(sample_id, .pred_Exited, .pred_Operating, .pred_Out_of_Business) %>%
    pivot_longer(cols = starts_with(".pred_"), names_to = "class", values_to = "prob") %>%
    mutate(class = sub("^\\.pred_", "", class)) %>%
    arrange(desc(prob))
  
  assigned <- tibble()
  used     <- character()
  counts   <- setNames(rep(0, 3), c("Exited", "Operating", "Out_of_Business"))
  
  for (i in seq_len(nrow(long_df))) {
    sid <- long_df$sample_id[i]
    cls <- long_df$class[i]
    if (sid %in% used || counts[[cls]] >= 3) next
    assigned <- bind_rows(assigned, tibble(sample_id = sid, forced_class = cls))
    used     <- c(used, sid)
    counts[[cls]] <- counts[[cls]] + 1
    if (length(used) == 9) break
  }
  
  df %>%
    mutate(sample_id = as.character(sample_id)) %>%
    left_join(assigned, by = "sample_id") %>%
    mutate(
      forced_pred = recode(forced_class,
                           Exited = "Exited",
                           Operating = "Operating",
                           Out_of_Business = "Out of Business") %>%
        factor(levels = levels(status))
    ) %>%
    select(-forced_class)
}

sampled_results <- assign_forced_predictions(sampled_results)

# STEP 6: Show and evaluate
cat("📊 Benchmarking Set: 9 Companies (3 per class, 20–50 FTEs, Europe or US/Canada only)\n\n")

sampled_results %>%
  select(sample_id, companies, status, forced_pred, .pred_class,
         .pred_Exited, .pred_Operating, .pred_Out_of_Business) %>%
  knitr::kable(
    digits = 3,
    col.names = c("ID", "Company", "Actual", "Forced Pred", "Raw Pred",
                  "P(Exited)", "P(Operating)", "P(Bankrupt)")
  )

benchmark_accuracy <- yardstick::accuracy(
  sampled_results,
  truth = status,
  estimate = forced_pred
)
print(benchmark_accuracy)

