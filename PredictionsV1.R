# ============================================================
# Digital Health Companies Success Prediction - Improved RF Model
# Author: Estelle Pfitzer
# Date: 2025-02-07
# ============================================================

# -------------------------------
# 1. Load Required Libraries
# -------------------------------
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(zoo)
library(ggplot2)
library(caret)
library(randomForest)
library(tidyr)
library(smotefamily)
library(caret)
library(xgboost)


# -------------------------------
# 2. Load and Prepare Dataset
# -------------------------------
file_path <- "/Users/estellepfitzer/Desktop/PB_Downloads/EP_PitchBook_Cleaned_2025_03_12.csv"
df <- read_csv(file_path)

# Convert Excel-style date columns
excel_date_cols <- c("first_financing_date", "last_financing_date", "last_updated_date", "last_known_valuation_date")
df[excel_date_cols] <- lapply(df[excel_date_cols], function(x) as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30"))

# Normalize ownership status to new status labels
df <- df %>%
  mutate(status = case_when(
    ownership_status %in% c("Acquired/Merged", "Acquired/Merged (Operating Subsidiary)", "Publicly Held") ~ "Exited",
    ownership_status %in% c("In IPO Registration", "Privately Held (backing)", "Privately Held (no backing)") ~ "Operating",
    ownership_status == "Out of Business" ~ "Out of Business",
    TRUE ~ ownership_status
  )) %>%
  select(-ownership_status)

# Drop irrelevant columns
drop_cols <- c("company_id", "latest_note", "latest_note_author", 
               "company_former_name", "company_also_known_as", "company_legal_name",
               "registration_number", "company_registry", "competitors", "pb_id", 
               "description","primary_industry_sector", "primary_industry_group", 
               "primary_industry_code", "all_industries", "verticals", "keywords",
               "company_financing_status", "business_status",
               "universe", "website", "linked_in_url", "exchange", "ticker",
               "parent_company", "daily_updates", "weekly_updates",
               "gross_profit", "net_income", "enterprise_value",
               "ebit", "market_cap", "net_debt",
               "primary_contact_pb_id", "primary_contact",
               "primary_contact_title", "primary_contact_email", "primary_contact_phone",
               "hq_location", "hq_address_line_1", "hq_address_line_2", "hq_city",
               "hq_state_province", "hq_post_code",
               "hq_phone", "hq_fax", "hq_email", "hq_global_region",
               "hq_global_sub_region", "financing_status_note", "active_investors",
               "acquirers","other_investors", "active_investors_websites", 
               "former_investors_websites","other_investors_websites", "general_services",
               "services_on_a_deal","first_financing_size_status","first_financing_valuation_status",
               "first_financing_deal_type", "first_financing_deal_type_2",
               "first_financing_deal_type_3", "first_financing_status",
               "last_financing_size", "last_financing_size_status", "last_financing_valuation",
               "last_financing_valuation_status", "last_financing_deal_type",
               "last_financing_deal_type_2", "last_financing_deal_type_3",
               "last_financing_deal_class", "last_financing_debt_date",
               "last_financing_debt_size", "last_financing_debt", "last_financing_status",
               "growth_rate", "growth_rate_percentile", "growth_rate_change",
               "growth_rate_percent_change", "web_growth_rate", "web_growth_rate_percentile",
               "size_multiple","size_multiple_percentile", "size_multiple_change",
               "size_multiple_percent_change", "web_size_multiple", "web_size_multiple_percentile",
               "profile_data_source", "opportunity_score", "success_class", "success_probability",
               "no_exit_probability", "predicted_exit_type", "ipo_probability", "m_a_probability",
               "last_known_valuation", "last_known_valuation_deal_type",
               "emerging_spaces", "clinical_trials_matching_criteria",
               "last_valuation_step_up", "view_company_online", "last_updated_date")

df <- df %>% select(-any_of(drop_cols))

# -------------------------------
# 3. Feature Engineering
# -------------------------------

# Employee growth metrics
compute_cagr <- function(history) {
  if (is.na(history)) return(NA)
  records <- str_split(history, ",\\s*")[[1]]
  values <- map(records, ~ str_match(.x, "(\\d{4}):\\s*(\\d+)")[, 2:3])
  df_local <- as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE)
  colnames(df_local) <- c("year", "count")
  df_local <- df_local %>% mutate(year = as.numeric(year), count = as.numeric(count)) %>% arrange(year)
  if (nrow(df_local) < 2 || df_local$count[1] == 0) return(NA)
  round((df_local$count[nrow(df_local)] / df_local$count[1])^(1 / (df_local$year[nrow(df_local)] - df_local$year[1])) - 1, 4)
}

compute_yoy <- function(history) {
  if (is.na(history)) return(NA)
  records <- str_split(history, ",\\s*")[[1]]
  values <- map(records, ~ str_match(.x, "(\\d{4}):\\s*(\\d+)")[, 2:3])
  df_local <- as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE)
  colnames(df_local) <- c("year", "count")
  df_local <- df_local %>% mutate(year = as.numeric(year), count = as.numeric(count)) %>% arrange(year)
  if (nrow(df_local) < 2 || df_local$count[nrow(df_local) - 1] == 0) return(NA)
  round((df_local$count[nrow(df_local)] - df_local$count[nrow(df_local) - 1]) / df_local$count[nrow(df_local) - 1], 4)
}

df$employee_cagr <- sapply(df$employee_history, compute_cagr)
df$employee_yoy_growth <- sapply(df$employee_history, compute_yoy)
df <- df %>% select(-employee_history)

# Investor count
df$investor_number <- ifelse(
  df$status == "Operating",
  df$number_active_investors,
  sapply(df$former_investors, function(x) {
    if (is.na(x)) 0 else length(str_split(x, ",\\s*")[[1]])
  })
)
df$investor_number[is.na(df$investor_number)] <- 0
df <- df %>% select(-number_active_investors, -former_investors)

# Replace NA with 0 in specific numeric columns
df <- df %>% mutate(across(
  c(total_clinical_trials, total_patent_documents, total_patent_families, total_raised, active_patent_documents, number_of_ucc_filings),
  ~replace(., is.na(.), 0)))

# Fiscal year extraction from fiscal_period
df$fiscal_year <- as.numeric(ifelse(
  str_detect(df$fiscal_period, "^FY\\s*\\d{4}$"),
  str_remove(df$fiscal_period, "FY\\s*"),
  ifelse(str_detect(df$fiscal_period, "^TTM\\s*\\w+\\d{4}$"), str_extract(df$fiscal_period, "\\d{4}$"), NA)
))
df <- df %>% select(-fiscal_period)

# Remove outdated revenue/EBITDA
# Identify outdated rows (those where fiscal year is after last known valuation year)
outdated_rows <- !is.na(df$last_known_valuation_date) & !is.na(df$fiscal_year) &
  as.numeric(format(df$last_known_valuation_date, "%Y")) < df$fiscal_year

# Replace outdated revenue and EBITDA with NA
df <- df %>% mutate(
  revenue = ifelse(outdated_rows, NA, revenue),
  ebitda = ifelse(outdated_rows, NA, ebitda)
) %>%
  select(-last_known_valuation_date, -fiscal_year)  # Drop columns that were used for the check


# Financing timing features
df <- df %>%
  filter(!is.na(year_founded)) %>%
  mutate(
    year_founded = as.numeric(year_founded),
    no_first_financing = as.integer(is.na(first_financing_date)),
    no_second_financing = as.integer(is.na(last_financing_date)),
    time_to_first_financing = ifelse(!is.na(first_financing_date),
                                     as.numeric(format(first_financing_date, "%Y")) - year_founded, 999),
    time_between_financings = ifelse(!is.na(first_financing_date) & !is.na(last_financing_date),
                                     as.numeric(difftime(last_financing_date, first_financing_date, units = "days")) / 365.25, 999)
  ) %>%
  select(-first_financing_date, -last_financing_date, -companies)

# -------------------------------
# 4. Handle Missing Data
# -------------------------------

df <- df %>%
  mutate(
    has_revenue_data = ifelse(is.na(revenue), 0, 1),
    has_ebitda_data = ifelse(is.na(ebitda), 0, 1),
    has_employee_cagr = ifelse(is.na(employee_cagr), 0, 1),
    has_employee_yoy_growth = ifelse(is.na(employee_yoy_growth), 0, 1),
    has_revenue_growth = ifelse(is.na(revenue_growth_percent), 0, 1),
    has_employee_data = ifelse(is.na(employees), 0, 1)
  )

# Calculate overall median (or mean) for the columns
median_revenue <- median(df$revenue, na.rm = TRUE)
median_ebitda <- median(df$ebitda, na.rm = TRUE)
median_employees <- median(df$employees, na.rm = TRUE)

# Impute missing values with overall median values
df <- df %>%
  mutate(
    revenue = ifelse(is.na(revenue), median_revenue, revenue),
    ebitda = ifelse(is.na(ebitda), median_ebitda, ebitda),
    employees = ifelse(is.na(employees), median_employees, employees)
  )


df <- df %>%
  mutate(
    revenue_growth_percent = replace(revenue_growth_percent, is.na(revenue_growth_percent), 0),
    employee_cagr = replace(employee_cagr, is.na(employee_cagr), 0),
    employee_yoy_growth = replace(employee_yoy_growth, is.na(employee_yoy_growth), 0),
    first_financing_size = replace(first_financing_size, is.na(first_financing_size), 0),
    hq_country_territory_region = replace(as.character(hq_country_territory_region), is.na(hq_country_territory_region), "Unknown"),
    first_financing_deal_class = ifelse(is.na(first_financing_deal_class) | first_financing_deal_class == "", "No financing", first_financing_deal_class),
    dht_category = ifelse(is.na(dht_category) | dht_category == 0, "Other", dht_category)
  ) %>%
  mutate(hq_country_territory_region = as.factor(hq_country_territory_region))

# Fill NAs in web/majestic metrics
df <- df %>%
  mutate(across(
    c(similar_web_size_multiple, similar_web_size_multiple_percentile,
      majestic_size_multiple, majestic_size_multiple_percentile,
      similar_web_unique_visitors, similar_web_unique_visitors_change,
      similar_web_unique_visitors_percent_change, majestic_referring_domains,
      majestic_referring_domains_change, majestic_referring_domains_percent_change,
      similar_web_growth_rate, similar_web_growth_rate_percentile,
      majestic_growth_rate, majestic_growth_rate_percentile),
    ~replace(., is.na(.), 0)
  ))


# Drop columns with >80% missing
missing_pct <- colSums(is.na(df)) / nrow(df) * 100
df <- df %>% select(-any_of(names(missing_pct[missing_pct > 80])))

# Convert target to factor
df$status <- as.factor(df$status)


# -------------------------------
# 4. Create Two DataFrames for Encoding Strategies
# -------------------------------

# Separate the target (status) from the features
df_features <- df %>% select(-status)  # Remove 'status' column (target variable)
df_target <- df$status  # Keep 'status' column as the target variable

# Create dataset with One-Hot Encoding for 'hq_country_territory_region' and 'DHT Category'
df_one_hot <- df_features %>%
  mutate(
    hq_country_territory_region = as.factor(hq_country_territory_region),
    DHT_Category = as.factor(dht_category),  # Ensure DHT Category is factored for one-hot encoding
    first_financing_deal_class = as.factor(first_financing_deal_class)
  ) 

# One-Hot encode categorical columns using caret's dummyVars
dummies <- dummyVars(~ ., data = df_one_hot)
df_one_hot_encoded <- predict(dummies, newdata = df_one_hot) %>% as.data.frame()

# Add back the 'status' column (target variable) to the One-Hot Encoded dataset
df_one_hot_encoded$status <- df_target

# Create dataset with Label Encoding for 'hq_country_territory_region', 'first_financing_deal_class' and 'DHT Category'
df_label_encoded <- df_features %>%
  mutate(
    hq_country_territory_region = as.numeric(factor(hq_country_territory_region)),
    first_financing_deal_class = as.numeric(factor(first_financing_deal_class)),
    dht_category = as.numeric(factor(dht_category))  # Label encode DHT Category
  )

# Add back the 'status' column (target variable) to the Label Encoded dataset
df_label_encoded$status <- df_target


# -------------------------------
# 5. Split, Balance, and Train Models for Both Datasets
# -------------------------------

# Train-test split (80-20)
set.seed(123)
train_index <- sample(seq_len(nrow(df_one_hot_encoded)), size = 0.8 * nrow(df_one_hot_encoded))
train_set_one_hot <- df_one_hot_encoded[train_index, ]
test_set_one_hot <- df_one_hot_encoded[-train_index, ]

train_set_label <- df_label_encoded[train_index, ]
test_set_label <- df_label_encoded[-train_index, ]

# Separate features and labels
train_features_one_hot <- train_set_one_hot %>% select(-status)
train_labels_one_hot <- train_set_one_hot$status

train_features_label <- train_set_label %>% select(-status)
train_labels_label <- train_set_label$status

# Apply SMOTE to balance classes for One-Hot Encoded data
smote_result_one_hot <- SMOTE(train_features_one_hot, train_labels_one_hot, K = 5)
smote_data_one_hot <- smote_result_one_hot$data
colnames(smote_data_one_hot)[ncol(smote_data_one_hot)] <- "status"
smote_data_one_hot$status <- as.factor(smote_data_one_hot$status)

# Apply SMOTE to balance classes for Label Encoded data
smote_result_label <- SMOTE(train_features_label, train_labels_label, K = 5)
smote_data_label <- smote_result_label$data
colnames(smote_data_label)[ncol(smote_data_label)] <- "status"
smote_data_label$status <- as.factor(smote_data_label$status)

# Train Random Forest for One-Hot Encoded data
rf_model_one_hot <- randomForest(x = smote_data_one_hot %>% select(-status),
                                 y = smote_data_one_hot$status,
                                 importance = TRUE, ntree = 500)

# Train Random Forest for Label Encoded data
rf_model_label <- randomForest(x = smote_data_label %>% select(-status),
                               y = smote_data_label$status,
                               importance = TRUE, ntree = 500)

# -------------------------------
# 6. Evaluate Models
# -------------------------------

# Predictions and Evaluation for One-Hot Encoded Model
test_features_one_hot <- test_set_one_hot %>% select(-status)
predictions_one_hot <- predict(rf_model_one_hot, newdata = test_features_one_hot)
conf_matrix_one_hot <- confusionMatrix(predictions_one_hot, test_set_one_hot$status)
print(conf_matrix_one_hot)

# Predictions and Evaluation for Label Encoded Model
test_features_label <- test_set_label %>% select(-status)
predictions_label <- predict(rf_model_label, newdata = test_features_label)
conf_matrix_label <- confusionMatrix(predictions_label, test_set_label$status)
print(conf_matrix_label)

# -------------------------------
# 7. Plot Feature Importance
# -------------------------------

# Plot feature importance for One-Hot Encoded model
varImpPlot(rf_model_one_hot)

# Plot feature importance for Label Encoded model
varImpPlot(rf_model_label)


# -------------------------------
# 9. Train Logistic Regression for One-Hot Encoded Data
# -------------------------------
library(nnet)

# One-Hot Encoded data
logit_model_one_hot <- multinom(status ~ ., data = smote_data_one_hot)
logit_predictions_one_hot <- predict(logit_model_one_hot, newdata = test_set_one_hot %>% select(-status))
logit_conf_matrix_one_hot <- confusionMatrix(logit_predictions_one_hot, test_set_one_hot$status)
print(logit_conf_matrix_one_hot)



# -------------------------------
# 10. Train XGBoost for One-Hot Encoded Data
# -------------------------------
# Prepare data for XGBoost
train_matrix_one_hot <- as.matrix(smote_data_one_hot %>% select(-status))
train_labels_one_hot <- smote_data_one_hot$status
test_matrix_one_hot <- as.matrix(test_set_one_hot %>% select(-status))
test_labels_one_hot <- test_set_one_hot$status

# Convert labels to numeric (from 0 to num_class-1)
train_labels_one_hot_num <- as.numeric(factor(train_labels_one_hot, levels = c("Exited", "Operating", "Out of Business"))) - 1
test_labels_one_hot_num <- as.numeric(factor(test_labels_one_hot, levels = c("Exited", "Operating", "Out of Business"))) - 1


# Train the XGBoost model
xgb_model_one_hot <- xgboost(data = train_matrix_one_hot, 
                             label = train_labels_one_hot_num, 
                             objective = "multi:softmax", 
                             num_class = 3, 
                             nrounds = 500, 
                             max_depth = 6, 
                             eta = 0.1)

# Predictions with XGBoost
xgb_predictions_one_hot <- predict(xgb_model_one_hot, newdata = test_matrix_one_hot)

# Confusion Matrix for XGBoost (One-Hot Encoded)
xgb_conf_matrix_one_hot <- confusionMatrix(as.factor(xgb_predictions_one_hot), as.factor(test_labels_one_hot_num))
print(xgb_conf_matrix_one_hot)



# -------------------------------
# 12. Train XGBoost for Label Encoded Data
# -------------------------------
# Prepare data for XGBoost
train_matrix_label <- as.matrix(smote_data_label %>% select(-status))
train_labels_label <- smote_data_label$status
test_matrix_label <- as.matrix(test_set_label %>% select(-status))
test_labels_label <- test_set_label$status

# Check the levels of the factor before conversion to numeric
levels(train_labels_label)  # This should print "Exited", "Operating", "Out of Business"

# Convert labels to numeric (0-based indexing)
train_labels_label_num <- as.numeric(factor(train_labels_label, levels = c("Exited", "Operating", "Out of Business"))) - 1
test_labels_label_num <- as.numeric(factor(test_labels_label, levels = c("Exited", "Operating", "Out of Business"))) - 1

# Verify that labels are in the correct range [0, 1, 2]
summary(train_labels_label_num)  # Should show 0, 1, and 2 as valid values


# Train the XGBoost model
xgb_model_label <- xgboost(data = train_matrix_label, 
                           label = train_labels_label_num, 
                           objective = "multi:softmax", 
                           num_class = 3, 
                           nrounds = 500, 
                           max_depth = 6, 
                           eta = 0.1)

# Predictions with XGBoost
xgb_predictions_label <- predict(xgb_model_label, newdata = test_matrix_label)

# Confusion Matrix for XGBoost (Label Encoded)
xgb_conf_matrix_label <- confusionMatrix(as.factor(xgb_predictions_label), as.factor(test_labels_label_num))
print(xgb_conf_matrix_label)

# -------------------------------
# 13. Evaluate Feature Importance for Random Forest, XGBoost, and Linear Regression
# -------------------------------
# Feature importance plot for Random Forest (One-Hot Encoded)
varImpPlot(rf_model_one_hot)

# Feature importance plot for XGBoost (One-Hot Encoded)
xgb.importance(model = xgb_model_one_hot) %>%
  xgb.plot.importance()

# Feature importance plot for Random Forest (Label Encoded)
varImpPlot(rf_model_label)

# Feature importance plot for XGBoost (Label Encoded)
xgb.importance(model = xgb_model_label) %>%
  xgb.plot.importance()


