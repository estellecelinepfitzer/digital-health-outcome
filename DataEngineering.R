# ==============================================================
# Digital Health Companies Success Prediction: Data Engineering
# ==============================================================

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
                              "Publicly Held") ~ "Success",
      ownership_status %in% c("In IPO Registration", 
                              "Privately Held (backing)", 
                              "Privately Held (no backing)",
                              "Out of Business") ~ "No Success",
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
  "majestic_growth_rate", "majestic_referring_domains", "majestic_growth_rate_percentile",
  "top_cpc_codes","first_financing_debt", "intended_user", "clinical_domain"
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
# --- 1. Define VC and Company Hubs ---
vc_hubs <- c(
  "san_francisco", "new_york", "boston", "london",
  "san_diego", "los_angeles", "washington", "chicago",
  "denver", "tel_aviv"
)

company_hubs <- c(
  "san_francisco", "new_york", "london", "boston",
  "los_angeles", "philadelphia", "tel_aviv", "seoul",
  "washington", "toronto"
)

# --- 2. Clean World Cities ---
data(world.cities)

city_door <- world.cities %>%
  mutate(
    hq_city = tolower(name),
    hq_city = gsub("[^a-z0-9]+", "_", hq_city),
    hq_city = gsub("_+", "_", hq_city),
    hq_city = gsub("^_|_$", "", hq_city)
  ) %>%
  group_by(hq_city) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hq_city, lat, lon = long)

# --- 3. Merge Coordinates to Companies ---
df_with_coords <- df %>%
  left_join(city_door, by = "hq_city") %>%
  mutate(row_id = row_number())

# --- 4. Function to Compute Hub Distance and Membership ---
compute_hub_features <- function(df_with_coords, city_door, hubs, label) {
  hub_coords <- city_door %>%
    filter(hq_city %in% hubs) %>%
    rename(hub_lat = lat, hub_lon = lon)
  
  distances <- df_with_coords %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    crossing(hub_coords %>% select(-hq_city)) %>%  # <-- Fix here
    mutate(
      dist_km = distHaversine(cbind(lon, lat), cbind(hub_lon, hub_lat)) / 1000
    ) %>%
    group_by(row_id) %>%
    summarise(!!paste0("distance_to_", label, "_hub_km") := min(dist_km, na.rm = TRUE), .groups = "drop")
  
  avg_distance <- mean(distances[[2]], na.rm = TRUE)
  
  df_with_coords %>%
    left_join(distances, by = "row_id") %>%
    mutate(
      !!paste0("distance_to_", label, "_hub_km") := if_else(
        is.na(.data[[paste0("distance_to_", label, "_hub_km")]]),
        avg_distance,
        .data[[paste0("distance_to_", label, "_hub_km")]]
      ),
      !!paste0("is_in_", label, "_hub") := as.integer(.data[[paste0("distance_to_", label, "_hub_km")]] <= 64)
    )
}


# --- 5. Compute VC and Company Hub Features ---
df_with_coords <- compute_hub_features(df_with_coords, city_door, vc_hubs, "vc")
df_with_coords <- compute_hub_features(df_with_coords, city_door, company_hubs, "company")

# --- 6. Final Cleanup ---
df <- df_with_coords %>%
  select(-row_id, -lat, -lon, -hq_city)

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
# 6. Remove Highly Correlated Features
# -------------------------------

# numeric_df <- df %>%
#   select(where(is.numeric))
# 
# corr_mat <- cor(numeric_df, use = "complete.obs")
# high_cor <- findCorrelation(corr_mat, cutoff = 0.9)
# names(numeric_df)[high_cor]
# # Remove those columns from df
# df <- df %>% select(-any_of(names(numeric_df)[high_cor]))


# -------------------------------
# 7. Save dataset
# -------------------------------

# Create a row ID in the raw dataset for later matching
df_raw  <- df_raw  %>% mutate(row_id = row_number())
df       <- df       %>% mutate(row_id = row_number())

write.csv(df, "/Users/estellepfitzer/Desktop/PhD/1. Papers/3. Data Model/df_clean.csv", row.names = FALSE, na = "")

