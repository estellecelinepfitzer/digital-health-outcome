library(dplyr)
library(geosphere)
library(tidyr)
library(maps)

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


# # --- Optional. Visualize effect
# library(ggplot2)
# 
# # Step 1: Binary outcome
# df$exited <- ifelse(df$status == "Exited", 1, 0)
# 
# # Step 2: Fit models
# model_company <- glm(exited ~ is_in_company_hub, data = df, family = binomial)
# model_vc <- glm(exited ~ is_in_vc_hub, data = df, family = binomial)
# 
# # Step 3: Predictions for Company Hub
# newdata_company <- data.frame(is_in_company_hub = c(0, 1))
# link_company <- predict(model_company, newdata = newdata_company, type = "link", se.fit = TRUE)
# 
# pred_company <- newdata_company %>%
#   mutate(
#     fit = link_company$fit,
#     se = link_company$se.fit,
#     pred = plogis(fit),
#     lower = plogis(fit - 1.96 * se),
#     upper = plogis(fit + 1.96 * se),
#     group = factor(is_in_company_hub, labels = c("Not in Company Hub", "In Company Hub"))
#   )
# 
# # Step 4: Plot for Company Hub
# plot_company <- ggplot(pred_company, aes(x = group, y = pred)) +
#   geom_col(fill = "steelblue") +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(
#     title = "Predicted Probability of Exit\nby Company Hub Membership",
#     x = "", y = "Predicted Probability of Exit"
#   ) +
#   annotate("text", x = 2, y = pred_company$upper[2] + 0.015,
#            label = "p < 0.001", size = 4.5, fontface = "italic") +
#   theme_minimal()
# 
# # Step 5: Predictions for VC Hub
# newdata_vc <- data.frame(is_in_vc_hub = c(0, 1))
# link_vc <- predict(model_vc, newdata = newdata_vc, type = "link", se.fit = TRUE)
# 
# pred_vc <- newdata_vc %>%
#   mutate(
#     fit = link_vc$fit,
#     se = link_vc$se.fit,
#     pred = plogis(fit),
#     lower = plogis(fit - 1.96 * se),
#     upper = plogis(fit + 1.96 * se),
#     group = factor(is_in_vc_hub, labels = c("Not in VC Hub", "In VC Hub"))
#   )
# 
# # Step 6: Plot for VC Hub
# plot_vc <- ggplot(pred_vc, aes(x = group, y = pred)) +
#   geom_col(fill = "darkgreen") +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(
#     title = "Predicted Probability of Exit\nby VC Hub Membership",
#     x = "", y = "Predicted Probability of Exit"
#   ) +
#   annotate("text", x = 2, y = pred_vc$upper[2] + 0.015,
#            label = "p < 0.001", size = 4.5, fontface = "italic") +
#   theme_minimal()
# 
# # Step 7: Print both plots
# print(plot_company)
# print(plot_vc)
# 
# df <- df%>%
#   select(-exited)
# 
# 
# 
