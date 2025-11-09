# Load libraries
library(dplyr)
library(httr)
library(jsonlite)

# STEP 1: Read and Clean HUD PIT Data
pit_data <- read.csv("C:/Users/evere/Downloads/2007-2024-PIT-Counts-by-State.csv", stringsAsFactors = FALSE)

hud_clean <- pit_data %>%
  select(
    state = State,
    total_homeless = Overall.Homeless,
    sheltered_total = Sheltered.Total.Homeless,
    unsheltered = Unsheltered.Homeless
  ) %>%
  filter(!is.na(state),
         nchar(state) == 2,
         !state %in% c("", "AS", "GU", "MP", "PR", "VI")) %>%
  mutate(
    total_homeless = as.numeric(total_homeless),
    sheltered_total = as.numeric(sheltered_total),
    unsheltered = as.numeric(unsheltered),
    state = trimws(state)
  )

# STEP 2: Get Complete Census Data with ALL Predictors
census_vars <- c(
  "NAME", 
  "S1701_C03_001E",  # Poverty rate
  "S1901_C01_012E",  # Median income
  "S2301_C04_001E",  # Unemployment rate
  "S2503_C01_002E",  # Median rent
  "S2503_C02_014E",  # Rent burden 35%+
  "S2504_C02_001E",  # Owner occupied units
  "S2504_C03_001E",  # Renter occupied units
  "S1501_C01_008E",  # College educated
  "S1810_C02_001E",  # Disability rate
  "S2101_C01_002E",  # Veteran population
  "S0501_C01_006E",  # Foreign-born population
  "S0101_C01_001E"   # Total population
)

base_url <- "https://api.census.gov/data/2024/acs/acs1/subject"
api_url <- paste0(base_url, "?get=", paste(census_vars, collapse = ","), "&for=state:*")

response <- GET(api_url)
census_data <- fromJSON(content(response, "text"))

census_clean <- as.data.frame(census_data[-1, ], stringsAsFactors = FALSE)
colnames(census_clean) <- census_data[1, ]

census_clean <- census_clean %>%
  mutate(across(-c(NAME, state), as.numeric)) %>%
  rename(
    state_name = NAME,
    poverty_rate = S1701_C03_001E,
    median_income = S1901_C01_012E,
    unemployment_rate = S2301_C04_001E,
    median_rent = S2503_C01_002E,
    rent_burden_severe = S2503_C02_014E,
    owner_occupied = S2504_C02_001E,
    renter_occupied = S2504_C03_001E,
    college_educated = S1501_C01_008E,
    disability_rate = S1810_C02_001E,
    veteran_population = S2101_C01_002E,
    foreign_born = S0501_C01_006E,
    total_population = S0101_C01_001E
  )

# STEP 3: Create State Mapping and Merge
state_mapping <- data.frame(
  hud_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
               "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
               "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
               "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  census_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                  "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                  "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

# Merge datasets
final_data <- hud_clean %>%
  left_join(state_mapping, by = c("state" = "hud_abbr")) %>%
  left_join(census_clean, by = c("census_full" = "state_name")) %>%
  mutate(
    homelessness_rate = (total_homeless / total_population) * 10000,
    # Create some derived variables
    severe_rent_burden_rate = rent_burden_severe / renter_occupied * 100,
    rental_vacancy_rate = (total_population - owner_occupied - renter_occupied) / total_population * 100
  ) %>%
  filter(!is.na(homelessness_rate), 
         !is.infinite(homelessness_rate),
         total_population > 0)

cat("Final merged dataset - Rows:", nrow(final_data), "\n")

# STEP 4: Prepare for Stepwise Selection with ALL Variables
model_data <- final_data %>%
  select(
    homelessness_rate,
    # Economic variables
    poverty_rate,
    median_income,
    unemployment_rate,
    # Housing variables
    median_rent,
    rent_burden_severe,
    severe_rent_burden_rate,
    rental_vacancy_rate,
    # Demographic variables
    college_educated,
    disability_rate,
    veteran_population,
    foreign_born,
    total_population
  )

# Remove any remaining NA values
model_data_clean <- model_data[complete.cases(model_data), ]

cat("\nModeling dataset - Rows:", nrow(model_data_clean), "\n")
cat("Variables available for stepwise selection:\n")
print(colnames(model_data_clean))

# STEP 5: Stepwise Selection
cat("\n=== PERFORMING STEPWISE SELECTION ===\n")

full_model <- lm(homelessness_rate ~ ., data = model_data_clean)
null_model <- lm(homelessness_rate ~ 1, data = model_data_clean)

stepwise_model <- step(null_model,
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both",
                       trace = 1)

selected_vars <- names(coef(stepwise_model))[-1]  # exclude intercept

cat("\n=== SELECTED VARIABLES ===\n")
cat("Number of selected predictors:", length(selected_vars), "\n")
cat("Selected variables:\n")
print(selected_vars)

cat("\n=== FINAL MODEL SUMMARY ===\n")
final_summary <- summary(stepwise_model)
print(final_summary)

# STEP 6: Variable Importance Analysis
cat("\n=== VARIABLE IMPORTANCE ===\n")
coefficients_df <- as.data.frame(summary(stepwise_model)$coefficients)
coefficients_df$Variable <- rownames(coefficients_df)
coefficients_df <- coefficients_df %>%
  filter(Variable != "(Intercept)") %>%
  arrange(desc(abs(Estimate)))

print(coefficients_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")])

# STEP 7: Save Results
write.csv(final_data, "homelessness_complete_data.csv", row.names = FALSE)
cat("\nData saved to: homelessness_complete_data.csv\n")

# STEP 8: Quick Visualization
library(ggplot2)

if(length(selected_vars) > 0) {
  top_predictor <- selected_vars[1]
  cat("\n=== VISUALIZATION: TOP PREDICTOR ===\n")
  cat("Top predictor:", top_predictor, "\n")
  
  p <- ggplot(final_data, aes_string(x = top_predictor, y = "homelessness_rate")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = paste("Relationship between", top_predictor, "and Homelessness Rate"),
      x = top_predictor,
      y = "Homelessness Rate (per 10,000 people)"
    ) +
    theme_minimal()
  
  print(p)
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Model R-squared:", round(final_summary$r.squared, 3), "\n")
cat("Adjusted R-squared:", round(final_summary$adj.r.squared, 3), "\n")
cat("Top predictors identified for homelessness risk:\n")
for(i in seq_along(selected_vars)) {
  cat(i, ". ", selected_vars[i], "\n", sep = "")
}
