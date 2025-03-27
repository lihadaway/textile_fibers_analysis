# load EDA script for data preprocessing
source("script/01_eda.R")

# merge U.S. cotton production (excluding "U.S." column) and cotton prices (Spot Price)
model_data <- merge(
    select(.data = us_cotton_data$production, -`U.S.`),
    select(.data = us_cotton_prices, `Spot Price`)
) |>
  drop_na()

# calculate correlation between variables, focusing on "Spot Price"
cor(model_data[2:ncol(model_data)])[, "Spot Price"]

# merge U.S. cotton prices, production, and world export data by year, then drop missing values
model_data <- merge(x = us_cotton_prices, y = us_cotton_data$production, by = "year") |>
  select(year, `U.S.`, `Spot Price`) |>
  merge(x = world_cotton_data$major_foreign_exporters, by = "year") |>
  drop_na()

# calculate correlation between variables, focusing on "Spot Price"
cor(model_data[2:ncol(model_data)])[, "Spot Price"]
