library(tidyverse)
library(readxl)

# define functions to read Excel sheets
read_us_cotton_data <- function(sheet_name) {
  stopifnot(is.character(sheet_name))
  read_excel(
    path = "./data/U.S.CottonSupplyandDemand.xlsx",
    sheet = sheet_name,
    range = "A7:U56",
    col_names = c("year", "AL", "AZ", "AR", "CA", "FL", "GA", "KS", "KY", "LA", "MS", "MO", "NV", "NM", "NC", "OK",
                  "SC", "TN", "TX", "VA", "U.S.")
  )
}

read_world_cotton_data <- function(sheet_name, columns) {
  stopifnot(is.character(sheet_name))
  stopifnot(is.vector(columns))
  read_excel(
    path = "./data/WorldCottonSupplyandDemand.xlsx",
    sheet = sheet_name,
    range = "A7:K56",
    col_names = columns
  )
}

read_cotton_price_data <- function(sheet_name, columns) {
  stopifnot(is.character(sheet_name))
  stopifnot(is.vector(columns))
  read_excel(
    path = "./data/CottonPrices.xlsx",
    sheet = sheet_name,
    range = "A6:D54",
    col_names = columns
  )
}

# read sheets
us_cotton_data <- list(
  planted_acreage = read_us_cotton_data("Table 4"), # 1,000 acres
  harvested_acreage = read_us_cotton_data("Table 5"), # 1,000 acres
  lint_yield = read_us_cotton_data("Table 6"), # pounds/harvested acre
  production = read_us_cotton_data("Table 7") # 1,000 480-pound bales
)

world_cotton_data <- list(
  major_foreign_exporters = read_world_cotton_data(
    sheet_name = "Table 17",
    columns = c("year", "Uzbekistan 1/", "Africa 2/", "Australia", "Pakistan", "India", "Turkey", "Sudan", "Brazil",
                "Mexico", "Egypt")), # 1,000 480-pound bales
  major_foreign_importers = read_world_cotton_data(
    sheet_name = "Table 18",
    columns = c("year", "Union 1/", "Bangladesh", "Vietnam", "Indonesia", "South Korea", "Thailand", "Turkey", "India",
                "Pakistan", "China")) # 1,000 480-pound bales
)

us_cotton_prices <- read_cotton_price_data(
  sheet_name = "Table11",
  columns = c("year", "Farm Price", "Spot Price", "Mill Price")
) # cents per pound

# clean U.S. cotton prices table
us_cotton_prices <- us_cotton_prices |>
  mutate(across(everything(), ~str_replace_all(.x, " 2/", ""))) |>
  mutate(across(everything(), ~ifelse(.x == "NA", NA, .x))) |>
  mutate(across(-year, as.numeric))

# output structure
str(us_cotton_data)
str(world_cotton_data)
str(us_cotton_prices)

# cast numeric columns
us_cotton_data <- lapply(us_cotton_data, function(df) {
  df |>
    mutate(across(.cols = 2:ncol(df), .fns = ~as.numeric(.)))
})

world_cotton_data <- lapply(world_cotton_data, function(df) {
  df |>
    mutate(across(.cols = 2:ncol(df), .fns = ~as.numeric(.)))
})

# check for missing values
map(us_cotton_data, ~sum(is.na(.)))
map(world_cotton_data, ~sum(is.na(.)))
map(us_cotton_prices, ~sum(is.na(.)))

# output structure
str(us_cotton_data)
str(world_cotton_data)
str(us_cotton_prices)

# pivot wide-format data to long format
exporters_long <- world_cotton_data$major_foreign_exporters |>
  pivot_longer(
    cols = -year,
    names_to = "Country",
    values_to = "Exports"
  ) |>
  drop_na(Exports)

us_cotton_prices_long <- us_cotton_prices |>
  pivot_longer(
    cols = -year,
    names_to = "Type",
    values_to = "Price"
  ) |>
  drop_na(Price)

# visualize total cotton produced in U.S. over time
ggplot(data = us_cotton_data$production, mapping = aes(x = year, y = `U.S.` / 100, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "U.S. Cotton Production Over Time",
       x = "Year",
       y = "Production (million 480-lb bales)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# visualize exported cotton by major exporters over time
ggplot(data = exporters_long, mapping = aes(x = year, y = Exports / 100, color = Country, group = Country)) +
  geom_line(linewidth = 1) +
  labs(title = "Major Foreign Cotton Exporters Over Time",
       x = "Year",
       y = "Exports (million 480-lb bales)",
       color = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# visualize U.S. cotton prices over time
ggplot(data = us_cotton_prices_long, mapping = aes(x = year, y = Price / 100, color = Type, group = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "U.S. Cotton Prices Over Time",
       x = "Year",
       y = "Price (USD per pound)",
       color = "Price Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# visualize U.S. cotton prices distribution
ggplot(data = us_cotton_prices_long, mapping = aes(x = Type, y = Price / 100, color = Type)) +
  geom_boxplot() +
  labs(title = "U.S. Cotton Prices Distribution",
       y = "Price (USD per pound)") +
  theme_minimal() +
  theme(legend.position = "none")
