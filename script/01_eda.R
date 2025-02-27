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

# read all sheets into a named list
us_cotton_data <- list(
  planted_acreage = read_us_cotton_data("Table 4"), # 1,000 acres
  harvested_acreage = read_us_cotton_data("Table 5"), # 1,000 acres
  lint_yield = read_us_cotton_data("Table 6"), # pounds/harvested acre
  production = read_us_cotton_data("Table 7") # 1,000 480-pound bales
)

world_cotton_data <- list(
  major_foreign_exporters = read_world_cotton_data(
    sheet_name = "Table 17",
    columns = c("year", "Uzbekistan 1/", "Africa 2/", "Australia", "Pakistan", "India", "Turkey", "Sudan", "Brazil", "Mexico",
      "Egypt")),  # 1,000 480-pound bales
  major_foreign_importers = read_world_cotton_data(
    sheet_name = "Table 18",
    columns = c("year", "Union 1/", "Bangladesh", "Vietnam", "Indonesia", "South Korea", "Thailand", "Turkey", "India",
      "Pakistan", "China"))  # 1,000 480-pound bales
)

# output structure
str(us_cotton_data)
str(world_cotton_data)

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

# output structure
str(us_cotton_data)
str(world_cotton_data)

# visualize total cotton produced in U.S. over time
ggplot(us_cotton_data$production, aes(x = year, y = `U.S.` / 100, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "U.S. Cotton Production Over Time",
       x = "Year",
       y = "Production (million 480-lb bales)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
