## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(knitr)
library(readxl)


## -----------------------------------------------------------------------------
#| label: fig-old-vis-on-poster
#| echo: false
#| fig.cap: "Heat map of inflationary impact on key items over 2023."

include_graphics("images/old_poster.png")


## -----------------------------------------------------------------------------
#| label: load-data

# Load full data, select specific sheet (T11), skip first 5 rows and remove rows with NA values
data <- read_excel("cpiapr24.xlsx", sheet = "T11", skip = 5) |>
  filter(if_all(everything(), ~ !is.na(.)))

data


## -----------------------------------------------------------------------------
#| label: filter-data-categories

# Filter the data to include only these categories
categories <- c(
        "Food",
        #  "Food Excl Food Serving Services", #  Temporary commented out
        "Clothing & Footwear",
        "Housing & Utilities",
        "Household Durables & Services",
        "Health Care",
        "Transport",
        "Communication",
        "Recreation & Culture",
        "Education"
        #  "Miscellaneous Goods & Services" #  Temporary commented out
)


## -----------------------------------------------------------------------------
#| label: filter-data-date

# Filter the total inflation every year
total_inf_data <- data |> 
  filter(Variables %in% "All Items") |>
  select(Variables, starts_with(c("2020", "2021", "2022", "2023"))) |>
  # Convert selected columns to numeric
  mutate_at(vars(starts_with(c("2020", "2021", "2022", "2023"))), as.numeric)

# Filter the data to include only these categories and the months from 2019 onwards
data <- data |>
  filter(Variables %in% categories) |>
  select(Variables, starts_with(c("2020", "2021", "2022", "2023"))) |>
  # Convert selected columns to numeric
  mutate_at(vars(starts_with(c("2020", "2021", "2022", "2023"))), as.numeric)

# Print the filtered data
options(max.print = 1e6)
print(data, n = nrow(data), width = Inf)


## -----------------------------------------------------------------------------
#| label: further-transform-data
# Create a month mapping
month_mapping <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
                   "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)

# Pivot the data frame
pivoted_data <- pivot_longer(
  data, 
  cols = starts_with(c("2019", "2020", "2021","2022", "2023")),
  names_to = "Month_Year",
  values_to = "Value"
) |>
  # Separate the Month column into Year and Month
  separate(Month_Year, into = c("Year", "Month"), sep = " ") |>
  # Convert Month column into numeric using mapping
  mutate(Year = as.numeric(Year), Month = month_mapping[Month]) |>
  # To sort the data by Year, then by Month
  arrange(Year, Month)

all_items_data <- total_inf_data |>
  pivot_longer(
    cols = starts_with(c("2019", "2020", "2021", "2022", "2023")),
    names_to = "Month_Year",
    values_to = "Value"
  ) |>
  separate(Month_Year, into = c("Year", "Month"), sep = " ") |>
  mutate(
    Year = as.numeric(Year),
    Month = month_mapping[Month],
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) |>
  arrange(Date)

all_items_data


## -----------------------------------------------------------------------------
#| label: find-average-except-2023

month_dates <- unique(pivoted_data$Month)
month_dates

# month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Find the average of the other years except 2023
pivoted_data_except_2023 <- pivoted_data |>
  filter(Year != 2023)

pivoted_data_except_2023

average_prices_except_2023 <- pivoted_data_except_2023 |>
  group_by(Variables, Month) |>
  summarize(average_Value = mean(Value)) |>
  arrange(Variables, Month) 

# average_prices_except_2023$Month <- factor(average_prices_except_2023$Month, levels = month_order)

average_prices_except_2023 |> 
  arrange(Variables, Month)


# Excluding 2023 data from all_items_data
all_items_data_except_2023 <- all_items_data |>
  filter(Year != 2023)

average_all_items_except_2023 <- all_items_data_except_2023 |>
  group_by(Variables, Month) |>
  summarize(average_Value = mean(Value)) |>
  arrange(Month)

average_all_items_except_2023 |> 
  arrange(Month)



## -----------------------------------------------------------------------------
#| label: process-2023-data

pivoted_data_2023 <- pivoted_data |>
  filter(Year == 2023)

pivoted_data_2023 |> 
  arrange(Variables, Month)

pivoted_data_2023$Year <- NULL

pivoted_data_2023

# Include only 2023 data from all_items_data
all_items_data_2023 <- all_items_data |>
  filter(Year == 2023)

all_items_data_2023 |> 
  arrange(Variables, Month)

all_items_data_2023$Year <- NULL

all_items_data_2023


## -----------------------------------------------------------------------------
#| label: combine-data

combined_data <- inner_join(average_prices_except_2023, pivoted_data_2023, by = c("Variables", "Month")) |>
  rename(Value_Average = average_Value, Value_2023 = Value) 
  
# combined_data$Month <- factor(combined_data$Month, levels = month_order)

combined_data |>
  arrange(Variables, Month)

combined_all_items_data <- inner_join(average_all_items_except_2023, all_items_data_2023, by = c("Variables", "Month")) |>
  rename(Value_Average = average_Value, Value_2023 = Value)

combined_all_items_data |>
  arrange(Variables, Month)

