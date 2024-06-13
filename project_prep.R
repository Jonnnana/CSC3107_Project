## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(ggplot2)
library(knitr)
library(dplyr)
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


## -----------------------------------------------------------------------------
#| label: filter-data-categories

# Filter the data to include only these categories
categories <- c(
        "Food",
        "Food Excl Food Serving Services",
        "Clothing & Footwear",
        "Housing & Utilities",
        "Household Durables & Services",
        "Health Care",
        "Transport",
        "Communication",
        "Recreation & Culture",
        "Education",
        "Miscellaneous Goods & Services"
)


## -----------------------------------------------------------------------------
#| label: filter-data-date

# Filter the data to include only these categories and the months from 2019 onwards
data <- data |>
  filter(Variables %in% categories) |>
  select(Variables, starts_with("2019"), starts_with("2020"), starts_with("2021"), starts_with("2022"), starts_with("2023"))

# Print the filtered data
options(max.print = 1e6)
print(data, n = nrow(data), width = Inf)

