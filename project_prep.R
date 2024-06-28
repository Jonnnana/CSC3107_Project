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

# Pivot the data for the total inflation
# pivoted_total_data <- pivot_longer(
#   total_inf_data, 
#   cols = starts_with(c("2019", "2020", "2021","2022", "2023")),
#   names_to = "Month_Year",
#   values_to = "Value"
# ) |>
#   # Separate the Month column into Year and Month
#   separate(Month_Year, into = c("Year", "Month"), sep = " ") |>
#   # Convert Month column into numeric using mapping
#   mutate(Year = as.numeric(Year), Month = month_mapping[Month]) |>
#   # To sort the data by Year, then by Month
#   arrange(Year, Month)

# pivoted_data
pivoted_total_data


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



## -----------------------------------------------------------------------------
#| label: process-2023-data

pivoted_data_2023 <- pivoted_data |>
  filter(Year == 2023)

pivoted_data_2023 |> 
  arrange(Variables, Month)

pivoted_data_2023$Year <- NULL

pivoted_data_2023


## -----------------------------------------------------------------------------
#| label: combine-data

combined_data <- inner_join(average_prices_except_2023, pivoted_data_2023, by = c("Variables", "Month")) |>
  rename(Value_Average = average_Value, Value_2023 = Value) 
  
# combined_data$Month <- factor(combined_data$Month, levels = month_order)

combined_data |>
  arrange(Variables, Month)



## -----------------------------------------------------------------------------
#| label: line-chart-plot

line_plot <- ggplot(combined_data, aes(x = Month)) +
  geom_line(aes(y = Value_Average, color = "Average")) +
  geom_line(aes(y = Value_2023, color = "2023")) +
  facet_wrap(~ Variables, scales = "free_y") +
  labs(
    title = "Trends vs 2023", 
    x = "Month", 
    y = "Rate",
    color = "Inflation Rate"  # Change legend title here
    ) +
  scale_color_manual(values = c("Average" = "blue", "2023" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1, 6, 12), labels = c("Jan", "Jun", "Dec"))

line_plot


## -----------------------------------------------------------------------------

#| line-graph-plot

#  Maybe it's something to build upon?

# Convert Month and Year to a combined date variable for X-axis
pivoted_data$Date <- as.Date(paste(pivoted_data$Year, pivoted_data$Month, "01", sep = "-"))

pivoted_data

# Use Qualitative Color Palette for different categories. Use colors that are easily distinguishable with the white bg
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22")

plt <- ggplot(pivoted_data, aes(x = Date, y = Value, color = Variables)) +
  geom_line(aes(color = Variables), alpha = 0.1, linetype="dotted") + #  add the exact line graph (harsh)
  scale_color_manual(values = custom_colors) +  
  geom_smooth(method = "auto", se = FALSE, aes(group = Variables), linewidth = 0.5) +  # Add a smooth curve
  # geom_point(aes(color=Variables), alpha=0.2) + #  add the exact data point
  labs(
    title = "Inflation trend",
    x = "Date",
    y = "Value"
  ) + 
  theme_minimal()

plt



## -----------------------------------------------------------------------------

df_long <- 
  pivot_longer(data, cols = -Variables, names_to = "Month", values_to = "Value")
df_long


# Create a data frame for the year labels and vertical lines
years <- unique(df_long$Year)
year_positions <- seq(12.5, by = 12, length.out = length(years))

year_labels <- data.frame(
  Year = years,
  x_position = seq(6.5, by = 12, length.out = length(years))  # Adjust x_position for year labels
)

ggplot(df_long, aes(x = Month, y = Variables, fill = Value)) +
  geom_tile(color = "white") +
    scale_fill_gradient2(name = "Inflation Rate\nPercentage Change",
                       low = "red",  # Red (low end)
                       mid = "white",    # White (midpoint)
                       high = "blue", # blue (high end)
                       midpoint = 0,     # Set midpoint to 0
                       breaks = c(20, 10, 0, -7),  # Specify breaks
                       labels = c("20", "10", "0", "-7"),  # Specify labels
                       ) +
  labs(x = "Years", y = "Variables", title = "Heatmap of Variables by Month") +
  theme_minimal() +
  theme(legend.position = "top",  # Move legend to the top
        plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.text.x = element_blank()  # Remove x-axis text
  ) +
  coord_fixed(ratio = 5) +  # Ensure square tiles
  geom_vline(xintercept = c(12.5, 24.5,36.5, 48.5, 60.5), linetype = "dashed", color = "black")   # Add vertical lines
  


## -----------------------------------------------------------------------------
pivoted_total_data$Date <- as.Date(paste(pivoted_total_data$Year, pivoted_total_data$Month, "01", sep = "-"))

test_plot <- ggplot(pivoted_data, aes(x = Date, y = Value, fill = Variables)) +
  geom_line() +
  geom_bar(data = pivoted_total_data, aes(x = Date, y = Value), stat = "identity", fill = "grey") +
  facet_wrap(~ Variables, scales = "free_y") + # Separate facets for each category
  labs(title = "Trends vs Overall Average", x = "Year", y = "Value") +
  theme_minimal()

test_plot + scale_y_continuous(sec.axis = sec_axis(~./max(.)*max(pivoted_total_data$Value), name = "Average Inflation Rate")) +
  coord_flip()

test_plot


## -----------------------------------------------------------------------------
# Create the bar chart
ggplot(pivoted_total_data, aes(x = Date, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Bar Chart of Pivoted Total Data", x = "Category", y = "Inflation Rate") +
  theme_minimal()



## -----------------------------------------------------------------------------
#| label: scatter-plot-visualization
#| fig.width: 12
#| fig.height: 8

# Convert Month and Year to a combined date variable for X-axis
pivoted_data$Date <- as.Date(paste(pivoted_data$Year, pivoted_data$Month, "01", sep = "-"))

# Create scatter plot
scatter_plot <- ggplot(pivoted_data, aes(x = Date, y = Value, color = Variables)) +
  geom_point(alpha = 0.6, size = 2) +  # Adjusted point size for better visibility
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1) + # Adjusted line width for clarity
  scale_color_manual(values = custom_colors) +  
  labs(
    title = "Scatter Plot of Year-on-Year CPI Changes by Category",
    x = "Date",
    y = "CPI Change (%)"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )

scatter_plot

