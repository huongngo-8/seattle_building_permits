library(tidyverse)
library(ggplot2)

building_permits_data <- read_csv("https://raw.githubusercontent.com/svteichman/STAT302-WIN2022/main/files/projects/01_data-visualization/Building_Permits_Clean.csv")

# Creating a new column to contain IssuedYear values that are converted from numeric to factor type
building_permits_data <- building_permits_data %>%
  mutate(IssuedYear_Factor = as.factor(IssuedYear))
# Creating a new column to contain CouncilDistrict values that are converted from numeric to factor type
building_permits_data <- building_permits_data %>%
  mutate(CouncilDistrict_Factor = as.factor(CouncilDistrict))

# Creating data frame to house number of permits (observations) for residential and non-residential permit classes
# respectively for each month and year.
permit_class_mapped_count <- building_permits_data %>%
  filter(PermitClassMapped %in% c("Residential", "Non-Residential")) %>%
  group_by(IssuedYear_Factor, IssuedMth) %>%
  count(PermitClassMapped, name = "Count")

# Figure 1
# Number of Residential and Non-Residential Permits in Each Year (Grouped by permit class)
ggplot(data = building_permits_data %>% 
         filter(PermitClassMapped != "N/A"), 
       aes(x = IssuedYear_Factor, 
           fill = PermitClassMapped)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Residential and Non-Residential Permits in Each Year",
       subtitle = "Grouped by permit class mapped",
       caption = "Figure 1: Total number of residential (left, blue) and non-residential (right, red) permits in years 2017, 2018, 2019, 2020, 2021",
       y = "Number of Permits",
       x = "Year",
       fill = "Permit Class Mapped") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 2
# Number of Residential Permits in Each Month (Grouped by year)
ggplot(data = permit_class_mapped_count %>% 
         filter(PermitClassMapped == "Residential", IssuedYear_Factor %in% c(2019, 2020, 2021)), 
       aes(x = IssuedMth, 
           y = Count, 
           group = IssuedYear_Factor, 
           color = IssuedYear_Factor)) +
  geom_line() +
  labs(title = "Number of Residential Permits in Each Month", 
       subtitle = "Grouped by year",
       caption = "Figure 2: Number of residential permits in each for years 2019 (red), 2020 (green), 2021 (blue)",
       x = "Month",
       y = "Number of Residential Permits",
       color = "Issued Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks = 10)

# Figure 3
# Number of Non-Residential Permits in Each Month (Grouped by year)
ggplot(data = permit_class_mapped_count %>% 
         filter(PermitClassMapped == "Non-Residential", IssuedYear_Factor %in% c(2019, 2020, 2021)), 
       aes(x = IssuedMth, 
           y = Count, 
           group = IssuedYear_Factor, 
           color = IssuedYear_Factor)) +
  geom_line() +
  labs(title = "Number of Non-Residential Permits in Each Month", 
       subtitle = "Grouped by year",
       caption = "Figure 3: Number of non-residential permits in each month for years 2019 (red), 2020 (green), 2021 (blue)",
       x = "Month",
       y = "Number of Non-Residential Permits",
       color = "Issued Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks = 10)

# Figure 4
# Number of Residential and Non-Residential Permits in Each Council District (Faceted by year, grouped by permit class mapped)
ggplot(data = building_permits_data %>% 
         filter(IssuedYear_Factor %in% c(2019, 2020, 2021), !is.na(CouncilDistrict_Factor)), 
       aes(x = CouncilDistrict_Factor,
           fill = PermitClassMapped)) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(IssuedYear_Factor)) +
  labs(title = "Number of Residential and Non-Residential Permits in Each Council District",
       subtitle = "Faceted by year, grouped by permit class mapped",
       caption = "Figure 4: Number of residential (right, blue) and non-residential (left, red) permits in all 7 council districts and faceted by years 2019, 2020, 2021", 
       y = "Number of Permits",
       x = "Council District", 
       fill = "Permit Class Mapped") +
  theme_bw(base_size = 23) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.1),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 5
# Number of Residential and Non-Residential Permits in Each Year (Faceted by council district, grouped by permit class mapped)
ggplot(data = building_permits_data %>%
         filter(IssuedYear_Factor %in% c(2019, 2020, 2021), 
                PermitClassMapped != "N/A", 
                !is.na(CouncilDistrict_Factor)), 
       aes(x = IssuedYear, 
           fill = PermitClassMapped)) +
  geom_bar(position = "dodge") +
  facet_wrap(vars(CouncilDistrict_Factor), scales = "free_x") +
  labs(title = "Number of Residential and Non-Residential Permits in Each Year",
       subtitle = "Faceted by council district, grouped by permit class mapped",
       caption = "Figure 5: Number of residential (right, blue) and non-residential (left, pink) permits in years 2019, 2020, 2021, faceted by all 7 council districts",
       y = "Number of Permits",
       x = "Year", 
       fill = "Permit Class Mapped") +
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.2),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a data frame to store the number of permits that have statuses: Completed, Withdrawn and Issued by year
status_current_year <- building_permits_data %>%
  # Filtering by status of the permit 
  filter(StatusCurrent %in% c("Completed", "Withdrawn", "Issued")) %>%
  # Grouping by year
  group_by(IssuedYear) %>%
  # Counting number observations with status that meets the filtering condition
  count(StatusCurrent, name = "Count")

# Figure 6
# Number of Permits in Each Year (Faceted by current status of permit)
ggplot(data = status_current_year, 
       aes(x = IssuedYear, 
           y = Count)) +
  geom_line() +
  facet_wrap(vars(StatusCurrent)) +
  labs(title = "Number of Permits in Each Year",
       subtitle = "Faceted by current status of permit",
       caption = "Figure 6: Number of permits in years 2017, 2018, 2019, 2020, 2021, faceted by 3 permit statuses (Completed, Issued, Withdrawn).",
       y = "Number of Permits",
       x = "Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.spacing.x = unit(0.75, "cm"))

# Creating a data frame to store the number of permits that have the permit type descriptions: Addition/Alteration, Demolition, New. Grouped by year and permit class mapped.
issued_buildings_operations <- building_permits_data %>%
  # Filtering by status of permit, year and permit type description
  filter(StatusCurrent == "Issued", 
         IssuedYear_Factor %in% c(2020, 2021),
         PermitTypeDesc %in% c("Addition/Alteration", "Demolition", "New")) %>%
  # Grouping by year and permit class mapped
  group_by(IssuedYear_Factor, PermitClassMapped) %>%
  # Counting number of observations that satisfy the permit type description filter
  count(PermitTypeDesc, name = "Count")

# Figure 7
# Number of Residential and Non-Residential Permits with 'Issued' Status (Faceted by year)
ggplot(data = issued_buildings_operations,
       aes(x = PermitTypeDesc,
           y = Count,
           fill = PermitClassMapped)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(IssuedYear_Factor)) +
  labs(title = "Number of Residential and Non-Residential Permits with 'Issued' Status",
       subtitle = "Faceted by year",
       caption = "Figure 7: Number of residential (left, blue) and non-residential (right, pink) permits with issued status associated 
       with an operation (Addition/Alteration, Demolition, New), faceted by years 2020, 2021",
       y = "Number of Permits",
       x = "Operation (Permit Type Description)",
       fill = "Permit Class Mapped") +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a data frame to store the respective total number of housing units that is planned to be added or removed/already added or removed for all years 2017, 2018, 2019, 2020, 2021.
housing_units_activity_year <- building_permits_data %>%
  # Filtering out all NA values of HousingUnitsAdded and HousingUnitsRemoved
  filter(!is.na(HousingUnitsAdded),
         !is.na(HousingUnitsRemoved)) %>%
  # Grouping by year
  group_by(IssuedYear) %>%
  # Calculating the total number of housing units added and housing units removed
  summarize(hua_sum = sum(HousingUnitsAdded),
            hur_sum = sum(HousingUnitsRemoved))

# Figure 8
# Number of Housing Units Added or Removed in Each Year (Grouped by housing unit activity)
colors <- c("Housing Units Added" = "red", "Housing Units Removed" = "blue")
ggplot(data = housing_units_activity_year, aes(x = IssuedYear)) +
  geom_line(aes(y = hua_sum, color = "Housing Units Added")) +
  geom_line(aes(y = hur_sum, color = "Housing Units Removed")) +
  labs(title = "Number of Housing Units Added or Removed in Each Year",
       subtitle = "Grouped by housing unit activity",
       caption = "Figure 8: Number of housing units added (red) and removed (blue) in years 2017, 2018, 2019, 2020, 2021",
       y = "Number of Housing Units",
       x = "Year",
       color = "Housing Unit Activity") +
  scale_color_manual(values = colors) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a data frame to store the respective total number of housing units that is added and removed in each month for all years 2019, 2020, 2021.
housing_units_activity_month <- building_permits_data %>%
  # Filtering by year, and filtering out all NA values of HousingUnitsAdded and HousingUnitsRemoved
  filter(IssuedYear_Factor %in% c(2019, 2020, 2021),
         !is.na(HousingUnitsAdded),
         !is.na(HousingUnitsRemoved)) %>%
  # Grouping by year and month
  group_by(IssuedYear_Factor, IssuedMth) %>%
  # Calculating the total number of housing units added and housing units removed
  summarize(hua_sum = sum(HousingUnitsAdded),
            hur_sum = sum(HousingUnitsRemoved))

# Figure 9
# Number of Housing Units Being Added or Removed in Each Month (Faceted by year, grouped by activity of housing units)
colors <- c("Housing Units Added" = "red", "Housing Units Removed" = "blue")

ggplot(data = housing_units_activity_month, aes(x = IssuedMth)) +
  geom_line(aes(y = hua_sum, color = "Housing Units Added")) +
  geom_line(aes(y = hur_sum, color = "Housing Units Removed")) +
  labs(title = "Number of Housing Units Being Added or Removed in Each Month", 
       subtitle = "Faceted by year, grouped by activity of housing units",
       caption = "Figure 9: Number of housing units removed (blue) or added (red) each month in years 2019, 2020, 2021",
       x = "Month",
       y = "Number of Housing Units",
       color = "Housing Unit Activity") +
  scale_color_manual(values = colors) +
  facet_grid(rows = vars(IssuedYear_Factor)) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks = 10)

# Creating a data frame to store the respective total number of housing units that is added and removed in for all years 2020, 2021 in all 7 council districts.
housing_units_location <- building_permits_data %>%
  # Filtering by year, and filtering out all NA values of HousingUnitsAdded, HousingUnitsRemoved and CouncilDistrict_Factor
  filter(IssuedYear_Factor %in% c(2020, 2021),
         !is.na(HousingUnitsAdded),
         !is.na(HousingUnitsRemoved),
         !is.na(CouncilDistrict_Factor)) %>%
  # Grouping by year and council district
  group_by(IssuedYear_Factor, CouncilDistrict_Factor) %>%
  # Calculating the total number of housing units added and housing units removed
  summarize(hua_sum = sum(HousingUnitsAdded),
            hur_sum = sum(HousingUnitsRemoved))

# Figure 10
# Number of Housing Units Added in Each Council District (Grouped by year)
ggplot(data = housing_units_location,
       aes(x = CouncilDistrict_Factor,
           y = hua_sum,
           fill = IssuedYear_Factor)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Housing Units Added in Each Council District",
       subtitle = "Grouped by year",
       caption = "Figure 10: Number of housing units added each month in years 2020 (left, pink), 2021 (right, blue)",
       x = "Council District",
       y = "Number of Housing Units",
       fill = "Issued Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 11
# Number of Housing Units Removed in Each Council District (Grouped by year)
ggplot(data = housing_units_location,
       aes(x = CouncilDistrict_Factor,
           y = hur_sum,
           fill = IssuedYear_Factor)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Housing Units Removed in Each Council District",
       subtitle = "Grouped by year",
       caption = "Figure 11: Number of housing units added each month in years 2020 (left, pink), 2021 (right, blue)",
       x = "Council District",
       y = "Number of Housing Units",
       fill = "Issued Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 12
# Distribution of Log-Transformed Estimated Project Cost (Distribution of Log-Transformed Estimated Project Cost)
ggplot(data = building_permits_data,
       aes(x = log10(EstProjectCost))) +
  geom_histogram(binwidth = 0.3) +
  facet_grid(rows = vars(IssuedYear_Factor)) +
  labs(title = "Distribution of Log-Transformed Estimated Project Cost",
       subtitle = "Faceted by year",
       caption = "Figure 12: Distribution of log-transformed estimated project costs across years 2017, 2018, 2019, 2020, 2021",
       x = "Log-Transformed Estimated Project Cost",
       y = "Count") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 13
# Distribution of Log-Transformed Estimated Project Cost (For each year)
ggplot(data = building_permits_data %>%
         filter(!is.na(EstProjectCost)),
       aes(x = log10(EstProjectCost), y = IssuedYear_Factor)) +
  geom_boxplot() +
  labs(title = "Distribution of Log-Transformed Estimated Project Cost",
       subtitle = "For each year",
       caption = "Figure 13: Distribution of log-transformed estimated project costs for years 2017, 2018, 2019, 2020, 2021",
       x = "Log-Transformed Estimated Project Cost",
       y = "Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a table that prints the median and interquartile range values for estimated project costs for all years 2017, 2018, 2019, 2020, 2021
building_permits_data %>%
  # Filtering out all estimated project costs with value 0
  filter(EstProjectCost != 0) %>%
  # Grouping by year
  group_by(IssuedYear) %>%
  # Calculating median and IQR values for estimated project costs
  summarize(median_est_cost = median(EstProjectCost),
            iqr_est_cost = IQR(EstProjectCost))

# Figure 14
# Distribution of Log-Transformed Estimated Project Cost (Faceted by year and permit class mapped)
ggplot(data = building_permits_data %>%
         filter(PermitClassMapped != "N/A",
                !is.na(EstProjectCost)),
       aes(x = log10(EstProjectCost))) +
  geom_histogram(binwidth = 0.4) +
  facet_grid(IssuedYear_Factor ~ PermitClassMapped) +
  labs(title = "Distribution of Log-Transformed Estimated Project Cost",
       subtitle = "Faceted by year and permit class mapped",
       caption = "Figure 14: Distribution of log-transformed estimated project costs for residential and non-residential projects in years 2017, 2018, 2019, 2020, 2021",
       x = "Log-Transformed Estimated Project Cost",
       y = "Count") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Figure 15
# Distribution of Log-Transformed Estimated Project Cost (For each year, faceted by permit class mapped)
ggplot(data = building_permits_data %>%
         filter(PermitClassMapped != "N/A",
                !is.na(EstProjectCost)),
       aes(x = log10(EstProjectCost), y = IssuedYear_Factor)) +
  geom_boxplot() +
  facet_grid(cols = vars(PermitClassMapped)) +
  labs(title = "Distribution of Log-Transformed Estimated Project Cost",
       subtitle = "For each year, faceted by permit class mapped",
       caption = "Figure 15: Distribution of log-transformed estimated project costs for residential and non-residential projects in years 
       2017, 2018, 2019, 2020, 2021, faceted by permit class mapped",
       x = "Log-Transformed Estimated Project Cost",
       y = "Year") +
  theme_bw(base_size = 25) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a table that prints the median and interquartile range values for estimated project costs for residential and non-residential permits respectively for all years 2017, 2018, 2019, 2020, 2021
building_permits_data %>%
  # Filtering out all estimated project costs with value 0 and NA values of PermitClassMapped
  filter(EstProjectCost != 0,
         PermitClassMapped != "N/A") %>%
  # Grouping by year and permit class mapped
  group_by(PermitClassMapped, IssuedYear) %>%
  # Calculating median and IQR values for estimated project costs
  summarize(median_est_cost = median(EstProjectCost),
            iqr_est_cost = IQR(EstProjectCost))

# Creating a data frame that stores the average estimate project costs for all years 2017, 2018, 2019, 2020, 2021
average_cost_yearly <- building_permits_data %>%
  # Filtering out NA values of EstProjectCost
  filter(!is.na(EstProjectCost)) %>%
  # Grouping by year
  group_by(IssuedYear) %>%
  # Calculating median values for estimated project costs
  summarize(median_cost = median(EstProjectCost))

# Figure 16
# Average (median) estimated project costs in years 2017, 2018, 2019, 2020, 2021
ggplot(data = average_cost_yearly,
       aes(x = IssuedYear,
           y = median_cost)) +
  geom_line() +
  labs(title = "Average (Median) Estimated Project Cost for Each Year",
       caption = "Figure 16: Average (median) estimated project costs in years 2017, 2018, 2019, 2020, 2021",
       x = "Year",
       y = "Average (median) estimated project cost") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Creating a data frame that stores the average estimate project costs in each month for all years 2019, 2020, 2021
average_cost_monthly <- building_permits_data %>%
  # Filtering by year and filtering out NA values of EstProjectCost
  filter(!is.na(EstProjectCost),
         IssuedYear_Factor %in% c(2019, 2020, 2021)) %>%
  # Grouping by year and month
  group_by(IssuedYear_Factor, IssuedMth) %>%
  # Calculating median values for estimated project costs
  summarize(median_cost = median(EstProjectCost))

# Figure 17
# Average (Median) Estimated Project Cost for Each Month in 2019, 2020, 2021
ggplot(data = average_cost_monthly,
       aes(x = IssuedMth, 
           y = median_cost, 
           group = IssuedYear_Factor,
           color = IssuedYear_Factor)) +
  geom_line() +
  labs(title = "Average (Median) Estimated Project Cost for Each Month in 2019, 2020, 2021",
       caption = "Figure 17: Average (median) estimated project costs in each month for years 2019, 2020, 2021",
       x = "Month",
       y = "Average (median) estimated project cost",
       color = "Issued Year") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks = 10)