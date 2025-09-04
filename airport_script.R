library(tidyverse)
library(dplyr)
library(readr)
airport_project_data2 <- read_csv("airport_project_data2.csv")
View(airport_project_data2)
str(airport_project_data2)

unique(airport_project_data2$`Wildlife Collision Status`)
unique(airport_project_data2$`Phase of Flight`)
unique(airport_project_data2$`Bird/Wildlife Species`)
unique(airport_project_data2$`Effect on Flight`)
unique(airport_project_data2$`Wildlife (GI)`)
unique(airport_project_data2$`Wildlife specie daily log`)


library(dplyr)
library(tidyr)
library(lubridate)

# Step 1: Clean date and extract year
airport_data <- airport_project_data2 %>%
  mutate(
    Date = dmy(Date),     # parse DD/MM/YYYY
    Year = year(Date)     # extract year
  )

### 1. Bird/Wildlife Species
bird_summary_long <- airport_data %>%
  filter(!is.na(`Bird/Wildlife Species`)) %>%       # drop NA values
  group_by(Year, `Bird/Wildlife Species`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Year)

bird_summary_wide <- bird_summary_long %>%
  pivot_wider(
    names_from = `Bird/Wildlife Species`,
    values_from = Count,
    values_fill = list(Count = 0)
  ) %>%
  arrange(Year)

### 2. Effect on Flight
effect_summary_long <- airport_data %>%
  filter(!is.na(`Effect on Flight`)) %>%
  group_by(Year, `Effect on Flight`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Year)

effect_summary_wide <- effect_summary_long %>%
  pivot_wider(
    names_from = `Effect on Flight`,
    values_from = Count,
    values_fill = list(Count = 0)
  ) %>%
  arrange(Year)

### 3. Phase of Flight
phase_summary_long <- airport_data %>%
  filter(!is.na(`Phase of Flight`)) %>%
  group_by(Year, `Phase of Flight`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Year)

phase_summary_wide <- phase_summary_long %>%
  pivot_wider(
    names_from = `Phase of Flight`,
    values_from = Count,
    values_fill = list(Count = 0)
  ) %>%
  arrange(Year)

### 4. Wildlife Collision Status
collision_summary_long <- airport_data %>%
  filter(!is.na(`Wildlife Collision Status`)) %>%
  group_by(Year, `Wildlife Collision Status`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Year)

collision_summary_wide <- collision_summary_long %>%
  pivot_wider(
    names_from = `Wildlife Collision Status`,
    values_from = Count,
    values_fill = list(Count = 0)
  ) %>%
  arrange(Year)


# Merge them by Year
combined_summary <- bird_summary_wide %>%
  full_join(effect_summary_wide, by = "Year") %>%
  full_join(phase_summary_wide, by = "Year") %>%
  full_join(collision_summary_wide, by = "Year") %>%
  arrange(Year)

# Save as CSV
write.csv(combined_summary, "combined_summary.csv", row.names = FALSE)

# Add identifiers before combining
bird_long <- bird_summary_long %>% mutate(Category = "Bird/Wildlife Species")
effect_long <- effect_summary_long %>% mutate(Category = "Effect on Flight")
phase_long <- phase_summary_long %>% mutate(Category = "Phase of Flight")
collision_long <- collision_summary_long %>% mutate(Category = "Wildlife Collision Status")

# Stack them together
combined_long <- bind_rows(bird_long, effect_long, phase_long, collision_long)

# Save as CSV
write.csv(combined_long, "combined_long_summary.csv", row.names = FALSE)



library(dplyr)
library(lubridate)
library(tidyr)

quarterly_report <- airport_project_data2 %>%
  mutate(
    Date = dmy(Date),  # parse "dd/mm/yyyy"
    Quarter = case_when(
      month(Date) %in% 1:3 ~ "Jan–Mar",
      month(Date) %in% 4:6 ~ "Apr–Jun",
      month(Date) %in% 7:9 ~ "Jul–Sep",
      month(Date) %in% 10:12 ~ "Oct–Dec"
    )
  ) %>%
  filter(!is.na(`Wildlife specie daily log`))   # drop rows where species is NA

# Summarize into wide format
summary_report <- quarterly_report %>%
  group_by(Quarter, `Wildlife specie daily log`) %>%
  summarise(Total = sum(`Daily log wildlife no`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = `Wildlife specie daily log`,
    values_from = Total,
    values_fill = 0   # fill missing species with 0
  )

# View result
print(summary_report)

View(summary_report)

write.csv(summary_report, "summary_report.csv", row.names = FALSE)



library(dplyr)
library(tidyr)

day_period <- airport_project_data2 %>%
  # drop rows with missing species
  filter(!is.na(`Wildlife specie daily log`)) %>%
  mutate(
    # Convert hms to numeric seconds
    secs = as.numeric(Time),
    hr   = secs %/% 3600,
    mn   = (secs %% 3600) %/% 60,
    
    # Day period classification
    Day_period = case_when(
      hr >= 6  & hr < 11                    ~ "Morning",
      hr >= 11 & hr < 16                    ~ "Afternoon",
      (hr >= 16 & hr < 19) | (hr == 19 & mn == 0) ~ "Evening",
      (hr < 6) | (hr > 19) | (hr == 19 & mn >= 1) ~ "Night",
      TRUE                                   ~ NA_character_
    ),
    Day_period = factor(
      Day_period,
      levels = c("Morning", "Afternoon", "Evening", "Night"),
      ordered = TRUE
    )
  ) %>%
  filter(!is.na(Day_period)) %>%   # drop rows outside defined ranges
  group_by(Day_period, `Wildlife specie daily log`) %>%
  summarise(Total = sum(`Daily log wildlife no`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = `Wildlife specie daily log`,
    values_from = Total,
    values_fill = 0
  ) %>%
  arrange(Day_period)

print(day_period)

View(day_period)
write.csv(day_period, "Day_Periods_Wildlife_Activity.csv", row.names = FALSE)

library(dplyr)
library(lubridate)

# Ensuring Date column is Date type
df <- airport_data %>%
  mutate(Date = as.Date(Date)) 

# Drop NA wildlife, extract month as ordered factor
df <- airport_data %>%
  filter(!is.na(`Bird/Wildlife Species`)) %>%
  mutate(Month = month(Date, label = TRUE, abbr = TRUE))  
# abbr=TRUE -> Jan, Feb, ...; abbr=FALSE -> January, February...

# Summarize counts per month (Jan–Dec order preserved)
month_summary <- df %>%
  group_by(Month) %>%
  summarise(Wildlife_Count = n(),
            .groups = "drop")

print(month_summary)

# Species by month (still ordered Jan–Dec)
species_month_summary <- df %>%
  group_by(Month, `Bird/Wildlife Species`) %>%
  summarise(Count = n(),
            .groups = "drop")

print(species_month_summary)

# Save monthly summary
write.csv(month_summary, "month_summary.csv", row.names = FALSE)

# Save species × month summary
write.csv(species_month_summary, "species_month_summary.csv", row.names = FALSE)