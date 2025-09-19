# NAAQS Averages
library(dplyr)
library(zoo)  # for rolling averages
library(lubridate)

# Example: df24_xy is your air sensor data frame
# Columns: Name, date, NO2, SO2, PM1, PM2.5, PM10, O3, CO, NO

# Make sure date is in POSIXct
df24_xy <- df24_xy %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"))

# ---- Carbon Monoxide (CO) ----
co_check2024 <- df24_xy %>%
  group_by(Name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    # 1-hour exceedance
    exceed_1h = CO > 35,
    
    # 8-hour rolling mean (align to right so each value = last 8 hours)
    CO_8h = rollapply(CO, width = 8, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    exceed_8h = CO_8h > 9
  ) %>%
  summarise(
    exceed_1h_count = sum(exceed_1h, na.rm = TRUE),
    exceed_8h_count = sum(exceed_8h, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Carbon Monoxide (CO) ----
co_check2024 <- df24_xy %>%
  group_by(Name) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    # Flags for CO exceedances
    exceed_CO_1h  = CO > 35,  # 1-hour standard
    CO_8h         = rollapply(CO, width = 8, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    exceed_CO_8h  = CO_8h > 9
  ) %>%
  ungroup()

print(co_check2024)
