# set the working directory. 
#setwd("C:/Users/christine.calleja/Documents/CDHI_Analysis/Data/AQI_test/Data")
q1aq25 <- read.csv("//ds.detroitmi.gov/dept-data/Sustainability/00_CDHI/JustAir_Data/AQI_Data/Q1.2025.AQIs.Detroit.csv")
# load csv
df <- q1aq25

# view column names of df
names(df)
View(df)


df_mod <- df[, c(1, 4, 8, 12, 17, 21, 25)]
names(df_mod)

#library(dplyr)
#library(ggplot2)
#library(ggrepel)
#library(tidyverse)

sensor <- read.csv("//ds.detroitmi.gov/dept-data/Sustainability/00_CDHI/JustAir_Data/DetMonitorPointsXY.csv")

sensor_mod <- sensor[, c(1, 2, 3)]

colnames(df_mod)[2] <- "date"

df_xy <- merge(df_mod, sensor_mod, by = "Name")

# Group by monitor
df_xy <- df_xy %>%
  group_by(Name)

# convert to Date to date variable fo x-axis
date = as.Date(df_xy$date)
PM2.5AQI = df$PM2.5.AQI

plot.airquality <- function(monitor_name, df_xy) {
  # Ensure 'date' is Date class
  df_xy$date <- as.Date(df_xy$date)
  
  # Filter data for the selected monitor
  df_monitor <- df_xy %>% filter(Name == monitor_name)
  
  if (nrow(df_monitor) == 0) {
    message("No data found for monitor: ", monitor_name)
    return(NULL)
  }
  
  # AQI categories and fill colors
  aqi_levels <- data.frame(
    ymin = c(0, 51, 101, 151, 201, 301),
    ymax = c(50, 100, 150, 200, 300, 500),
    category = c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
    fill = c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#8f3f97", "#7e0023")
  )
  
  xmin <- min(df_monitor$date, na.rm = TRUE)
  xmax <- max(df_monitor$date, na.rm = TRUE)
  
  # Create plot
  p <- ggplot(df_monitor, aes(x = date, y = PM2.5.AQI)) +
    geom_rect(data = aqi_levels,
              aes(ymin = ymin, ymax = ymax, fill = category),
              xmin = xmin, xmax = xmax,
              inherit.aes = FALSE, alpha = 0.2) +
    scale_fill_manual(values = setNames(aqi_levels$fill, aqi_levels$category)) +
    geom_line(color = "turquoise4") +
    theme_minimal() + 
    labs(
      x = "", y = "PM 2.5 AQI",
      title = paste("AQI for PM 2.5 (Q1 2025) -", monitor_name)
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_text(angle = 50, hjust = 1),
      legend.position = "none"
    )
  
  print(p)
}
plot.airquality("3rd Avenue & West Euclid Street", df_xy)

