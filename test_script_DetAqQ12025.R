# load libraries
library(data.table)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(doBy)
library(cowplot)
library(foreign)

# set the working directory. 
#setwd("C:/Users/christine.calleja/Documents/CDHI_Analysis/Data/AQI_test/Data")
q1aq25 <- read.csv("//ds.detroitmi.gov/dept-data/Sustainability/00_CDHI/JustAir_Data/AQI_Data/Q1.2025.AQIs.Detroit.csv")
# load csv
df <- q1aq25

# view column names of df
names(df)
#View(df)

df_mod <- df[, c(1, 4, 8, 12, 17, 21, 25)]
#names(df_mod)

sensor <- read.csv("//ds.detroitmi.gov/dept-data/Sustainability/00_CDHI/JustAir_Data/DetMonitorPointsXY.csv")

sensor_mod <- sensor[, c(1, 2, 3)]

colnames(df_mod)[2] <- "date"

df_xy <- merge(df_mod, sensor_mod, by = "Name")

# Group by monitor
df_xy <- df_xy %>%
  group_by(Name)

# convert to Date to date variable fo x-axis
date = as.Date(df_xy$date)

# convert df_xy to data.table
setDT(df_xy)

# list AQI column names to be summarized
aqi_cols <- c("PM2.5.AQI", "PM10.AQI", "SO2.AQI", "NO2.AQI", "O3.AQI")

# loop through the columns, calculate summary stats per pollutant and combine into one table
  summary_all <- rbindlist(lapply(aqi_cols, function(col) {
    df_xy[
      ,.(
        mean=as.numeric(mean(get(col), na.rm = T)), 
        max=as.numeric(max(get(col), na.rm = T)), 
        min=as.numeric(min(get(col), na.rm = T)), 
        median=as.numeric(median(get(col), na.rm = T)), 
        StD=as.numeric(sd(get(col), na.rm = T))
      ), 
    by=Name
  ][, Pollutant := col] # adds column for each pollutant by name
}), use.names = T)

  
  # Create a list to store the plots
  plots <- list()
  
  for (poll in aqi_cols) {
    df_poll <- summary_all %>%
      filter(Pollutant == poll)
    
    p <- ggplot(df_poll, aes(x = Name, y = mean, fill = Name)) +
      geom_col() +
      labs(
        title = paste("Mean AQI by Monitor for", poll, "Q1 2025"),
        y = "Mean AQI",
        x = "Monitor"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    plots[[poll]] <- p
  }
  
  # Example: print PM2.5 plot
  print(plots[["PM2.5.AQI"]])
  print(plots[["PM10.AQI"]])
  print(plots[["SO2.AQI"]])
  print(plots[["NO2.AQI"]])
  print(plots[["O3.AQI"]])
  


ggplot(summary_all, aes(x = Name, y = mean, fill = Name)) +
  geom_col() +
  facet_wrap(~Pollutant, scales = "free_y") +
  labs(title = "Mean AQI by Monitor (per Pollutant) for Q1 2025",
       y = "Mean AQI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) +
  theme(legend.position = "none"))
  
summary_all_xy <- merge(summary_all, sensor_mod, by = "Name")

# establish AQI colors
aqi_levels <- data.frame(
  ymin = c(0, 51, 101, 151, 201, 301),
  ymax = c(50, 100, 150, 200, 300, 500),
  category = c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
  fill = c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#8f3f97", "#7e0023"),
  stringAsFactors = F
)

# Filter by singular pollutant
pm25_map <- summary_all_xy %>%
  filter(Pollutant == "PM2.5.AQI") %>%
  mutate(
    AQI_Category = cut(
      mean,
      breaks = c(aqi_levels$ymin[1], aqi_levels$ymax),
      labels = aqi_levels$category,
      include.lowest = T,
      right = T
    )
  )

# plot it on a map of the us  
ggplot(pm25_map) +
  borders("states", colour = "gray80", fill = "gray95") +  # US states background
  geom_point(
    aes(x = longitude, y = latitude, color = AQI_Category),
    size = 3
  ) +
  scale_color_manual(
    values = setNames(aqi_levels$fill, aqi_levels$category),
    drop = FALSE
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Mean PM2.5 AQI by Monitor Location",
    x = "Longitude", y = "Latitude",
    color = "AQI Category"
  ) +
  theme_minimal()

write.dbf(summary_all_xy, "2025q1_aqi_summary.dbf")

###

ggplot(summary_all_xy, aes(x = Name, y = value)) +
  geom_boxplot() +
  facet_wrap(~Pollutant, scales = "free_y", ncol = 1) +  # one graph per pollutant
  labs(
    title = "Distribution of AQI Readings per Monitor",
    x = "Monitor",
    y = "AQI"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate monitor names
    legend.position = "none"  # remove huge legend
  )

# create box and whisker summary plot per monitor
ggplot(summary_all_xy %>% filter(Pollutant == "PM2.5.AQI"), aes(x = Name, y = mean, fill = Name)) +
  geom_boxplot() +
  labs(title = "PM2.5 AQI Distribution by Monitor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Filter for PM2.5 only
pm25_df <- summary_all_xy %>% filter(Pollutant == "PM2.5.AQI")

# Summarise stats per monitor
pm25_stats <- pm25_df %>%
  group_by(Name) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE),
    min_val  = min(min, na.rm = TRUE),
    max_val  = max(max, na.rm = TRUE),
    sd_val   = sd(StD, na.rm = TRUE),
    .groups = "drop"
  )

# 3️⃣ Plot boxplot + stats text
ggplot(pm25_df, aes(x = Name, y ='PM2.5.AQI')) +
  geom_boxplot() +
  geom_text(
    data = pm25_stats,
    aes(
      x = Name,
      y = max_val + 5,  # place above max whisker
      label = paste0(
        "Mean=", round(mean_val, 1),
        "\nMin=", round(min_val, 1),
        "\nMax=", round(max_val, 1),
        "\nSD=", round(sd_val, 1)
      )
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    title = "PM2.5 AQI Distribution per Monitor",
    x = "Monitor",
    y = "PM2.5 AQI"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


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

