# load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(doBy)
library(cowplot)
library(foreign)
library(scales)

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

df <- data.frame(df_xy)
head(df)

Monitor <- df$Name
PM2.5AQI <- df$PM2.5.AQI
boxplot(PM2.5AQI ~ Monitor, df)

# AQI background levels
aqi_levels <- data.frame(
  ymin = c(0, 51, 101, 151),
  ymax = c(50, 100, 150, 200),
  category = c("Good", "Moderate", "Unhealthy for Sensitive"),
  fill = c("#00e400", "#ffff00", "#ff7e00")
)

# Without max min labeled
# PM2.5 boxplot with AQI background
ggplot(df, aes(x = Name, y = PM2.5.AQI)) +
  # Background AQI bands
  geom_rect(
    data = aqi_levels,
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = ymin, ymax = ymax,
      fill = category
    ),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_boxplot(outlier.color = "black") +
  scale_fill_manual(values = setNames(aqi_levels$fill, aqi_levels$category), guide = "none") +
  labs(
    title = "PM2.5 AQI by Monitor",
    x = "Monitor",
    y = "PM2.5 AQI"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

poll <- c("PM2.5.AQI", "PM10.AQI", "SO2.AQI", "NO2.AQI", "O3.AQI")
aqi_cols <- c("PM2.5.AQI", "PM10.AQI", "SO2.AQI", "NO2.AQI", "O3.AQI")

plots <- list()

for (poll in aqi_cols) {
  
  # Prep data for plotting
  df_poll <- df %>%
    select(Name, value = all_of(poll)) %>%
    filter(!is.na(value))  # remove NA rows
  
  # Skip if empty
  if (nrow(df_poll) == 0) next
  
  # Compute min/max per monitor, removing Inf/-Inf
  df_minmax <- df_poll %>%
    group_by(Name) %>%
    summarise(
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(min_val), is.finite(max_val))
  
  # Make boxplot
  p <- ggplot(df_poll, aes(x = Name, y = value)) +
    # AQI bands
    geom_rect(
      data = aqi_levels,
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = category),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    geom_boxplot(outlier.color = "black") +
    # Min points
    geom_point(
      data = df_minmax,
      aes(x = Name, y = min_val),
      color = "red", size = 2
    ) +
    geom_text(
      data = df_minmax,
      aes(x = Name, y = min_val, label = round(min_val, 1)),
      vjust = 1.5, size = 2, color = "red"
    ) +
    # Max points
    geom_point(
      data = df_minmax,
      aes(x = Name, y = max_val),
      color = "blue", size = 2
    ) +
    geom_text(
      data = df_minmax,
      aes(x = Name, y = max_val, label = round(max_val, 1)),
      vjust = -0.5, size = 2, color = "blue"
    ) +
    scale_fill_manual(values = setNames(aqi_levels$fill, aqi_levels$category), guide = "none") +
    labs(
      title = paste(poll, "by Monitor"),
      x = "Monitor",
      y = "AQI"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save PNG
  ggsave(
    filename = paste0(gsub("\\.", "_", poll), "_boxplot.png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  plots[[poll]] <- p
}
p
print(plots[["PM2.5.AQI"]])
print(plots[["PM10.AQI"]])
print(plots[["SO2.AQI"]])
print(plots[["NO2.AQI"]])
print(plots[["O3.AQI"]])


library(gridExtra)



aqi_cols <- c("PM2.5.AQI", "PM10.AQI", "SO2.AQI", "NO2.AQI", "O3.AQI")

# quick checks
cat("Columns present in df:\n")
print(intersect(aqi_cols, names(df)))
cat("Columns missing from df (these will be skipped):\n")
print(setdiff(aqi_cols, names(df)))

# check non-NA counts
counts <- sapply(intersect(aqi_cols, names(df)), function(col) sum(!is.na(df[[col]])))
cat("Non-NA counts per pollutant:\n")
print(counts)

plots <- list()
saved_files <- character(0)

# make sure Name column exists
if (!"Name" %in% names(df)) stop("Column 'Name' not found in df — please check the monitor name column.")

for (poll in aqi_cols) {
  if (!(poll %in% names(df))) {
    message("Skipping ", poll, ": column not found in df.")
    next
  }
  
  n_non_na <- sum(!is.na(df[[poll]]))
  if (n_non_na == 0) {
    message("Skipping ", poll, ": all values are NA.")
    next
  }
  
  # Prepare df_poll
  df_poll <- df %>%
    select(Name, value = all_of(poll)) %>%
    filter(!is.na(value))
  
  if (nrow(df_poll) == 0) {
    message("Skipping ", poll, ": no non-NA rows after select/filter.")
    next
  }
  
  # compute min/max per monitor and drop Inf/-Inf
  df_minmax <- df_poll %>%
    group_by(Name) %>%
    summarise(
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(min_val), is.finite(max_val))
  
  # Build plot
  p <- ggplot(df_poll, aes(x = Name, y = value)) +
    geom_rect(
      data = aqi_levels,
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = category),
      inherit.aes = FALSE, alpha = 0.2
    ) +
    geom_boxplot(outlier.color = "black") +
    scale_fill_manual(values = setNames(aqi_levels$fill, aqi_levels$category), guide = "none") +
    labs(title = paste0(poll, " by Monitor"), x = "Monitor", y = "AQI") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # add min/max points/labels only if we have them
  if (nrow(df_minmax) > 0) {
    p <- p +
      geom_point(data = df_minmax, aes(x = Name, y = min_val), color = "red", size = 2) +
      geom_text(data = df_minmax, aes(x = Name, y = min_val, label = round(min_val, 1)),
                vjust = 1.5, size = 2, color = "red") +
      geom_point(data = df_minmax, aes(x = Name, y = max_val), color = "blue", size = 2) +
      geom_text(data = df_minmax, aes(x = Name, y = max_val, label = round(max_val, 1)),
                vjust = -0.5, size = 2, color = "blue")
  } else {
    message("No finite min/max values for ", poll, " — plotting boxplot without min/max points.")
  }
  
  # save file-safe name and save
  safe_name <- gsub("[^A-Za-z0-9_]", "_", poll)   # replace problem chars
  fname <- paste0(safe_name, "_boxplot.png")
  
  ggsave(filename = fname, plot = p, width = 8, height = 6, dpi = 300)
  message("Saved plot for ", poll, " -> ", fname)
  
  plots[[poll]] <- p
  saved_files <- c(saved_files, fname)
}

# optionally display plots if any were created
if (length(plots) > 0) {
  do.call(grid.arrange, c(plots, ncol = 2))
} else {
  message("No plots created. Check the messages above for why each pollutant was skipped.")
}

# summary
cat("Saved files:\n")
print(saved_files)


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

#aqi_levels <- data.frame(
#    ymin = c(0, 51, 101, 151, 201, 301),
 #   ymax = c(50, 100, 150, 200, 300, 500),
  #  category = c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
   # fill = c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#8f3f97", "#7e0023")
#  )

  # Create a list to store the plots
  plots <- list()
  
  for (poll in aqi_cols) {
    df_poll <- summary_all_xy %>%
      filter(Pollutant == poll, !is.na(mean))
    
    # Set x range to cover all bars
    xmin <- 0.5
    xmax <- nrow(df_poll) + 0.5
    
    p <- ggplot(df_poll, aes(x = Name, y = mean, fill = Name)) +
      # Background AQI categories
      geom_rect(
        data = aqi_levels,
        aes(
          xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax,
          fill = category
        ),
        inherit.aes = FALSE,
        alpha = 0.2
      ) +
      geom_col() +
      scale_fill_manual(
        values = c(setNames(aqi_levels$fill, aqi_levels$category),
                   setNames(rep("grey60", length(unique(df_poll$Name))), unique(df_poll$Name))),
        guide = "none"
      ) +
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

df_xy
df_xy$date <- as.Date(df_xy$date)
###
#  # create time series plot
#  p <- ggplot(df_mod, aes(date, SO2.AQI)) + geom_line() +
#    geom_rect(data = aqi_levels, aes(xmin = min(date), xmax = max(date),
#                                     ymin = ymin, ymax = ymax, fill = category),
#              inherit.aes = FALSE, alpha = 0.2) +
#    scale_fill_manual(values = setNames(aqi_levels$fill, aqi_levels$category)) +
    
#    # Line plot
#    geom_line(color="turquoise4") +
#    theme_minimal() + 
#    labs(x="", y="SO2 AQI", title="AQI for SO2 (Q1 2025)") +
#    theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))
  
  # display time series plot
#  p + theme(legend.position='none') + 
#    theme(axis.text.x=element_text(angle=50, hjust=1)) 
#  
#  p
  
 ### 

ggplot(summary_all, aes(x = Name, y = mean, fill = Name)) +
  geom_col() +
  facet_wrap(~Pollutant, scales = "free_y") +
  labs(title = "Mean AQI by Monitor (per Pollutant) for Q1 2025",
       y = "Mean AQI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) +
  theme(legend.position = "none"))
  
summary_all_xy <- merge(summary_all, sensor_mod, by = "Name")

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

#write.dbf(summary_all_xy, "2025q1_aqi_summary.dbf")

df_wide <- summary_all_xy %>%
  pivot_wider(
    id_cols = c(Name, latitude, longitude),
    names_from = Pollutant,
    values_from = c(mean, median, min, max, StD),
    names_glue = "{Pollutant}_{.value}"
  )

df_wide_clean <- as.data.frame(df_wide)

#write.dbf(df_wide_clean, "2025q1_aqi_summary_wide.dbf")

###

# Filter for PM2.5 only
pm25_df <- summary_all_xy %>% filter(Pollutant == "PM2.5.AQI")

# Summarize stats per monitor
pm25_stats <- pm25_df %>%
  group_by(Name) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE),
    min_val  = min(min, na.rm = TRUE),
    max_val  = max(max, na.rm = TRUE),
    sd_val   = sd(StD, na.rm = TRUE),
    .groups = "drop"
  )

# Plot time series per monitor per pollutant
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

