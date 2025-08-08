library(openair)
library(tidyverse)
library(worldmet)
library(lubridate)
library(colorspace)

head(mydata)

?polarPlot

# polar plot default poll = nox
polarPlot(mydata)
polarPlot(mydata, type = "year", poll = "no2")
polarPlot(mydata, type = "year", poll = "o3")
polarPlot(mydata, type = "year", poll = "pm10")
polarPlot(mydata, type = "year", poll = "so2")
polarPlot(mydata, type = "year", poll = "co")
polarPlot(mydata, type = "year", poll = "pm25")

# 2.5 Testing out type 
# splits data by month ~ average monthly concentractions across years consolidated
polarPlot(mydata, type = "month", poll = "pm25")

# splits data by month AND year ~ average monthly concentractions per month per year

polarPlot(mydata, type = "monthyear", poll = "pm25")

# splits data by season. (Northern by default)
polarPlot(mydata, type = "season", poll = "pm25")

#splits data by day of week
polarPlot(mydata, type = "weekday", poll = "pm25")

# splits data by Saturday, Sunday, weekday
polarPlot(mydata, type = "weekend", poll = "pm25")

# splits data by day time/night time
polarPlot(mydata, type = "daylight", poll = "pm25", fontsize = 20)

# splits data by daylight saving time and non-daylight saving time
polarPlot(mydata, type = "dst", poll = "pm25")

# splits data by wind direction into 8 sectors: N, NE, E, SE, S, SW, W, NW
polarPlot(mydata, type = "wd", poll = "pm25")

# splits data into year-seasonal intervals (December 2001 is considered part of winter 2002)
polarPlot(mydata, type = "seasonyear", poll = "pm25")

# divide data by air quality intervals
mydata$intervals <- cut(mydata$no2,
                        breaks = c(0, 50, 100, 150, 1000),
                        labels = c(
                          "Very low", "Low", "High",
                          "Very High"
                        ),
                        include.lowest = TRUE
)

# look at the data
head(mydata)

# 2.5.1
splitByDate(
  mydata,
  dates = "01/01/2002",
  labels = c("before", "after"),
  name = "scenario"
)

polarPlot(mydata, type = "intervals", poll = "no2")
#polarPlot(mydata, type = "scenario", poll = "pm25")

# 2.10 openairmaps packages used to create interactive HTML maps  - easier because it preserves resolution and dimension
library(openairmaps)
# use sample 2009 data
View(polar_data)
# plot nox values for year 2009 at the different stations
polarmap <- polarMap(polar_data, "nox")
buildPopup(
  data = polar_data,
  columns = c(
    "Site" = "site",
    "Site Type" = "site_type",
    "Date Range" = "date"
  )
) %>%
  polarMap("nox", popup = "popup")

buildPopup(
  data = polar_data,
  columns = c(
    "Site" = "site",
    "Site Type" = "site_type",
    "Date Range" = "date"
  )
) %>%
  polarMap("pm2.5", popup = "popup")

# save the html map
htmlwidgets::saveWidget(widget = polarmap, file = "polarmap_london_nox.html")

# To Open the HTML file in RStudio's Viewer Pane:
# Navigate to the "Files" pane in RStudio.
# Locate the saved HTML file (e.g., "my_plot.html").
# Click on the file name and choose View in Browser
###

# 4.1 get meteorological data
getMeta(lat = 42.3, lon = -83.0, returnMap = TRUE)
det_met <- importNOAA(code = "725370-94847", year = 2025)
ham_met <- importNOAA(code = "725375-14822", year = 2025)
#gp_met <- importNOAA(code = "997996-99999", year = 2015:2025)

# 4.2  Linking meteorological data with aq data
# import some air quality data and check the variables that exist
#est_datetime_string <- "2025-07-10 02:23:24 PM"
#est_datetime <- parse_date_time(est_datetime_string, orders = "%Y-%m-%d %I:%M:%s %p", tz = "America/New_York")
#utc_datetime <- with_tz(est_datetime, tzone = "UTC")
#print(est_datetime)
#print(utc_datetime)


# print first few lines of date
head(det_met)
head(ham_met)
#head(gp_met)
colnames(det_met)
colnames(ham_met)
#colnames(gp_met)


# Plot multiple plots on the same page and use a color scheme from colorspace package
a <- windRose(det_met, cols = qualitative_hcl(4, palette = "Dark 3"))
b <- windRose(ham_met, cols = qualitative_hcl(4, palette = "Dark 3"))

print(a, split = c(1, 1, 2, 1))
print(b, split = c(2, 1, 2, 1), newpage = FALSE)

# set the working directory. 
setwd("C:/Users/christine.calleja/Documents/CDHI_Analysis/Data/AQI_test/Data")

# load csv
#q1jmdf <- read.csv("Q1.2025.JeromeandMoran.csv")
#q1detdf <- read.csv("Q1.2025.AQIs.Detroit.csv")
#q1detFinaldf <- read.csv("Q1.2025.final.Detroit.csv")
det2024con <- read.csv("Detroit.2024.alldata.csv")
View(det2024con)
View(q1detdf)
View(q1detFinaldf)
summarise()
df <- read.csv("DelRay-last_24_hours_JustAir.csv")
colnames(df)
colnames(df) <- c("date_string", "no2", "pm10", "pm25", "maxaqi_value", "maxaqi_poll")  
head(df)
df$date_string <- parse_date_time(df$date_string, orders = "%Y-%m-%d %I:%M:%s %p", tz = "America/New_York")
#df$date <- with_tz(est_datetime, tzone = "UTC")
head(df)
colnames(df)[1] <- "date"
#df_mod <- df[, c(1, 4, 8, 12, 17, 21, 25)]
#colnames(df) <- c("name", "city", "ZIP",  "date", "time",  "state", "no2",  "so2", "pm1",  "pm25", "pm10",  "tf",  "rh", "o3",  "co", "no")
#head(df_mod)

names(df)
head(df)
# add columns for merging wind speed and wind direction into aq data
cols_to_add=c("ws", "wd", "air_temp")
df[,cols_to_add]=""

df_met <- left_join(df, det_met, by = "date")
View(df_met)
mydata <- df_met
polarPlot(df_met)
colnames(df_met)
colnames(df_met)[15] <- "ws"
colnames(df_met)[16] <- "wd"

polarPlot(mydata, type = "wd")
windRose(mydata, type = "wd")

a <- windRose(mydata)
b <- polarPlot(mydata)
print(a, split = c(1, 1, 2, 1))
print(b, split = c(2, 1, 2, 1), newpage = FALSE)

# STEP 1: Make a polarPlot
polar <- polarPlot(mydata, "pm25")

# STEP 2: Open a graphics device
png(
  filename = "polarplot_delray_pm25.png",
  width = 6, height = 6, units = "in", res = 300
)

# STEP 3: Print the plot
polar
