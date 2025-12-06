library(XML)
library(data.table)
library(lubridate)
library(ggplot2)
library(stringr)
library(leaflet)
library(calendR)

# IMPORT AND CLEAN DATA --------------------------------------------------------

# import
xml_dat = xmlParse("data/export.xml")

# split records & clean times
health_dat = as.data.table(XML:::xmlAttrsToDataFrame(xml_dat["//Record"]))
health_dat[, startDate := ymd_hms(startDate, tz = "America/Chicago")]
health_dat[, endDate := ymd_hms(endDate, tz = "America/Chicago")]

# split workouts & clean times
workout_dat = as.data.table(XML:::xmlAttrsToDataFrame(xml_dat["//Workout"]))
workout_dat[, startDate := ymd_hms(startDate, tz = "America/Chicago")]
workout_dat[, endDate := ymd_hms(endDate, tz = "America/Chicago")]

# split summaries & clean times
summary_dat = as.data.table(XML:::xmlAttrsToDataFrame(xml_dat["//ActivitySummary"]))
summary_dat[, dateComponents := ymd(dateComponents)]

# HANDWASH ANALYSIS ------------------------------------------------------------

# subset to mindfulness records
handwash_dat = health_dat[type == "HKCategoryTypeIdentifierHandwashingEvent"]

# only meaningful item is duration
handwash_dat = handwash_dat[, .(startDate, endDate)]
handwash_dat[, sessionLength := difftime(endDate, startDate, units = "mins")]
handwash_dat[, sessionLength := as.numeric(sessionLength, units = "mins")]
handwash_dat[, sessionID := 1:.N]

# get number of handwashes per day with 0 for missing days
all_dates = data.table(date = seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"))
handwash_daily = merge(all_dates, handwash_dat[, .N, by = .(date = as.Date(startDate))], by = "date", all.x = TRUE)
setnames(handwash_daily, "N", "numHandwashes")
handwash_daily[is.na(numHandwashes), numHandwashes := 0]

#get number of handwashes per day in 2022 with 0 for missing days
all_dates_2022 = data.table(date = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"))
handwash_2022 = merge(all_dates_2022, handwash_daily, by = "date", all.x = TRUE)
handwash_2022[is.na(numHandwashes), numHandwashes := 0]
handwash_2022[, dayOfYear := yday(date)]

# plot handwashes over time
ggplot(handwash_daily, aes(x = date, y = numHandwashes)) +
  geom_line() +
  geom_point() +
  labs(title = "Daily Handwashing Events Over Time",
       x = "Date",
       y = "Number of Handwashing Events") +
  theme_minimal()

calendR(from = "2025-01-01", # Custom start date
        to = "2025-12-31",
        special.days = handwash_daily$numHandwashes[handwash_daily$date >= as.Date("2025-01-01") & handwash_daily$date <= as.Date("2025-12-31")],
        gradient = TRUE,
        special.col = "blue",
        legend.pos = "right",     # Position of the legend
        legend.title = "Legend",
        title = "Handwashing Events 2025")

# loop through all the files in the data/routes folder and get the first latitude and longitude listed and lookup the geolocation
library(XML)

route_files <- list.files("data/routes", pattern = "*.gpx", full.names = TRUE)

for (file in route_files) {

  gpx_data <- xmlParse(file)

  # extract first trkpt
  first_pt <- xpathSApply(
    gpx_data,
    "/gpx/trk/trkseg/trkpt[1]",
    function(x) c(
      lat = xmlGetAttr(x, "lat"),
      lon = xmlGetAttr(x, "lon")
    )
  )

  # xpathSApply returns a matrix (1 column)
  lat <- as.numeric(first_pt["lat"])
  lon <- as.numeric(first_pt["lon"])

  print(paste("File:", basename(file),
              "Lat:", lat,
              "Lon:", lon))
}


# loop through all the files in the data/routes folder and get the time of the activity
for (file in route_files) {
  gpx_data <- xmlParse(file)
  time <- xpathSApply(gpx_data, "///time", xmlValue)

  print(paste("File:", file, "Time:", time))
}

# READING AND PARSING OUTPUT FILE -----------------------------------------------

# make table with date, city, state, country
location_data <- data.table(date = character(), city = character(), state = character(), country = character())

# Specify the file path
file_path <- "output.txt"

# Open a file connection in read mode
con <- file(file_path, "r")

# Loop to read and process lines until the end of the file
while (TRUE) {
  line <- readLines(con, n = 1) # Read one line
  
  # Check if the end of the file is reached
  if (length(line) == 0) {
    break 
  }
  
  # Process the current line
  # print(line)

  # add date, city, state, country to location_data for each line
  location_data <- rbind(location_data, {
    # Extract date
    date <- str_extract(line, "\\d{4}-\\d{2}-\\d{2}")
    
    # Extract city, state, country
    location_parts <- str_match(line, "in (.*), (.*), (.*)\\. ")[, 2:4]
    city <- location_parts[1]
    state <- location_parts[2]
    country <- location_parts[3]
    
    list(date = date, city = city, state = state, country = country)
  })

  # You can add your specific parsing logic here
}
print(location_data)

# Close the file connection
close(con)

# delete duplicate dates keeping the first occurrence
location_data <- location_data[!duplicated(location_data$date)]

# add missing dates from 2020-01-01 to 2025-12-31 with NA for city, state, country
all_dates <- data.table(date = seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"))
location_data[, date := as.Date(date)]
location_data <- merge(all_dates, location_data, by = "date", all.x = TRUE)
setorder(location_data, date)
# write to csv
fwrite(location_data, "location_data.csv")

# get the state for each day and add it to calendar in gradient format 
library(randomcoloR)
num_colors <- length(unique(na.omit(location_data$state)))
calendar_colors <- distinctColorPalette(num_colors)
state_colors <- setNames(calendar_colors, unique(na.omit(location_data$state)))
calendR(from = "2025-01-01", # Custom start date
        to = "2025-12-31",
        special.days = location_data$state[location_data$date >= as.Date("2025-01-01") & location_data$date <= as.Date("2025-12-31")],
        gradient = TRUE,
        special.col = state_colors,
        legend.pos = "right",     # Position of the legend
        legend.title = "Legend",
        title = "State by Day 2025")

# get most common city not including NAs
most_common_city <- location_data[!is.na(city), .N, by = city][order(-N)][1]
print(most_common_city)

# make calendar were the color is based on whether the city is the most common city or not
location_data[, is_most_common := ifelse(city == most_common_city$city, "Most Common City", "Other City")]
calendR(from = "2025-01-01", # Custom start date
        to = "2025-12-31",
        special.days = location_data$is_most_common[location_data$date >= as.Date("2025-01-01") & location_data$date <= as.Date("2025-12-31")],
        gradient = TRUE,
        special.col = c("green", "red"),
        legend.pos = "right",     # Position of the legend
        legend.title = "Legend",
        title = "Location by Day 2025")