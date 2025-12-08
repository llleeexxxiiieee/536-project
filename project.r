library(XML)
library(data.table)
library(lubridate)
library(ggplot2)
library(stringr)
library(leaflet)
library(calendR)
library(stringr)

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
print(summary_dat)

# HANDWASH ANALYSIS ------------------------------------------------------------

# subset to handwashing records
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

# READING AND PARSING LOCATIONS FILE -----------------------------------------------

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

# HEARTRATE ANALYSIS ------------------------------------------------------------

# subset to heart rate records
heartrate_dat = health_dat[type == "HKQuantityTypeIdentifierHeartRate"]
heartrate_dat[, value := as.numeric(value)]
heartrate_dat[, date := as.Date(startDate, tz = "America/Chicago")]
print(heartrate_dat)

# calculate average heart rate per day
heartrate_daily = heartrate_dat[, .(avgHeartRate = mean(value, na.rm = TRUE)), by = date]
print(heartrate_daily)

# find the high and low heart rate per day with time stamps
heartrate_daily_extremes = heartrate_dat[, .(maxHeartRate = max(value, na.rm = TRUE),
                                            maxTime = startDate[which.max(value)],
                                            minHeartRate = min(value, na.rm = TRUE),
                                            minTime = startDate[which.min(value)]), by = date]
print(heartrate_daily_extremes)

# find first and last heart rate time per seperate day
heartrate_daily_times = heartrate_dat[, .(firstTime = min(startDate),
                                          lastTime = max(startDate)), by = date] 
print(heartrate_daily_times)

# print all heart rate data for 2025-01-01
print(heartrate_dat[as.Date(startDate) == as.Date("2025-01-01")])


# plot average heart rate over 2025 
ggplot(heartrate_daily[date >= as.Date("2025-01-01") & date <= as.Date("2025-12-31")], aes(x = date, y = avgHeartRate)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Daily Heart Rate Over Time (2025)",
       x = "Date",
       y = "Average Heart Rate (bpm)") +
  theme_minimal()

# DAILY LOG FORMATTING ------------------------------------------

#select 20 random dates from health_dat
set.seed(123) # for reproducibility
random_dates <- sample(unique(as.Date(health_dat$startDate, tz = "America/Chicago")), 20)
print(random_dates)

# for 2025-01-01 get all the data from workout_dat and print to console
date_to_check <- as.Date("2023-01-09", tz = "America/Chicago")
day_activity <- workout_dat[as.Date(startDate, tz = "America/Chicago") == date_to_check]
print(day_activity)

day_heartrate_extremes <- heartrate_daily_extremes[date == date_to_check]
day_heartrate <- heartrate_daily_times[date == date_to_check]
# print(paste("On", date_to_check, "the person put their Apple watch on at", day_heartrate$firstTime, "and took it off at", day_heartrate$lastTime))
day_location <- location_data[date == date_to_check][1]
# print(paste("On", date_to_check, "at", day_activity$startDate, "the person did a", str_match(day_activity$workoutActivityType, "HKWorkoutActivityType(.*)")[,2], "workout that lasted", difftime(day_activity$endDate, day_activity$startDate, units = "mins"), "minutes."))

# put events from above into a data table and sort events by time
day_log <- data.table(
  time = c(day_heartrate$firstTime, day_activity$startDate, day_activity$endDate, day_heartrate$lastTime, day_heartrate_extremes$maxTime, day_heartrate_extremes$minTime),
  event = c("Put on Apple Watch", 
            paste("Started", str_match(day_activity$workoutActivityType, "HKWorkoutActivityType(.*)")[,2], "workout"), 
            paste("Ended", str_match(day_activity$workoutActivityType, "HKWorkoutActivityType(.*)")[,2], "workout"), 
            "Took off Apple Watch",
            paste("Max Heart Rate:", day_heartrate_extremes$maxHeartRate),
            paste("Min Heart Rate:", day_heartrate_extremes$minHeartRate))
)
setorder(day_log, time) 

print(paste("On", date_to_check, "the person was in", day_location$city, ",", day_location$state, ",", day_location$country))
print(day_log)

# SIMILARITY ANALYSIS BETWEEN DAYS ------------------------------------------------

# for each day check if city is the same as the most common city of the previous two weeks

# loop through each day in 2025
dates_2025 <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
# print(dates_2025)
for (i in 15:length(dates_2025)) {
  current_date <- dates_2025[i]
  # print(current_date)
  # get the most common city in the previous two weeks
  start_date <- dates_2025[i - 7]
  end_date <- dates_2025[i - 1]
  previous_two_weeks <- location_data[date >= start_date & date <= end_date]
  # print(start_date)
  # print(end_date)
  # print(previous_two_weeks)
  most_common_city_prev <- previous_two_weeks[!is.na(city), .N, by = city][order(-N)][1]
  # print(most_common_city_prev)
  
  # get the city for the current day
  current_day_city <- location_data[date == current_date]$city
  
  # compare and print result
  if (!is.na(current_day_city) && !is.na(most_common_city_prev$city) && current_day_city == most_common_city_prev$city) {
    print(paste("On", current_date, "the person was in their most common city:", current_day_city))
  } else {
    print(paste("On", current_date, "the person was NOT in", most_common_city_prev$city, ", they were in", current_day_city))
  }
}

# create a daily summary data table with dates for 2025 added
daily_summary <- data.table()

# for every day of 2025, get the location, the number of workouts, the types of workouts, the duration of workouts, the average heart rate, the first heart rate time, and the last heart rate time and add to a data table with 0 or NA for missing values
daily_summary <- data.table()

for (current_date in seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")) {

  ## --- Location ---
  location <- location_data[date == current_date]
  city_val <- if (nrow(location) > 0) location$city else NA_character_

  ## --- Workouts ---
  workouts <- workout_dat[as.Date(startDate, tz = "America/Chicago") == current_date]
  num_workouts <- nrow(workouts)

  workout_types <- if (num_workouts > 0) {
    paste(str_match(workouts$workoutActivityType, "HKWorkoutActivityType(.*)")[,2], collapse = ", ")
  } else {
    NA_character_
  }

  total_duration <- if (num_workouts > 0) {
    as.numeric(sum(difftime(workouts$endDate, workouts$startDate, units = "mins")))
  } else {
    0
  }

  ## --- Heart rate ---
  hr_row <- heartrate_daily[date == current_date]

  avg_hr <- if (nrow(hr_row) > 0) hr_row$avgHeartRate else NA_real_

  # Fix empty indexing: if no row, set NA
  first_heartrate_time <- heartrate_daily_times[date == current_date]$firstTime
  first_heartrate_time <- if (length(first_heartrate_time) == 0) NA else first_heartrate_time

  last_heartrate_time <- heartrate_daily_times[date == current_date]$lastTime
  last_heartrate_time <- if (length(last_heartrate_time) == 0) NA else last_heartrate_time

  ## --- Build daily row ---
  today_summary <- data.table(
    date = as.Date(current_date),
    city = city_val,
    num_workouts = num_workouts,
    workout_types = workout_types,
    total_duration = total_duration,
    avg_heart_rate = avg_hr,
    first_heartrate_time = first_heartrate_time,
    last_heartrate_time = last_heartrate_time
  )

  daily_summary <- rbind(daily_summary, today_summary, fill = TRUE)
}

print(daily_summary)

# for every day in daily_summary, add a similarity score value
daily_summary[, similarity_score := 0]
for (i in 8:nrow(daily_summary)) {
  current_city <- daily_summary[i]$city
  previous_two_weeks <- daily_summary[(i-7):(i-1)]
  most_common_city_prev <- previous_two_weeks[!is.na(city), .N, by = city][order(-N)][1]
  
  if (!is.na(current_city) && !is.na(most_common_city_prev$city) && current_city == most_common_city_prev$city) {
    daily_summary[i, similarity_score := 1]
  } else {
    daily_summary[i, similarity_score := 0]
  }

  # if num workouts is the same as the average number of workouts in the previous two weeks, add 1 to similarity score
  avg_num_workouts_prev <- mean(previous_two_weeks$num_workouts, na.rm = TRUE)
  if (daily_summary[i]$num_workouts == round(avg_num_workouts_prev)) {
    daily_summary[i, similarity_score := similarity_score + 1]
  }

  # if the total duration is within 20% of the average total duration in the previous two weeks, add 1 to similarity score
  avg_total_duration_prev <- mean(previous_two_weeks$total_duration, na.rm = TRUE)  
  if (abs(daily_summary[i]$total_duration - avg_total_duration_prev) <= 0.2 * avg_total_duration_prev) {
    daily_summary[i, similarity_score := similarity_score + 1]
  }

  # if the average heart rate is within 10 bpm of the average heart rate in the previous two weeks, add 1 to similarity score
  avg_heart_rate_prev <- mean(previous_two_weeks$avg_heart_rate, na.rm = TRUE)  
  if (abs(daily_summary[i]$avg_heart_rate - avg_heart_rate_prev) <= 10) {
    daily_summary[i, similarity_score := similarity_score + 1]
  }

  # if the first heart rate time is within 1 hour of the average first heart rate time in the previous two weeks, add 1 to similarity score
  avg_first_heartrate_time_prev <- mean(as.numeric(previous_two_weeks$first_heartrate_time), na.rm = TRUE)  
  if (abs(as.numeric(daily_summary[i]$first_heartrate_time) - avg_first_heartrate_time_prev) <= 3600) {
    daily_summary[i, similarity_score := similarity_score + 1]
  } 

  # if the last heart rate time is within 1 hour of the average last heart rate time in the previous two weeks, add 1 to similarity score
  avg_last_heartrate_time_prev <- mean(as.numeric(previous_two_weeks$last_heartrate_time), na.rm = TRUE)  
  if (abs(as.numeric(daily_summary[i]$last_heartrate_time) - avg_last_heartrate_time_prev) <= 3600) {
    daily_summary[i, similarity_score := similarity_score + 1]
  }

}

print(daily_summary)

# using daily_summary, plot a calendar where the color is based on similarity score for 2025
calendR(from = "2025-01-01", # Custom start date
        to = "2025-12-31",
        special.days = daily_summary$similarity_score[daily_summary$date >= as.Date("2025-01-01") & daily_summary$date <= as.Date("2025-12-31")],
        gradient = TRUE,
        special.col = "white",
        low.col = "red",
        legend.pos = "right",     # Position of the legend
        legend.title = "Similarity Score",
        title = "Daily Similarity Score 2025")

length(daily_summary$similarity_score[daily_summary$date >= as.Date("2025-08-01") & daily_summary$date <= as.Date("2025-08-31")])

print(location_data[location_data$date >= as.Date("2025-08-01") & location_data$date <= as.Date("2025-08-31")])
print(daily_summary[daily_summary$date >= as.Date("2025-08-01") & daily_summary$date <= as.Date("2025-08-31")])