library(XML)
library(data.table)
library(lubridate)
library(ggplot2)
library(stringr)
library(leaflet)

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

library(calendR)

calendR(from = "2025-01-01", # Custom start date
        to = "2025-12-31",
        special.days = handwash_daily$numHandwashes[handwash_daily$date >= as.Date("2025-01-01") & handwash_daily$date <= as.Date("2025-12-31")],
        gradient = TRUE,
        special.col = "blue",
        legend.pos = "right",     # Position of the legend
        legend.title = "Legend",
        title = "Handwashing Events 2025")
