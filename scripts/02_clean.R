# --------------------------------------------------
# Bellabeat Time – Phase II: Clean data
# Purpose: Clean, mutate, and standardize Fitbit proxy data
# --------------------------------------------------

library(tidyverse)  # includes dplyr, readr, ggplot2, magrittr (%>%)
library(janitor)    # used for cleaning files
library(lubridate)  # used for formatting dates & times

source("scripts/01_import.R")

# --------------------------------------------------
# Clean and standardize variable names in the main data table to be used for this analysis
# Standardize dates to display date_time as YYYY-MM-DD and 24-hour time (instead of AM/PM)
# Display short data sample from old & new files for visual comparison before saving file
#   since column names might not convert optimally (e.g., METs column converted to "me_ts" instead of "mets")
# --------------------------------------------------

CHECK_DATA <- TRUE

daily_activity_clean <- daily_activity %>%
  clean_names() %>%
  mutate(activity_date = mdy(activity_date))

if (CHECK_DATA) {
  head(daily_activity, show_col_types = FALSE)
  head(daily_activity_clean, show_col_types = FALSE)
}

write_csv(
  daily_activity_clean,
  "data/processed/daily_activity_clean.csv"
)
# Note: CSV read-back verified for initial table; omitted for subsequent tables
read_csv("data/processed/daily_activity_clean.csv")

# next table & visual verification
heartrate_seconds_clean <- heartrate_seconds %>%
  clean_names() %>%
  mutate(time = mdy_hms(time))

if (CHECK_DATA) {
  head(heartrate_seconds, show_col_types = FALSE)
  head(heartrate_seconds_clean, show_col_types = FALSE)
}

write_csv(
  heartrate_seconds_clean,
  "data/processed/heartrate_seconds_clean.csv"
)

# next table & visual verification
hourly_calories_clean <- hourly_calories %>%
  clean_names() %>%
  mutate(activity_hour = mdy_hms(activity_hour))

if (CHECK_DATA) {
  head(hourly_calories, show_col_types = FALSE)
  head(hourly_calories_clean, show_col_types = FALSE)
}

write_csv(
  hourly_calories_clean,
  "data/processed/hourly_calories_clean.csv"
)

# next table conversion & visual verification
hourly_steps_clean <- hourly_steps %>%
  clean_names() %>%
  mutate(activity_hour = mdy_hms(activity_hour))

if (CHECK_DATA) {
  head(hourly_steps, show_col_types = FALSE)
  head(hourly_steps_clean, show_col_types = FALSE)
}

write_csv(
  hourly_steps_clean,
  "data/processed/hourly_steps_clean.csv"
)

# next table conversion & visual verification
hourly_intensity_clean <- hourly_intensity %>%
  clean_names() %>%
  mutate(activity_hour = mdy_hms(activity_hour))

if (CHECK_DATA) {
  head(hourly_intensity, show_col_types = FALSE)
  head(hourly_intensity_clean, show_col_types = FALSE)
}

write_csv(
  hourly_intensity_clean,
  "data/processed/hourly_intensity_clean.csv"
)

# next table conversion & visual verification
minute_calories_narrow_clean <- minute_calories_narrow %>%
  clean_names() %>%
  mutate(activity_minute = mdy_hms(activity_minute))

if (CHECK_DATA) {
  head(minute_calories_narrow, show_col_types = FALSE)
  head(minute_calories_narrow_clean, show_col_types = FALSE)
}

write_csv(
  minute_calories_narrow_clean,
  "data/processed/minute_calories_narrow_clean.csv"
)

# next table conversion & visual verification
minute_intensity_narrow_clean <- minute_intensity_narrow %>%
  clean_names() %>%
  mutate(activity_minute = mdy_hms(activity_minute))

if (CHECK_DATA) {
  head(minute_intensity_narrow, show_col_types = FALSE)
  head(minute_intensity_narrow_clean, show_col_types = FALSE)
}

write_csv(
  minute_intensity_narrow_clean,
  "data/processed/minute_intensity_narrow_clean.csv"
)

# next table conversion & visual verification + rename METS column to desired lowercase without the underscore
minute_mets_narrow_clean <- minute_mets_narrow %>%
  clean_names() %>%
  rename(mets = me_ts) %>%
  mutate(activity_minute = mdy_hms(activity_minute))

if (CHECK_DATA) {
  head(minute_mets_narrow, show_col_types = FALSE)
  head(minute_mets_narrow_clean, show_col_types = FALSE)
}

write_csv(
  minute_mets_narrow_clean,
  "data/processed/minute_mets_narrow_clean.csv"
)

# --------------------------------------------------
# Clean minute-level sleep data
# Used for sleep episodes and nap analysis
# --------------------------------------------------

minute_sleep_clean <- minute_sleep %>%
  clean_names() %>%
  mutate(
    date_time = mdy_hms(date)
  ) %>%
  select(
    id,
    date_time,
    log_id,
    value
  )

# quick verification
if (CHECK_DATA) {
  head(minute_sleep_clean)
}

write_csv(
  minute_sleep_clean,
  "data/processed/minute_sleep_clean.csv"
)

# --------------------------------------------------
# Create daily sleep summary from minute-level data
# One row per user per sleep date
# --------------------------------------------------

daily_sleep_clean <- minute_sleep_clean %>%
  filter(value == 1) %>%                          # asleep only
  mutate(sleep_date = as_date(date_time)) %>%
  group_by(id, sleep_date) %>%
  summarize(
    sleep_minutes = n(),
    sleep_hours = sleep_minutes / 60,
    .groups = "drop"
  )

# quick verification
if (CHECK_DATA) {
  head(daily_sleep_clean)
}

write_csv(
  daily_sleep_clean,
  "data/processed/daily_sleep_clean.csv"
)

# next table conversion & visual verification
minute_sleep_clean <- minute_sleep %>%
  clean_names() %>%
  mutate(date_time = mdy_hms(date))

if (CHECK_DATA) {
  head(minute_sleep, show_col_types = FALSE)
  head(minute_sleep_clean, show_col_types = FALSE)
}

write_csv(
  minute_sleep_clean,
  "data/processed/minute_sleep_clean.csv"
)

# next table conversion & visual verification
minute_steps_narrow_clean <- minute_steps_narrow %>%
  clean_names() %>%
  mutate(activity_minute = mdy_hms(activity_minute))

if (CHECK_DATA) {
  head(minute_steps_narrow, show_col_types = FALSE)
  head(minute_steps_narrow_clean, show_col_types = FALSE)
}

write_csv(
  minute_steps_narrow_clean,
  "data/processed/minute_steps_narrow_clean.csv"
)

# next table conversion & visual verification
weight_log_info_clean <- weight_log_info %>%
  clean_names() %>%
  mutate(date_time = mdy_hms(date))

if (CHECK_DATA) {
  head(weight_log_info, show_col_types = FALSE)
  head(weight_log_info_clean, show_col_types = FALSE)
}

write_csv(
  weight_log_info_clean,
  "data/processed/weight_log_info_clean.csv"
)

# --------------------------------------------------
# Initial data integrity checks
# --------------------------------------------------

n_distinct(daily_activity_clean$id)
n_distinct(heartrate_seconds_clean$id)
n_distinct(hourly_calories_clean$id)
n_distinct(hourly_intensity_clean$id)
n_distinct(hourly_steps_clean$id)
n_distinct(minute_calories_narrow_clean$id)
n_distinct(minute_intensity_narrow_clean$id)
n_distinct(minute_mets_narrow_clean$id)
n_distinct(minute_sleep_clean$id)
n_distinct(minute_steps_narrow_clean$id)
n_distinct(weight_log_info_clean$id)

# --------------------------------------------------
# END Clean Data - NEXT STEPS in ~project/scripts/03_scripts.R
# --------------------------------------------------
