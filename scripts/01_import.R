# --------------------------------------------------
# Bellabeat Time – Phase I: Prepare (import data)
# Purpose: Load and prepare raw Fitbit proxy data
# --------------------------------------------------

library(tidyverse)      # includes dplyr and readr

# --------------------------------------------------
# Data folder structure is created and raw data files were uploaded to ~project/data/raw
# --------------------------------------------------

getwd()      # confirm project root

# --------------------------------------------------
# Data folder structure was created and raw data files uploaded manually to ~project/data/raw
# Import the raw data files into clean file names
# --------------------------------------------------

daily_activity <- read_csv("data/raw/dailyActivity_merged.csv")
heartrate_seconds <- read_csv("data/raw/heartrate_seconds_merged.csv")
hourly_calories <- read_csv("data/raw/hourlyCalories_merged.csv")
hourly_intensity <- read_csv("data/raw/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("data/raw/hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("data/raw/minuteCaloriesNarrow_merged.csv")
minute_intensity_narrow <- read_csv("data/raw/minuteIntensitiesNarrow_merged.csv")
minute_mets_narrow <- read_csv("data/raw/minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("data/raw/minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("data/raw/minuteStepsNarrow_merged.csv")
weight_log_info <- read_csv("data/raw/weightLogInfo_merged.csv")

# --------------------------------------------------
# END Import Data - NEXT STEPS in ~project/scripts/02_clean.R
# --------------------------------------------------
