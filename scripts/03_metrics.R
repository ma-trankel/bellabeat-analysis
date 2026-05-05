# --------------------------------------------------
# Bellabeat Time – Phase III: Metrics
# Purpose: Generate appropriate statistics and graphic visualizations
# --------------------------------------------------

library(tidyverse)  # includes dplyr, readr, ggplot2, magrittr (%>%)
library(lubridate)
library(janitor)
        
theme_bellabeat <- function(base_size = 11) {
  theme(
    plot.title = element_text(
      face = "bold",
      color = "#165081"
    ),
    plot.subtitle = element_text(
      face = "bold.italic",
      color = "#2171B5"
    ),
    axis.title.x = element_text(
      face = "bold",
      color = "#2171B5"
    ),
    axis.title.y = element_text(
      face = "bold",
      color = "#2171B5"
    ),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )
}

# --------------------------------------------------
# Read cleaned input data
# --------------------------------------------------
# Note: Sleep is attributed to calendar dates; overnight sleep spanning midnight
# is distributed by minute rather than assigned to a single "sleep night."
# --------------------------------------------------

hourly_steps_clean <- read_csv(
  "data/processed/hourly_steps_clean.csv"
)

hourly_intensity_clean <- read_csv(
  "data/processed/hourly_intensity_clean.csv"
)

hourly_calories_clean <- read_csv(
  "data/processed/hourly_calories_clean.csv"
)

daily_sleep_clean <- read_csv(
  "data/processed/daily_sleep_clean.csv"
)

daily_activity_clean <- read_csv(
  "data/processed/daily_activity_clean.csv"
)

minute_sleep_clean <- read_csv(
  "data/processed/minute_sleep_clean.csv"
)

# --------------------------------------------------
# Join 2 tables and create new columns to categorize Time of Day behaviors
# This transforms raw sensor data into a behavioral context 
# --------------------------------------------------

names(hourly_steps_clean)
names(hourly_intensity_clean)
names(hourly_calories_clean)

hourly_activity_clean <- hourly_steps_clean %>%
  left_join(hourly_intensity_clean, by = c("id", "activity_hour")) %>%
  left_join(hourly_calories_clean, by = c("id", "activity_hour")) %>%
  mutate(
    hour = hour(activity_hour),
    day_of_week = wday(activity_hour, label = TRUE),
    time_of_day = case_when(
      hour >= 6  & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 22 ~ "Evening",
      TRUE                  ~ "Night"
    )
  )

write_csv(
  hourly_activity_clean,
  "data/processed/hourly_activity_clean.csv"
)

# --------------------------------------------------
# Average steps by hour of day
# Generate plot of data
# --------------------------------------------------

hourly_steps_by_hour_clean <- hourly_activity_clean %>%
  group_by(hour) %>%
  summarize(
    avg_steps    = mean(step_total, na.rm = TRUE),
    median_steps = median(step_total, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  hourly_steps_by_hour_clean,
  "data/processed/hourly_steps_by_hour_clean.csv"
)

# STORE the plot
hourly_steps_by_hour_plot <- ggplot(hourly_steps_by_hour_clean) +
  geom_line(
    aes(x = hour, y = avg_steps, color = "Mean"),
    linewidth = 1
  ) +
  geom_line(
    aes(x = hour, y = median_steps, color = "Median"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_point(
    aes(x = hour, y = avg_steps, color = "Mean")
  ) +
  geom_point(
    aes(x = hour, y = median_steps, color = "Median")
  ) +
  scale_x_continuous(breaks = 0:23) +
  scale_color_manual(
    values = c("Mean" = "#2C7FB8", "Median" = "black")
  ) +
  labs(
    title = "Hourly Step Patterns Across the Day",
    subtitle = "Median reveals typical behavior masked by high-activity outliers",
    x = "Hour of Day",
    y = "Steps",
    color = "Statistic"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/hourly_steps_by_hour.png",
  plot = hourly_steps_by_hour_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Average intensity by time of day
# --------------------------------------------------

intensity_by_time_of_day_clean <- hourly_activity_clean %>%
  group_by(time_of_day) %>%
  summarize(
    avg_intensity = mean(average_intensity, na.rm = TRUE),
    median_intensity = median(average_intensity, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  intensity_by_time_of_day_clean,
  "data/processed/intensity_by_time_of_day_clean.csv"
)

# STORE the plot
intensity_by_tod_plot <- ggplot(intensity_by_time_of_day_clean, aes(x = time_of_day)) +
  geom_col(
    aes(y = avg_intensity, fill = "Mean"),
    show.legend = TRUE
  ) +
  geom_point(
    aes(y = median_intensity, color = "Median"),
    size = 3
  ) +
  geom_line(
    aes(y = median_intensity, group = 1, color = "Median"),
    linewidth = 0.8
  ) +
  scale_fill_manual(
    values = c("Mean" = "#9ECAE1")
  ) +
  scale_color_manual(
    values = c("Median" = "black")
  ) +
  labs(
    title = "Activity Intensity by Time of Day",
    subtitle = "Mean shown by bars; median highlighted to reveal skewness",
    x = "Time of Day",
    y = "Activity Intensity",
    fill = "Statistic",
    color = "Statistic"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/intensity_by_tod.png",
  plot = intensity_by_tod_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Weekday vs. weekend hourly step patterns
# --------------------------------------------------

hourly_activity_clean <- hourly_activity_clean %>%
  mutate(
    day_type = if_else(day_of_week %in% c("Sat", "Sun"),
                       "Weekend", "Weekday")
  )

steps_weekday_weekend_clean <- hourly_activity_clean %>%
  group_by(hour, day_type) %>%
  summarize(
    avg_steps    = mean(step_total, na.rm = TRUE),
    median_steps = median(step_total, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  steps_weekday_weekend_clean,
  "data/processed/steps_weekday_weekend_clean.csv"
)

# STORE the plot
steps_weekday_weekend_plot <- ggplot(steps_weekday_weekend_clean, aes(x = hour)) +
  geom_line(
    aes(y = avg_steps, color = day_type),
    linewidth = 1
  ) +
  geom_line(
    aes(y = median_steps, color = day_type),
    linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Steps: Weekday vs Weekend",
    subtitle = "Solid lines show means; dashed lines show typical behavior",
    x = "Hour of Day",
    y = "Steps",
    color = "Day Type"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/steps_weekday_weekend.png",
  plot = steps_weekday_weekend_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Distribution of daily steps
# --------------------------------------------------

# STORE the plot
daily_activity_distribution_plot <- ggplot(daily_activity_clean, aes(x = total_steps)) +
  geom_histogram(
    bins = 40,
    fill = "#41AB5D",
    color = "white"
  ) +
  labs(
    title = "Distribution of Daily Steps",
    subtitle = "Most days show moderate, sustainable activity levels",
    x = "Total Daily Steps",
    y = "Number of Days"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/daily_activity_distribution.png",
  plot = daily_activity_distribution_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# User-level behavioral summary table
# --------------------------------------------------

# Daily activity summary per user
user_daily_summary_clean <- daily_activity_clean %>%
  group_by(id) %>%
  summarize(
    avg_daily_steps = mean(total_steps, na.rm = TRUE),
    sd_daily_steps  = sd(total_steps, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  user_daily_summary_clean,
  "data/processed/user_daily_summary_clean.csv"
)

# Hourly engagement per user
user_hourly_summary_clean <- hourly_activity_clean %>%
  group_by(id) %>%
  summarize(
    avg_hourly_steps = mean(step_total, na.rm = TRUE),
    avg_intensity    = mean(average_intensity, na.rm = TRUE),
    active_hours_per_day = n_distinct(activity_hour) / n_distinct(as_date(activity_hour)),
    .groups = "drop"
  )

write_csv(
  user_hourly_summary_clean,
  "data/processed/user_hourly_summary_clean.csv"
)

# --------------------------------------------------
# Evening activity share (routine indicator)
# --------------------------------------------------
user_evening_behavior_clean <- hourly_activity_clean %>%
  mutate(is_evening = time_of_day == "Evening") %>%
  group_by(id) %>%
  summarize(
    evening_activity_ratio = mean(is_evening, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  user_evening_behavior_clean,
  "data/processed/user_evening_behavior_clean.csv"
)

# --------------------------------------------------
# Sleep behavior per user
# --------------------------------------------------
user_sleep_summary_clean <- daily_sleep_clean %>%
  group_by(id) %>%
  summarize(
    avg_sleep_hours = mean(sleep_hours, na.rm = TRUE),
    nights_tracked  = n(),
    .groups = "drop"
  )

write_csv(
  user_sleep_summary_clean,
  "data/processed/user_sleep_summary_clean.csv"
)

# --------------------------------------------------
# Combine all user-level features
# --------------------------------------------------
user_behavior_clean <- user_daily_summary_clean %>%
  left_join(user_hourly_summary_clean, by = "id") %>%
  left_join(user_evening_behavior_clean, by = "id") %>%
  left_join(user_sleep_summary_clean, by = "id")

write_csv(
  user_behavior_clean,
  "data/processed/user_behavior_clean.csv"
)

# --------------------------------------------------
# Daily activity vs sleep relationship
# --------------------------------------------------

# STORE the plot
user_activity_vs_sleep_plot <- ggplot(user_behavior_clean,
       aes(x = avg_sleep_hours, y = avg_daily_steps)) +
  geom_point(alpha = 0.6, color = "#2C7FB8") +
  labs(
    title = "Average Daily Steps vs Average Sleep Duration",
    subtitle = "Most users balance moderate activity with adequate sleep",
    x = "Average Sleep (Hours)",
    y = "Average Daily Steps"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/user_activity_vs_sleep.png",
  plot = user_activity_vs_sleep_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Activity consistency vs daily engagement
# --------------------------------------------------

# STORE the plot
activity_intensity_plot <- ggplot(user_behavior_clean,
       aes(x = avg_intensity, y = active_hours_per_day)) +
  geom_point(alpha = 0.6, color = "#41AB5D") +
  labs(
    title = "Activity Intensity vs Daily Engagement Span",
    subtitle = "Higher engagement does not require high intensity",
    x = "Average Intensity",
    y = "Active Hours per Day"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/activity_intensity.png",
  plot = activity_intensity_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Distribution of evening activity preference
# --------------------------------------------------

# STORE the plot
evening_activity_preference_plot <- ggplot(user_behavior_clean,
       aes(x = evening_activity_ratio)) +
  geom_histogram(bins = 30, fill = "#756BB1", color = "white") +
  labs(
    title = "Evening Activity Preference Among Users",
    subtitle = "Some users concentrate activity in the evening",
    x = "Proportion of Activity Occurring in the Evening",
    y = "Number of Users"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/evening_activity_preference.png",
  plot = evening_activity_preference_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Create behavior-based segments
# --------------------------------------------------

activity_by_engagement_segment <- user_behavior_clean %>%
  mutate(
    activity_segment = case_when(
      avg_daily_steps < 5000  ~ "Low Engagement",
      avg_daily_steps < 10000 ~ "Moderate Engagement",
      TRUE                    ~ "High Engagement"
    ),
    routine_segment = case_when(
      evening_activity_ratio > 0.5 ~ "Evening-Focused",
      active_hours_per_day >= 10   ~ "All-Day Wear",
      TRUE                         ~ "Routine-Based"
    )
  )

write_csv(
  activity_by_engagement_segment,
  "data/processed/activity_by_engagement_segment_clean.csv"
)

# STORE the plot
activity_by_engagement_segment_plot <- ggplot(
  activity_by_engagement_segment,
  aes(x = activity_segment, y = avg_daily_steps, fill = activity_segment)
) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Daily Activity Levels by Engagement Segment",
    subtitle = "Clear behavioral groupings reveal marketing-relevant user segments",
    x = "Engagement Segment",
    y = "Average Daily Steps"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/activity_by_engagement_segment.png",
  plot = activity_by_engagement_segment_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Generate descriptive statistics based on Time of Day categories
# This answers: "When does Bellabeat Time deliver the most value to users?"
# Statistics are saved to a separate summary table.
# --------------------------------------------------

tod_summary_clean <- hourly_activity_clean %>%
  group_by(time_of_day) %>%
  summarize(
    mean_steps     = mean(step_total, na.rm = TRUE),
    median_steps   = median(step_total, na.rm = TRUE),
    mean_intensity = mean(average_intensity, na.rm = TRUE),
    median_intensity = median(average_intensity, na.rm = TRUE),
    mean_calories  = mean(calories, na.rm = TRUE),
    median_calories = median(calories, na.rm = TRUE),
    .groups = "drop"
  )

tod_summary_clean

write_csv(
  tod_summary_clean,
  "data/processed/tod_summary_clean.csv"
)

# --------------------------------------------------
# Sleep duration data summarized
# Minutes are converted to hours to make insights intuitive 
# Data is saved to a separate table to preserve raw data in original table
# --------------------------------------------------

sleep_stats_clean <- daily_sleep_clean %>%
  summarize(
    mean_sleep_hours   = mean(sleep_hours, na.rm = TRUE),
    median_sleep_hours = median(sleep_hours, na.rm = TRUE),
    min_sleep_hours    = min(sleep_hours, na.rm = TRUE),
    max_sleep_hours    = max(sleep_hours, na.rm = TRUE),
    nights_observed    = n()
  )

sleep_stats_clean

write_csv(
  sleep_stats_clean,
  "data/processed/sleep_stats_clean.csv"
)

# --------------------------------------------------
# Summary table of "nap" data created by computing the difference between sleep_start and sleep_end
# Naps are saved in a separate Nap Summary table in the variable minutes_asleep  
# --------------------------------------------------

nap_summary_clean <- minute_sleep_clean %>%
  filter(value == 1) %>%                       # asleep
  group_by(id, log_id) %>%
  summarise(
    sleep_start = as_date(min(date_time)),
    sleep_end   = as_date(max(date_time)),
    minutes_asleep = as.numeric(
      difftime(max(date_time), min(date_time), units = "mins")
    ),
    .groups = "drop"
  ) %>%
  filter(sleep_start == sleep_end) %>%          # naps only
  group_by(id, sleep_start) %>%
  summarize(
    number_naps = n(),
    total_minutes_napping = sum(minutes_asleep),
    .groups = "drop"
  )

nap_summary_clean

write_csv(
  nap_summary_clean,
  "data/processed/nap_summary_clean.csv"
)

# --------------------------------------------------
# Nap behavior summary
# --------------------------------------------------

nap_stats_clean <- nap_summary_clean %>%
  summarize(
    avg_naps_per_day = mean(number_naps, na.rm = TRUE),
    median_naps_per_day = median(number_naps, na.rm = TRUE),
    avg_minutes_napping = mean(total_minutes_napping, na.rm = TRUE)
  )

nap_stats_clean

write_csv(
  nap_stats_clean,
  "data/processed/nap_stats_clean.csv"
)

# --------------------------------------------------
# Distribution of sleep duration
# --------------------------------------------------

# STORE the plot
distribution_sleep_duration_plot <- ggplot(daily_sleep_clean, aes(x = sleep_hours)) +
  geom_histogram(
    bins = 30,
    fill = "#756BB1",
    color = "white"
  ) +
  labs(
    title = "Distribution of Sleep Duration",
    subtitle = "Sleep tracking suggests overnight device wear",
    x = "Sleep Duration (Hours)",
    y = "Number of Nights"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/distribution_sleep_duration.png",
  plot = distribution_sleep_duration_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Variation in sleep duration
# --------------------------------------------------

# STORE the plot
variation_sleep_duration_plot <- ggplot(daily_sleep_clean, aes(y = sleep_hours)) +
  geom_boxplot(
    fill = "#756BB1",
    alpha = 0.6,
    outlier.color = "black"
  ) +
  labs(
    title = "Variation in Sleep Duration",
    subtitle = "Variation in sleep duration supports median-based summaries",
    y = "Sleep Duration (Hours)",
    x = NULL
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/variation_sleep_duration.png",
  plot = variation_sleep_duration_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Distribution of daily steps
# --------------------------------------------------

# STORE the plot
distribution_daily_steps_plot <- ggplot(daily_activity_clean, aes(x = total_steps)) +
  geom_histogram(
    bins = 40,
    fill = "#9ECAE1",
    color = "white"
  ) +
  labs(
    title = "Distribution of Daily Steps",
    subtitle = "Skewed distribution supports use of median-based summaries",
    x = "Total Daily Steps",
    y = "Number of Days"
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/distribution_daily_steps.png",
  plot = distribution_daily_steps_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# Sleep duration patterns
# --------------------------------------------------

# STORE the plot
sleep_duration_patterns_plot <- ggplot(daily_sleep_clean, aes(x = 1, y = sleep_hours)) +
  geom_boxplot(
    fill = "#756BB1",
    alpha = 0.6,
    outlier.color = "black"
  ) +
  geom_point(
    position = position_jitter(width = 0.05),
    alpha = 0.3,
    color = "#756BB1"
  ) +
  labs(
    title = "Sleep Duration Patterns Across Tracked Nights",
    subtitle = "Most sleep episodes reflect overnight wear, with some shorter-duration rest periods",
    y = "Sleep Duration (Hours)",
    x = NULL
  ) + theme_bellabeat(base_size = 11)

# SAVE the plot
ggsave(
  filename = "output/sleep_duration_patterns.png",
  plot = sleep_duration_patterns_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# --------------------------------------------------
# END generation of metrics and visualizations.
# Project report is contained in project markup notebooks/bellabeat_analysis.Rmd
# --------------------------------------------------
