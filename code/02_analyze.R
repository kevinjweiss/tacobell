# The purpose of this script is to analyze the taco bell spending and create
# intriguing data visualizations.

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(gganimate)

# Parameters
ls_ftr <- list(
  date_start = ymd("2019-01-01"),
  date_end = ymd("2020-12-31")
)

# Set color pallette
pal <- c("#535C8D", "#36827F", "#DE9151", "#9A031E", "#E0E0E0")

#weekend = ifelse(weekday_lab %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# READ DATA ---------------------------------------------------------------

df_tbell_trans <- read_rds(here("data", "processed", "df_tbell_trans.rds"))


# CREATE SKELETONS --------------------------------------------------------

# Create a date skeleton
df_skel_date <- df_tbell_trans %>%
  expand(
    date = seq.Date(ls_ftr$date_start, ls_ftr$date_end, by = "1 day")
  )

# Create skeleton of dates and stores
df_skel_store <- df_tbell_trans %>%
  expand(
    date = seq.Date(ls_ftr$date_start, ls_ftr$date_end, by = "1 day"),
    store_no
  )


# CUMULATIVE TREND (2020) -------------------------------------------------

# Create a cumulative plot of tbell spend
p_cum <- df_skel_date %>%
  left_join(df_tbell_trans, by = "date") %>%
  arrange(date) %>%
  replace_na(list(amount = 0)) %>%
  group_by(date) %>%
  summarize(amount_daily = sum(amount)) %>%
  ungroup() %>%
  mutate(
    amount_cum = cumsum(amount_daily)
  ) %>%
  ggplot() +
  geom_line(
    aes(x = date, y = amount_cum),
    color = "grey10", alpha = 0.7, size = 1.25
  ) +
  # geom_point(
  #   aes(x = date, y = amount_cum)
  # ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b, %Y"
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Cumulative Taco Bell Spending",
    x = "Date",
    y = "Cumulative Spending"
  )

p_cum

# Define animination
a_cum <- p_cum +
  transition_reveal(date) +
  view_follow(fixed_y = TRUE)

# Render animation
animate(
  plot = a_cum, 
  fps = 10, duration = 15, 
  height = 300, width = 600,
  ref_frame = -1, start_pause = 5,
  renderer = gifski_renderer()
)

# Save animination
anim_save("cumulative.gif", path = here("out"))


# CUMULATIVE TREND (2020) - BY LOCATION -----------------------------------

# Create a cumulative plot of tbell spend
p_cum <- df_skel_store %>%
  left_join(df_tbell_trans, by = c("date", "store_no")) %>%
  arrange(date) %>%
  replace_na(list(amount = 0)) %>%
  mutate(
    #store_no = fct_lump(store_no, n = 3, w = amount),
    store_no = fct_reorder(store_no, amount, .fun = sum, .desc = TRUE)
  ) %>%
  group_by(date, store_no) %>%
  summarize(amount_daily = sum(amount)) %>%
  ungroup() %>%
  group_by(store_no) %>%
  mutate(
    amount_cum = cumsum(amount_daily)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = amount_cum, color = store_no)) +
  geom_line(alpha = 0.7) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b, %Y"
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Cumulative Taco Bell Spending",
    x = "Date",
    y = "Taco Spend",
    color = "Store Number"
  )

# Save plot
ggsave(
  here("out", "cumulative_by_location.jpg"),
  height = 5, width = 8, dpi = 1000
)

# WEEKDAY TREND - WITH LOCATION -------------------------------------------

# Generate plot
p_wday <- df_tbell_trans %>%
  filter(
    date >= ls_ftr$date_start,
    date <= ls_ftr$date_end
  ) %>%
  mutate(
    weekday = wday(date, label = T, abbr = T),
    store_no = fct_lump(store_no,n = 5, w = amount),
    store_no = fct_reorder(store_no, amount, .fun = sum, .desc = TRUE)
  ) %>%
  ggplot(aes(x = weekday, y = amount, fill = store_no)) +
  geom_col(alpha = 0.7) +
  theme_light() +
  labs(
    title = "Spending by Day of Week",
    x = "Day of Week",
    y = "Total Spending",
    fill = "Store Number",
    caption = "Data for 2019 and 2020"
  )


# Save plot
ggsave(
  here("out", "weekday_trend.jpg"),
  height = 5, width = 8, dpi = 1000
)

