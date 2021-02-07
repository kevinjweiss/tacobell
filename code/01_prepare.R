# The purpose of this script is to prepare the taco bell data by creating new
# descriptive features and casting as a complete time series.


# SETUP -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# READ DATA ---------------------------------------------------------------

df_tbell_raw <- read_csv(here("data", "raw", "rawdata.csv"))


# PREPARE DATA ------------------------------------------------------------

# Select and rename primary data fields
df_tbell_trans <- df_tbell_raw %>%
  select(
    date = Date,
    description = `Original Description`,
    amount = Amount
  ) %>%
  mutate(
    date = mdy(date)
  )

# Exract information from description
df_tbell_trans <- df_tbell_trans %>%
  mutate(
    description = description %>% str_remove_all("\\\\n") %>% str_trim(),
    state = description %>% str_extract("[:upper:]{2}$"),
    store_no = description %>% str_extract("[:digit:]{5,6}") %>% str_sub(start = -5)
  ) %>%
  select(-description)

# Write data to disk
write_csv(df_tbell_trans, here("data", "processed", "df_tbell_trans.csv"))
write_rds(df_tbell_trans, here("data", "processed", "df_tbell_trans.rds"))
