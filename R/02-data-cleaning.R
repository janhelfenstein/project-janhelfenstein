# 02-data-cleaning.R
# This script cleans and tidies the data from the google survey about waste separation,
# which we did in the RBTL course in FS25
# author: Jan Helfenstein
# date: 2025-06-02


# load packages -------------------------
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(lubridate)
library(readr)


# read data from csv -----------
survey_data <- read_csv("data/raw/survey-raw.csv")


# tidy data from survey -----------------

# step 1: rename columns
survey_renamed <- survey_raw |> 
  rename(timestamp = Timestamp) |> 
  rename(feeling = 2) |> 
  rename(waste_location = 3) |> 
  rename(waste_location_today = 4) |> 
  rename(waste_type = 5) |> 
  rename(day_frequency = 6) |> 
  rename(activities = 7) |> 
  rename(activities_today = 8) |> 
  rename(wasteful_activities = 9) |> 
  rename(sports_value = 10) |> 
  rename(sports_waste_freq = 11) |> 
  rename(sportsmen_littering = 12) |>
  rename(sportsevent_waste = 13) |> 
  rename(measures = 14) |> 
  rename(responsible = 15) |> 
  rename(removal_freq = 16) |> 
  rename(age = 17)
  rename(gender = 18)


# step 2: change dates and times, add weekday variable
survey_dates <- survey_renamed |> 
  mutate(date = as_date(timestamp)) |> 
  mutate(time = format(timestamp, "%H:%M:%S")) |> 
  mutate(weekday = wday(timestamp, label = TRUE, abb = FALSE)) |> 
  relocate(weekday) |> 
  relocate(time) |> 
  relocate(date) |> 
  select(!timestamp) # remove old timestamp variable

# step 3: shorten text values
trash_locations = c("paths", "picnicareas", "parkinglots", "deepforest", "other")
trash_types = c("paper", "plasticbottles", "cans", "foodpackaging", "cigarette", "dogwastebag", "clothing", "other")


# coerce data types ------------------------
head(survey_names)
# todo: recycler_level to always, mostly, some, never
# recycler_level and disposal_level to factor variables
levels_recycling <- c("always", "mostly", "sometimes", "never")
levels_disposal <- c("fee-bag", "non-fee-bag", "public-bin", "bushes", "toilet", "hole", "fire")

# check levels
# survey_names$disposal_level

# categorize recycler_level and disposal_level
survey_cat <- survey_names |>
  mutate(recycler_cat = case_when(recycler_level == "Yes, always" ~ "always",
                                  recycler_level == "Yes, most of the time" ~ "mostly",
                                  recycler_level == "Sometimes" ~ "sometimes",
                                  recycler_level == "no" ~ "never"
  )
  ) |> 
  mutate(disposal_cat = case_when(disposal_level == "I dispose my waste in bags where I pay the waste fee. (like the blue Züri-Sack)" ~ "fee-bag",
                                  disposal_level == "I dispose my waste in common bags without paying the fee." ~ "non-fee-bag",
                                  disposal_level == "I dispose my waste in public waste bins." ~ "public-bin",
                                  disposal_level == "I throw my waste on the ground or into the bushes." ~ "bushes",
                                  disposal_level == "I flush my waste down the toilet." ~ "toilet",
                                  disposal_level == "I dig holes to bury my waste." ~ "hole",
                                  disposal_level == "I burn my waste." ~ "fire"
  )
  ) |> 
  relocate(recycler_cat, .after = rating) |> 
  relocate(disposal_cat, .after = regular_walker) |> 
  select(!disposal_level) |> 
  select(!recycler_level)

# define PET and banana recycling levels
levels_park_recycling = c("bushes", "on-full-bin", "next-to-bin", "find-recycling", "take-home", "other")

# categorize park recycling variables
survey_cat2 <- survey_cat |> 
  mutate(pet_cat = case_when(pet_recycler == "I try to somehow balance my bottle on top of the full bin." ~ "on-full-bin",
                             pet_recycler == "I put my bottle on the ground next to the full bin." ~ "next-to-bin",
                             pet_recycler == "I walk around to find the next recycling container for PET bottles." ~ "find-recycling",
                             pet_recycler == "I take my bottle home with me." ~ "take-home",
                             pet_recycler == "I throw my bottle onto the ground or in the bushes." ~ "bushes",
                             .default = "other"
  )
  ) |> 
  mutate(banana_cat = case_when(banana_recycler == "I try to somehow balance the banana peel on top of the full bin." ~ "on-full-bin",
                                banana_recycler == "I put my banana peel on the ground next to the full bin." ~ "next-to-bin",
                                banana_recycler == "I walk around to find the next recycling container for organic waste." ~ "find-recycling",
                                banana_recycler == "I take my banana peel home with me." ~ "take-home",
                                banana_recycler == "I throw the peel on the ground or into the bushes." ~ "bushes",
                                .default = "other"
  )
  ) |> 
  select(!pet_recycler) |> 
  select(!banana_recycler)

# order recycler_cat, disposal_cat, pet_recycler, banana_recycler with factor variables
survey_order <- survey_cat2 |> 
  mutate(recycler_cat = factor(recycler_cat, levels = levels_recycling)) |> 
  mutate(disposal_cat = factor(disposal_cat, levels = levels_disposal)) |> 
  mutate(pet_cat = factor(pet_cat, levels = levels_park_recycling)) |> 
  mutate(banana_cat = factor(banana_cat, levels = levels_park_recycling))

# convert walkable_station and regular_walker into boolean variables
survey_logic <- survey_order |> 
  mutate(walkable_station = case_when(walkable_station == "Yes" ~ TRUE,
                                      walkable_station == "No" ~ FALSE,
                                      .default = FALSE
  )
  
  ) |> 
  mutate(regular_walker = case_when(regular_walker == "Yes" ~ TRUE,
                                    regular_walker == "No" ~ FALSE,
                                    .default = FALSE
  )
  )

survey_processed <- survey_logic

# TO DO ---------------
# write code to separate data in materials_recycled. 
# Find out how to separate answers that were selected in multiple choice questions. 

# write csv and rds in processed
write_csv(survey_processed, "data/processed/survey-data-processed.csv")
write_rds(survey_processed, "data/processed/survey-data-processed.rds")
