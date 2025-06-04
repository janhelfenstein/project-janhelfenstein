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
survey_raw <- read_csv("data/raw/survey-raw.csv")

# tidy data from survey -----------------

# step 1: rename columns
survey_renamed <- survey_raw |> 
  rename(timestamp = Timestamp) |> 
  rename(waste_feeling = 2) |> 
  rename(waste_rating = 3) |> 
  rename(waste_location = 4) |> 
  rename(waste_seen_today = 5) |> 
  rename(waste_location_today = 6) |> 
  rename(waste_type_today = 7) |> 
  rename(activities_frequency = 8) |> 
  rename(activities = 9) |> 
  rename(activities_today = 10) |> 
  rename(activities_wasteful = 11) |> 
  rename(sports_value = 12) |> 
  rename(sports_waste_frequency = 13) |> 
  rename(sports_littering = 14) |>
  rename(sports_event_waste = 15) |> 
  rename(measures = 16) |> 
  rename(measures_responsible = 17) |> 
  rename(measures_frequency = 18) |> 
  rename(age = 19) |> 
  rename(gender = 20)


# step 2: change dates and times, add weekday variable
survey_dates <- survey_renamed |> 
  mutate(date = as_date(timestamp)) |> 
  mutate(time = format(timestamp, "%H:%M:%S")) |> 
  mutate(weekday = wday(timestamp, label = TRUE, abb = FALSE)) |> 
  relocate(weekday) |> 
  relocate(time) |> 
  relocate(date) |> 
  select(!timestamp) # remove old timestamp variable

# step 3: add a column "id" and reorder
survey_id <- survey_dates |> 
  mutate(id = row_number()) |> 
  relocate(id) # move to the front

# reorder: demographic data to the front, waste info of today to the back
survey_id <- survey_id |> 
  relocate(gender, .after = weekday) |> 
  relocate(age, .after = gender) |> 
  relocate(waste_seen_today, .after = measures_frequency) |>  # move to last column 
  relocate(waste_location_today, .after = waste_seen_today) |> 
  relocate(waste_type_today, .after = waste_location_today) 
  

# step 4: lowercase letters ----------------
# replace "I'm not sure" answers with "unsure" to have fewer spaces
# change answers to lower case letters for consistency
survey_shorter1 <- survey_id |> 
  mutate(sports_event_waste = case_when(sports_event_waste == "I'm not sure" ~ "unsure",
                                        sports_event_waste == "Yes" ~ "yes",
                                        sports_event_waste == "No" ~ "no",
                                        .default = "unsure")
        )

survey_shorter1 <- survey_shorter1 |> 
  mutate(sports_littering = case_when(sports_littering == "I'm not sure" ~ "unsure",
                                      sports_littering == "Yes" ~ "yes",
                                      sports_littering == "No" ~ "no",
                                      .default = "unsure")
        )

survey_shorter1 <- survey_shorter1 |> 
  mutate(waste_seen_today = case_when(waste_seen_today == "Yes" ~ "yes",
                                      waste_seen_today == "No" ~ "no")
         )

# do the same to change weekday and gender variables to lowercase,
# e.g. from "Sunday" to "sunday" and  from "Male" to male"
# to avoid typing too much I asked ChatGPT, see prompts here:
# https://chatgpt.com/share/68406f4e-4628-8011-a1cd-6a9b0d268322

survey_shorter1 <- survey_shorter1 |> 
  mutate(gender = str_to_lower(gender)) |> 
  mutate(weekday = str_to_lower(weekday))

# step 5: separate and shorten text values -------------------
# create lists with shorter texts
trash_locations = c("picnicareas", "paths", "parkinglots", "deepforest", "other")
trash_types = c("plasticbottles", "cigarettes", "paper", "cans", "foodpackaging", "dogwastebag", "clothing", "horseshit", "other")
activity = c("walking", "biking", "picnic", "photography", "camping", "gathering", "horseriding", "birdwatching", "other")
measure = c("bins", "fines", "authority", "cleanupevent", "volunteers", "signs")
responsible = c("litterer", "authorities", "volunteers", "me", "nobody")

# separate multiple text values stored in one cell, coming from to multiple choice questions
# then shorten the text values to single words
# do it for waste_location
survey_shorter2 <- survey_shorter1 |> 
  separate_longer_delim(waste_location, ", ") |> 
  mutate(waste_location = case_when(waste_location == "Along paths or trails" ~ "paths",
                                    waste_location == "Around picnic areas or benches" ~ "picnicareas",
                                    waste_location == "Near parking lots" ~ "parkinglots", 
                                    waste_location == "Deeper in the forest" ~ "deepforest",
                                    .default = "other")
    )

# do it for activities
survey_shorter3 <- survey_shorter2 |> 
  separate_longer_delim(activities, ", ") |> 
  mutate(activities = case_when(activities == "Walking / Running" ~ "walking",
                                activities == "Barbecue or Picnic" ~ "picnic",
                                activities == "Cycling / Biking" ~ "biking",
                                activities == "Photography" ~ "photography",
                                activities == "Camping" ~ "camping",
                                activities == "Horse Riding" ~ "horseriding",
                                activities == "Bird Watching" ~ "birdwatching",
                                activities == "Gathering" ~ "gathering",
                                .default = "other"))

# do it for activities_today
survey_shorter4 <- survey_shorter3 |> 
  separate_longer_delim(activities_today, ", ") |> 
  mutate(activities_today = case_when(activities_today == "Walking / Running" ~ "walking",
                                activities_today == "Barbecue or Picnic" ~ "picnic",
                                activities_today == "Cycling / Biking" ~ "biking",
                                activities_today == "Photography" ~ "photography",
                                activities_today == "Camping" ~ "camping",
                                activities_today == "Horse Riding" ~ "horseriding",
                                activities_today == "Bird Watching" ~ "birdwatching",
                                activities_today == "Gathering" ~ "gathering",
                                .default = "other"))

# do it for activities_wasteful
survey_shorter5 <- survey_shorter4 |> 
  separate_longer_delim(activities_wasteful, ", ") |> 
  mutate(activities_wasteful = case_when(activities_wasteful == "Walking / Running" ~ "walking",
                                      activities_wasteful == "Barbecue or Picnic" ~ "picnic",
                                      activities_wasteful == "Cycling / Biking" ~ "biking",
                                      activities_wasteful == "Photography" ~ "photography",
                                      activities_wasteful == "Camping" ~ "camping",
                                      activities_wasteful == "Horse Riding" ~ "horseriding",
                                      activities_wasteful == "Bird Watching" ~ "birdwatching",
                                      activities_wasteful == "Gathering" ~ "gathering",
                                      .default = "other"))
# do it for measures
survey_shorter6 <- survey_shorter5 |> 
  separate_longer_delim(measures, ", ") |> 
  mutate(measures = case_when(measures == "Removal by authorities" ~ "authorities",
                              measures == "More trash bins" ~ "bins",
                              measures == "Signs telling people to take their trash home" ~ "signs",
                              measures == "Fines for littering" ~ "fines",
                              measures == "Organized clean-up events" ~ "cleanupevent",
                              measures == "Volunteer forest rangers" ~ "volunteers"))

# for measures_responsible, simply shorten the answers to single words
survey1_almost <- survey_shorter6 |> 
  mutate(measures_responsible = case_when(measures_responsible == "Pick up by the one who left it" ~ "litterer",
                                          measures_responsible == "Pick up by authorities" ~ "authorities",
                                          measures_responsible == "Volunteers should pick it up" ~ "volunteers",
                                          measures_responsible == "I feel responsible to pick it up" ~ "me",
                                          measures_responsible == "Nobody is responsible" ~ "nobody",))

# step 6: create second dataset for the waste situation today: ----------
# when people answered "Yes" to "Have you seen trash in the forest today?"

# filtering out participants who didn't see waste today
# and separating multiple text values in one cell
survey2 <- survey1_almost |> 
  filter(waste_seen_today == "yes") |> 
  separate_longer_delim(waste_location_today, ", ") |> 
  separate_longer_delim(waste_type_today, ", ")

# shorten text values to single words
# first for waste_location_today
survey2 <- survey2 |> 
  mutate(waste_location_today = case_when(waste_location_today == "Along paths or trails" ~ "paths",
                                    waste_location_today == "Around picnic areas or benches" ~ "picnicareas",
                                    waste_location_today == "Near parking lots" ~ "parkinglots", 
                                    waste_location_today == "Deeper in the forest" ~ "deepforest",
                                    .default = "other")
           )

survey2 <- survey2 |> 
  mutate(waste_type_today = case_when(waste_type_today == "Paper or Cardboard" ~ "paper",
                                      waste_type_today == "Plastic Bottles" ~ "plasticbottles",
                                      waste_type_today == "Cans" ~ "cans",
                                      waste_type_today == "Food Packaging" ~ "foodpackaging",
                                      waste_type_today == "Cigarette Butts" ~ "cigarettes",
                                      waste_type_today == "Dog waste bags" ~ "dogwastebag",
                                      waste_type_today == "Left-behind clothing items" ~ "clothing",
                                      waste_type_today == "Horse Shit" ~ "horseshit",
                                      .default = "other")
         )

# step 7: repeat step 6 for dataset survey1 (which has all participants but many NA cells)
survey1 <- survey1_almost |> 
  separate_longer_delim(waste_location_today, ", ") |> 
  separate_longer_delim(waste_type_today, ", ")

# shorten text values to single words
# first for waste_location_today
survey1 <- survey1 |> 
  mutate(waste_location_today = case_when(waste_location_today == "Along paths or trails" ~ "paths",
                                          waste_location_today == "Around picnic areas or benches" ~ "picnicareas",
                                          waste_location_today == "Near parking lots" ~ "parkinglots", 
                                          waste_location_today == "Deeper in the forest" ~ "deepforest",
                                          .default = "other")
  )

survey1 <- survey1 |> 
  mutate(waste_type_today = case_when(waste_type_today == "Paper or Cardboard" ~ "paper",
                                      waste_type_today == "Plastic Bottles" ~ "plasticbottles",
                                      waste_type_today == "Cans" ~ "cans",
                                      waste_type_today == "Food Packaging" ~ "foodpackaging",
                                      waste_type_today == "Cigarette Butts" ~ "cigarettes",
                                      waste_type_today == "Dog waste bags" ~ "dogwastebag",
                                      waste_type_today == "Left-behind clothing items" ~ "clothing",
                                      waste_type_today == "Horse Shit" ~ "horseshit",
                                      .default = "other")
  )

# now I have 2 clean datasets, survey1 and survey2
# survey1 has all the participants 
# but also all those who said "No, I haven't seen trash today"
# --> survey1 has lots of NA cells for waste_location_today and waste_type_today
# survey 2 has only those people who answered "Yes, I've seen trash today"

# step 8: create ordered factor variable for measures_responsible and weekday ------------

# order for weekday: Survey started on Sunday, ended on Thursday
levels_weekday <- c("sunday", "monday", "tuesday", "wednesday", "thursday")

# order for measures: How much is it a personal responsibility vs a collective responsibility
levels_responsible <- c("litterer", "me", "volunteers", "authorities", "nobody")

# order and create factor variables for survey1
survey1_ordered <- survey1 |> 
  mutate(weekday = factor(weekday, levels = levels_weekday)) |> 
  mutate(measures_responsible = factor(measures_responsible, levels = levels_responsible))


# repeat for survey2
survey2_ordered <- survey2 |> 
  mutate(weekday = factor(weekday, levels = levels_weekday)) |> 
  mutate(measures_responsible = factor(measures_responsible, levels = levels_responsible))



# legacy code, use only for reference ----------

# coerce data types ------------------------

head(survey_names)
# todo: recycler_level to always, mostly, some, never


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

# recycler_level and disposal_level to factor variables
levels_recycling <- c("always", "mostly", "sometimes", "never")
levels_disposal <- c("fee-bag", "non-fee-bag", "public-bin", "bushes", "toilet", "hole", "fire")
# define PET and banana recycling levels
levels_park_recycling = c("bushes", "on-full-bin", "next-to-bin", "find-recycling", "take-home", "other")

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
