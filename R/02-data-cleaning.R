# 02-data-cleaning.R
# This script cleans and tidies the data from the google survey about waste in the forest,
# which was the capstone project in the RBTL course in the 2025 spring semester
# author: Jan Helfenstein
# date: 2025-06-04

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
survey_renamed <- survey_renamed |> 
  mutate(date = as_date(timestamp)) |> 
  mutate(time = format(timestamp, "%H:%M:%S")) |> 
  mutate(weekday = wday(timestamp, label = TRUE, abb = FALSE)) |> 
  relocate(weekday) |> 
  relocate(time) |> 
  relocate(date) |> 
  relocate(timestamp) # timestamp to the front

# create dataset with no timestamp variable
# this dataset is "cleaner" because the time info is not saved twice
# but for data visualization it is easier to use the survey_renamed dataset
survey_dates <- survey_renamed |> 
  select(!timestamp) 

# step 3: add a column "id" and reorder
survey_id <- survey_renamed |> 
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
survey_short1 <- survey_id |> 
  mutate(sports_event_waste = case_when(sports_event_waste == "I'm not sure" ~ "unsure",
                                        sports_event_waste == "Yes" ~ "yes",
                                        sports_event_waste == "No" ~ "no",
                                        .default = "unsure")
        )

survey_short1 <- survey_short1 |> 
  mutate(sports_littering = case_when(sports_littering == "I'm not sure" ~ "unsure",
                                      sports_littering == "Yes" ~ "yes",
                                      sports_littering == "No" ~ "no",
                                      .default = "unsure")
        )

# create boolean variable for the question "Did you see waste in the forest today?"
survey_short1 <- survey_short1 |> 
  mutate(waste_seen_today = case_when(waste_seen_today == "Yes" ~ TRUE,
                                      waste_seen_today == "No" ~ FALSE,
                                      .default = FALSE)
         )

# do the same to change weekday and gender variables to lowercase,
# e.g. from "Sunday" to "sunday" and  from "Male" to male"
# to avoid typing too much I asked ChatGPT, see prompts here:
# https://chatgpt.com/share/68406f4e-4628-8011-a1cd-6a9b0d268322

survey_short1 <- survey_short1 |> 
  mutate(gender = str_to_lower(gender)) |> 
  mutate(weekday = str_to_lower(weekday))

# Shorten texts for measures_responsible
survey_short1 <- survey_short1 |> 
  mutate(measures_responsible = case_when(measures_responsible == "Pick up by the one who left it" ~ "litterer",
                                          measures_responsible == "Pick up by authorities" ~ "authorities",
                                          measures_responsible == "Volunteers should pick it up" ~ "volunteers",
                                          measures_responsible == "I feel responsible to pick it up" ~ "me",
                                          measures_responsible == "Nobody is responsible" ~ "nobody",))


# step 5: create ordered factor variable for measures_responsible and weekday ------------

# order for weekday: Survey started on Sunday, ended on Thursday
levels_weekday <- c("sunday", "monday", "tuesday", "wednesday", "thursday")

# order for measures: How much is it a personal responsibility vs a collective responsibility
levels_responsible <- c("litterer", "me", "volunteers", "authorities", "nobody")

# order and create factor variables for survey1
survey_ordered <- survey_short1 |> 
  mutate(weekday = factor(weekday, levels = levels_weekday)) |> 
  mutate(measures_responsible = factor(measures_responsible, levels = levels_responsible))

# This dataset survey_ordered will be used for some visualization tasks
# It has the benefit of having 1 row per participant
# But it is not a clean dataset, because it has multiple values in a single cell in some columns

# step 6: separate and shorten text values -------------------

# separate multiple text values stored in one cell, coming from to multiple choice questions
# then shorten the text values to single words
# do it for waste_location
survey_short2 <- survey_ordered |> 
  separate_longer_delim(waste_location, ", ") |> 
  mutate(waste_location = case_when(waste_location == "Along paths or trails" ~ "paths",
                                    waste_location == "Around picnic areas or benches" ~ "picnicareas",
                                    waste_location == "Near parking lots" ~ "parkinglots", 
                                    waste_location == "Deeper in the forest" ~ "deepforest",
                                    .default = "other")
    )

# do it for activities
survey_short3 <- survey_short2 |> 
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
survey_short4 <- survey_short3 |> 
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
survey_short5 <- survey_short4 |> 
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
survey_short6 <- survey_short5 |> 
  separate_longer_delim(measures, ", ") |> 
  mutate(measures = case_when(measures == "Removal by authorities" ~ "authorities",
                              measures == "More trash bins" ~ "bins",
                              measures == "Signs telling people to take their trash home" ~ "signs",
                              measures == "Fines for littering" ~ "fines",
                              measures == "Organized clean-up events" ~ "cleanupevent",
                              measures == "Volunteer forest rangers" ~ "volunteers"))

# step 7: create second dataset for the waste situation today: ----------
# when people answered "Yes" to "Have you seen trash in the forest today?"

# filtering out participants who didn't see waste today
# and separating multiple text values in one cell
survey2_short <- survey_short6 |> 
  filter(waste_seen_today == TRUE) |> 
  separate_longer_delim(waste_location_today, ", ") |> 
  separate_longer_delim(waste_type_today, ", ")

# shorten text values to single words
# first for waste_location_today
survey2_short <- survey2_short |> 
  mutate(waste_location_today = case_when(waste_location_today == "Along paths or trails" ~ "paths",
                                    waste_location_today == "Around picnic areas or benches" ~ "picnicareas",
                                    waste_location_today == "Near parking lots" ~ "parkinglots", 
                                    waste_location_today == "Deeper in the forest" ~ "deepforest",
                                    .default = "other")
           )

survey2_short <- survey2_short |> 
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

# step 8: repeat step 6 for dataset survey1 (which has all participants but many NA cells)
survey1_short <- survey_short6 |> 
  separate_longer_delim(waste_location_today, ", ") |> 
  separate_longer_delim(waste_type_today, ", ")

# shorten text values to single words
# first for waste_location_today
survey1_short <- survey1_short |> 
  mutate(waste_location_today = case_when(waste_location_today == "Along paths or trails" ~ "paths",
                                          waste_location_today == "Around picnic areas or benches" ~ "picnicareas",
                                          waste_location_today == "Near parking lots" ~ "parkinglots", 
                                          waste_location_today == "Deeper in the forest" ~ "deepforest",
                                          .default = "other")
  )

survey1_short <- survey1_short |> 
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

# step 9: write csv and rds in the processed directory
# write csv and rds in processed
write_csv(survey1_short, "data/processed/survey1-data-processed.csv")
write_csv(survey2_short, "data/processed/survey2-data-processed.csv")
write_rds(survey1_short, "data/processed/survey1-data-processed.rds")
write_rds(survey2_short, "data/processed/survey2-data-processed.rds")

# save survey_ordered for the sake of having 1 row per participant
write_csv(survey_ordered, "data/processed/survey-small.csv")
write_rds(survey_ordered, "data/processed/survey-small.rds")
