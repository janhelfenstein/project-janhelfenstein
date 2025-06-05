# 01-data-download.R
# This script downloads data from the google survey about waste separation,
# which we did in the RBTL course in FS25
# author: Jan Helfenstein
# date: 2025-04-01

# libraries
library(googlesheets4)
library(tidyverse)

# import data from google sheet
survey_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1IRUdMmEAWMbT-olep1jKo_4UReBfhb8ACsQbxbs__dU")

# store data
write_csv(survey_raw, "data/raw/survey-raw.csv")
