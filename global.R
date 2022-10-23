library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(magrittr)
library(readr)

# https://www.kaggle.com/datasets/evangower/eurovision-song-contest

eurovision <- read_csv("dataset/eurovision.csv")

# eurovision |> lapply(unique) |> lapply(length) |> print()

# factor_columns <- c(
#   "event", "host_city", "section", "artist_country"
# )
# 
# eurovision_clean <- eurovision |>
#   mutate(across(all_of(factor_columns), as.factor))

eurovision |>
  group_by(year, artist_country, section) |>
  filter(qualified) |>
  summarise(qualified_count = n()) |>
  ungroup() 