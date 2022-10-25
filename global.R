# - (1 Point) Tahapan data pre-processing menggunakan dplyr 1 > filter/arange/mutate/groupby...
# - (1 Point) Plot yang ditampilkan pada dashboard sudah interaktif > cukup pakai plotly atau ggplotly(...)
# - (1 Point) Setiap plot yang ditampilkan menampilkan informasi yang relevan dari dashboard > informasinya apa?

library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(magrittr)
library(readr)

# https://www.kaggle.com/datasets/evangower/eurovision-song-contest

evdataset <- read_csv("dataset/evdataset.csv")

evdataset

# evdataset |>
#   group_by(year, artist_country, section) |>
#   filter(qualified) |>
#   summarise(qualified_count = n()) |>
#   ungroup() 