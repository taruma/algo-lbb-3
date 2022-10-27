# LBB3
# - (1 Point) Tahapan data pre-processing menggunakan dplyr 1 > filter/arange/mutate/groupby...
# - (1 Point) Plot yang ditampilkan pada dashboard sudah interaktif > cukup pakai plotly atau ggplotly(...)
# - (1 Point) Setiap plot yang ditampilkan menampilkan informasi yang relevan dari dashboard > informasinya apa?

# GLOBAL.R SCOPE ----

# LOAD LIBRARY ----

library(shiny)
library(shinydashboard)

library(plotly)
library(ggplot2)

library(readr)
library(dplyr)
library(glue)

library(DT)

# LOAD DATASET ----

# https://www.kaggle.com/datasets/mohamedalishiha/electric-vehicles

evdataset <- read_csv("dataset/evdataset.csv")

# DATASET: ELECTRIC VEHICLES (evdataset) ====

# 01 |> id <- unique identifier
# 02 |> Make <- brand of the car
# 03 |> link <- source url
# 04 |> City - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) in cities
# 05 |> Highway - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) on highways
# 06 |> Combined - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) combined
# 07 |> City - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
# 08 |> Highway - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
# 09 |> Combined - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
# 10 |> Acceleration 0 - 100 km/h [seconds] <- acceleration from 0 to 100 Km per hour in seconds
# 11 |> Top Speed [Km/h] <- Top speed in Km/h
# 12 |> Electric Range [Km] <- Advertised electric range in Km
# 13 |> Total Power [Power]
# 14 |> Total Torque [Torque]
# 15 |> Drive <- Rear, Front, AWD (Sumbu roda?)
# 16 |> Battery Capacity [KW] <- Total capacity of the battery in KW
# 17 |> Charge Power
# 18 |> Charge Speed
# 19 |> Fastcharge Speed
# 20 |> Length [mm] <- Car lengths in mm
# 21 |> Width [mm] <- Car width in mm
# 22 |> Height [mm] <- Car height in mm
# 23 |> Wheelbase [mm] <- Wheelbase in mm
# 24 |> Gross Vehicle Weight (GVWR) [Kg] <- Gross weight of the car in Kg
# 25 |> Max. Payload
# 26 |> Cargo Volume [liter/dm^3] <- Cargo volume of the car in litters
# 27 |> Seats <- Number of seats

# CODE ----

# 1 PREPARATION ====

# Menyederhanakan nama kolom

newname_columns <- c(
  "id", "make", "link",
  "cold_city", "cold_highway", "cold_combined",
  "mild_city", "mild_highway", "mild_combined",
  "acceleration", "top_speed", "electric_range",
  "total_power", "total_torque",
  "drive",
  "battery_capacity", "charge_power", "charge_speed", "fastcharge_speed",
  "length", "width", "height", "wheelbase",
  "gross_vehicle_weight", "max_payload",
  "cargo_volume", "seats"
)

names(evdataset) <- newname_columns

# Mengubah make menjadi kategori

evdataset$make <- evdataset$make |> as.factor()
evdataset$drive <- evdataset$drive |> as.factor()

# RUBRIK 1: PREPROCESSING DPLYR ====

# (1 Point) Tahapan data pre-processing menggunakan dplyr 1
# -> filter/arange/mutate/groupby...

# KECEPATAN SETIAP BRAND ####

evspeed <- evdataset |>
  select(make, top_speed, acceleration) |>
  group_by(make) |>
  summarise(
    mean_top_speed = mean(top_speed),
    max_top_speed = max(top_speed),
    total_cars = n()
  ) |>
  ungroup() |>
  mutate(
    tooltip = glue(
      "<b>{make}</b> [{total_cars} car(s)]
      Rata-rata: {mean_top_speed |> format(digits = 4)} Km/jam
      Maksimum: {format(max_top_speed, digits = 4)} Km/jam"
    )
  ) |>
  arrange(desc(mean_top_speed))

# 2 PLOT ----

# PLOT COL/BAR KECEPATAN ====

funcplotevspeed <- function(dataset) {
  dataset |>
    ggplot(aes(
      x = reorder(make, mean_top_speed),
      y = mean_top_speed,
      text = tooltip
    )) +
    geom_col(
      aes(fill = max_top_speed),
      show.legend = T
    ) +
    scale_fill_gradient(low = "black", high = "red") +
    coord_flip() +
    labs(
      title = "Kecepatan Terbaik <i>(Top Speed)</i> Mobil Listrik",
      x = "Brand",
      y = "Kecepatan (Km/jam)",
      fill = "Kecepatan<br>Maksimum"
    )
}

# RUBRIK 2+3: PLOT INTERAKTIF + PLOT RELEVAN ====

# (1 Point) Plot yang ditampilkan pada dashboard sudah interaktif
# -> cukup pakai plotly atau ggplotly(...)
# (1 Point) Setiap plot yang ditampilkan menampilkan informasi
# yang relevan dari dashboard
# -> menampilkan perbandingan kecepatan rata-rata antar brand

# ggplotly(funcplotevspeed(evspeed), tooltip = "text")


# SHINY APPS ------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = "MOBIL ELEKTRIK"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Kecepatan Terbaik",
        tabName = "menuTopSpeed",
        icon = icon("car")
      ),
      menuItem(
        "Dataset",
        tabName = "menuDataset",
        icon = icon("table")
      ),
      menuItem(
        text = "Source code @ Github",
        href = "https://github.com/taruma/algo-lbb-3"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "menuTopSpeed",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              sliderInput(
                "sliderTotalCars",
                label = "Jumlah Mobil Minimum",
                min = 1,
                max = max(evspeed$total_cars),
                value = 1,
                step = 1,
                round = TRUE,
              )
            ),
            mainPanel(
              plotlyOutput(
                "plotlySpeed",
                height = "700px"
              )
            ),
          )
        )
      ),
      tabItem(
        "menuDataset",
        fluidPage(
          markdown(
            "# DATASET _ELECTRIC VEHICLE_

            Source: <https://www.kaggle.com/datasets/mohamedalishiha/electric-vehicles>
            
            ```
            - 01 |> id <- unique identifier
            - 02 |> Make <- brand of the car
            - 03 |> link <- source url
            - 04 |> City - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) in cities
            - 05 |> Highway - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) on highways
            - 06 |> Combined - Cold Weather [Km] <- range in Km under cold weather conditions (-10 degrees) combined
            - 07 |> City - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
            - 08 |> Highway - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
            - 09 |> Combined - Mild Weather [Km] <- range in Km under mild weather conditions (23 degrees) in cities
            - 10 |> Acceleration 0 - 100 km/h [seconds] <- acceleration from 0 to 100 Km per hour in seconds
            - 11 |> Top Speed [Km/h] <- Top speed in Km/h
            - 12 |> Electric Range [Km] <- Advertised electric range in Km
            - 13 |> Total Power [Power]
            - 14 |> Total Torque [Torque]
            - 15 |> Drive <- Rear, Front, AWD (Sumbu roda?)
            - 16 |> Battery Capacity [KW] <- Total capacity of the battery in KW
            - 17 |> Charge Power
            - 18 |> Charge Speed
            - 19 |> Fastcharge Speed
            - 20 |> Length [mm] <- Car lengths in mm
            - 21 |> Width [mm] <- Car width in mm
            - 22 |> Height [mm] <- Car height in mm
            - 23 |> Wheelbase [mm] <- Wheelbase in mm
            - 24 |> Gross Vehicle Weight (GVWR) [Kg] <- Gross weight of the car in Kg
            - 25 |> Max. Payload
            - 26 |> Cargo Volume [liter/dm^3] <- Cargo volume of the car in litters
            - 27 |> Seats <- Number of seats
            ```
            "
          ),
          DT::dataTableOutput("tableDataset")
        )
      )
    )
  ),
  skin = "red"
)

server <- function(input, output) {
  evspeedfilter <- reactive(evspeed |>
    filter(total_cars >= input$sliderTotalCars))

  output$plotlySpeed <- renderPlotly(
    ggplotly(
      funcplotevspeed(
        evspeedfilter()
      ),
      tooltip = "text"
    )
  )

  # https://stackoverflow.com/questions/30765338
  output$tableDataset <- renderDataTable(
    evdataset, options = list(pageLength = 20, scrollX = "400px"), filter = 'top'
  )
}

shinyApp(ui = ui, server = server)
