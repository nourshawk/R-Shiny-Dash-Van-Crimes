library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(proj4)


df_raw <- read_csv("data/combined_crime_data_2023_2025.csv", show_col_types = FALSE)

#merging vehicle collison subtypes
df_raw <- df_raw |>
  mutate(TYPE = case_when(
    grepl("Vehicle Collision", TYPE) ~ "Vehicle Collision or Pedestrian Struck",
    TRUE ~ TYPE))

# Convert UTM Zone 10N (EPSG:32610) → WGS84 lat/lon
valid <- df_raw |> filter(X != 0, Y != 0)
coords <- proj4::ptransform(
  cbind(valid$X, valid$Y),
  src.proj = "+proj=utm +zone=10 +datum=WGS84",
  dst.proj  = "+proj=longlat +datum=WGS84")
valid$LON <- coords[, 1]
valid$LAT <- coords[, 2]

# Re-attach rows that had no coordinates (e.g. Homicide)
df <- bind_rows(
  valid,
  df_raw |> filter(X == 0 | Y == 0) |> mutate(LON = NA, LAT = NA))

all_neighbourhoods <- sort(unique(df$NEIGHBOURHOOD))
all_years          <- sort(unique(df$YEAR))

default_neighbourhoods <- c("Central Business District", "West End")
business_crimes <- c(
  "Break and Enter Commercial", "Theft from Vehicle",
  "Other Theft", "Mischief", "Theft of Vehicle")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f8f9fa; margin: 0; }
    .navbar-custom {
      background: #1565c0; color: white; padding: 14px 24px;
      font-size: 1.4rem; font-weight: 700; margin-bottom: 0;
    }
    .sidebar-panel {
      background: white; border-radius: 8px; padding: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,.1); height: 100%;
    }
    .main-panel { padding: 0 8px; }
    .kpi-box {
      background: white; border-radius: 8px; padding: 16px 12px;
      box-shadow: 0 1px 4px rgba(0,0,0,.1); text-align: center; margin-bottom: 12px;
    }
    .kpi-title { font-size: .8rem; color: #6c757d; margin-bottom: 4px; }
    .kpi-value { font-size: 1.6rem; font-weight: 700; color: #1565c0; }
    .card-panel {
      background: white; border-radius: 8px; padding: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,.1); margin-bottom: 16px;
    }
    .card-title { font-size: 1rem; font-weight: 600; margin-bottom: 10px; color: #333; }
    .btn-success { width: 100%; margin-top: 12px; }
    h2.app-title { margin: 0; color: white; font-size: 1.4rem; }
  "))),

  #Navigation bar set up
  div(class = "navbar-custom",
    h2(class = "app-title", "🔍 VanCrimeWatch — R Edition")),

  br(),

  fluidRow(
    # Side bar set up
    column(3,
      div(class = "sidebar-panel",
        h5("Dashboard Filters", style = "font-weight:600; margin-bottom:14px;"),

        selectizeInput(
          "neighbourhood", "Select Neighbourhoods:",
          choices  = all_neighbourhoods,
          selected = default_neighbourhoods,
          multiple = TRUE,
          options  = list(placeholder = "Displaying All", plugins = list("remove_button"))
        ),

        checkboxGroupInput(
          "year", "Select Year:",
          choices  = setNames(all_years, all_years),
          selected = 2025
        ),

        actionButton("reset_btn", "Reset Filters", class = "btn btn-success")
      )
    ),

    
    column(9,
      div(class = "main-panel",

        
        fluidRow(
          column(4, div(class = "kpi-box",
            div(class = "kpi-title", "Total Crimes"),
            div(class = "kpi-value", textOutput("kpi_total", inline = TRUE))
          )),
          column(4, div(class = "kpi-box",
            div(class = "kpi-title", "Least Crime Neighbourhood"),
            div(class = "kpi-value", style = "font-size:1rem;",
                textOutput("kpi_safest", inline = TRUE))
          )),
          column(4, div(class = "kpi-box",
            div(class = "kpi-title", "Most Common Crime"),
            div(class = "kpi-value", style = "font-size:1rem;",
                textOutput("kpi_top_crime", inline = TRUE))
          ))
        ),

        # Map
        div(class = "card-panel",
          div(class = "card-title", "🗺️ Crime Map — Bubble size reflects crime density"),
          leafletOutput("map", height = "420px")
        ),

        # Bar chart
        div(class = "card-panel",
          div(class = "card-title", "📊 Top 5 Crime Types"),
          plotOutput("bar_chart", height = "320px")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # Reactive dataframe
  filtered_data <- reactive({
    d <- df
    if (length(input$neighbourhood) > 0)
      d <- d |> filter(NEIGHBOURHOOD %in% input$neighbourhood)
    if (length(input$year) > 0)
      d <- d |> filter(YEAR %in% as.integer(input$year))
    d
  })

 
  output$kpi_total <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })

  output$kpi_safest <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    counts <- d |> count(NEIGHBOURHOOD, sort = TRUE)
    counts$NEIGHBOURHOOD[which.min(counts$n)]
  })

  output$kpi_top_crime <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    counts <- d |> count(TYPE, sort = TRUE)
    counts$TYPE[1]
  })

  
  output$map <- renderLeaflet({
    d <- filtered_data() |> filter(!is.na(LAT), !is.na(LON))

    neighbourhood_counts <- d |>
      group_by(NEIGHBOURHOOD, LAT, LON) |>
      summarise(COUNT = n(), .groups = "drop") |>
      group_by(NEIGHBOURHOOD) |>
      summarise(
        LAT   = mean(LAT),
        LON   = mean(LON),
        COUNT = sum(COUNT),
        .groups = "drop"
      )

    max_count <- max(neighbourhood_counts$COUNT, 1)
    neighbourhood_counts <- neighbourhood_counts |>
      mutate(radius = 5 + sqrt(COUNT / max_count) * 40)

    leaflet(neighbourhood_counts) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -123.10, lat = 49.25, zoom = 12) |>
      addCircleMarkers(
        lng    = ~LON,
        lat    = ~LAT,
        radius = ~radius,
        color  = "#1565c0",
        weight = 1,
        fillColor  = "#1565c0",
        fillOpacity = 0.5,
        popup  = ~paste0("<b>", NEIGHBOURHOOD, "</b><br>Total Crimes: ", format(COUNT, big.mark = ","))
      )
  })

  
  output$bar_chart <- renderPlot({
    d <- filtered_data()
    if (nrow(d) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data for selected filters",
                 size = 6, colour = "grey50") +
        theme_void()
    } else {
      top5 <- d |>
        count(TYPE, sort = TRUE) |>
        slice_head(n = 5) |>
        mutate(TYPE = reorder(TYPE, n))

      ggplot(top5, aes(x = TYPE, y = n, fill = TYPE)) +
        geom_col(show.legend = FALSE, width = 0.65) +
        coord_flip() +
        scale_fill_manual(values = rep("#1565c0", 5)) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL, y = "Number of Incidents") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text          = element_text(colour = "#333"),
          plot.background    = element_rect(fill = "white", colour = NA)
        )
    }
  })
}

shinyApp(ui, server)