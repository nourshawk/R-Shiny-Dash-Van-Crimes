library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(proj4)
library(plotly)
 
df_raw <- read_csv("../data/combined_crime_data_2023_2025.csv", show_col_types = FALSE)

#merging vehicle collison subtypes
df_raw <- df_raw |>
  mutate(TYPE = case_when(
    grepl("Vehicle Collision", TYPE) ~ "Vehicle Collision or Pedestrian Struck",
    TRUE ~ TYPE))

# Converting lon/lat 
valid <- df_raw |> filter(X != 0, Y != 0)
pts <- st_as_sf(valid, coords = c("X", "Y"), crs = 32610) |>
  st_transform(crs = 4326)
coords <- st_coordinates(pts)
valid$LON <- coords[, 1]
valid$LAT <- coords[, 2]

df <- bind_rows(
  valid,
  df_raw |> filter(X == 0 | Y == 0) |> mutate(LON = NA, LAT = NA))

# Re-attach homicde rows with no lat/lon so they are still in filtered df and bar chart
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
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))),

  #Navigation bar set up
  div(class = "navbar-custom",
    h2(class = "app-title", "VanCrimeWatch with R")),

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
          div(class = "card-title", "Crime Map"),
          leafletOutput("map", height = "420px")
        ),
 
        # Bar chart
        div(class = "card-panel",
          div(class = "card-title", "Top 5 Crime Types"),
          plotlyOutput("bar_chart", height = "320px")
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

 observeEvent(input$reset_btn, {
    updateSelectizeInput(session, "neighbourhood", selected = default_neighbourhoods)
    updateCheckboxGroupInput(session, "year", selected = 2025)
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

  #Map
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
        lng         = ~LON,
        lat         = ~LAT,
        radius      = ~radius,
        color       = "#1565c0",
        weight      = 1,
        fillColor   = "#1565c0",
        fillOpacity = 0.5,
        label       = ~paste0(NEIGHBOURHOOD, ": ", format(COUNT, big.mark = ","), " crimes"),
        popup       = ~paste0("<b>", NEIGHBOURHOOD, "</b><br>Total Crimes: ", format(COUNT, big.mark = ",")),
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      )
  })
 
  #Bar Chart
  output$bar_chart <- renderPlotly({
    d <- filtered_data()
    if (nrow(d) == 0) {
      plotly_empty() |> layout(title = "No data for selected filters")
    } else {
      top5 <- d |>
        count(TYPE, sort = TRUE) |>
        slice_head(n = 5) |>
        mutate(TYPE = reorder(TYPE, n))
 
      plot_ly(
        top5,
        x           = ~n,
        y           = ~TYPE,
        type        = "bar",
        orientation = "h",
        marker      = list(color = "#1565c0"),
        hovertemplate = ~paste0("<b>", TYPE, "</b><br>Incidents: %{x:,}<extra></extra>")
      ) |>
        layout(
          xaxis  = list(title = "Number of Incidents", tickformat = ","),
          yaxis  = list(title = ""),
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        ) |>
        config(displayModeBar = FALSE)
    }
  })
}

shinyApp(ui, server)

#print(head(valid[, c("LON", "LAT")]))