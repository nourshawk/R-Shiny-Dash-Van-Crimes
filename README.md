# VanCrimeWatch — R Edition

> A Shiny for R version of the VanCrimeWatch dashboard for visualizing Vancouver neighborhood crime patterns.

**[Live App](https://nourshawky-r-shiny-dash-van-crimes.share.connect.posit.cloud/)**

## Purpose

VanCrimeWatch is an interactive dashboard that visualizes crime data across Vancouver neighbourhoods from 2023–2025, based on publicly available data from the Vancouver Police Department. It is designed to help prospective business owners and community members identify crime hotspots to make informed decisions about thier businesses and safety.

The dashboard includes:

- **Interactive Map** : Bubble map where size reflects crime density per neighbourhood, users can select their desired neighborhoods to explore in the dropdown menu in the sidebar
- **Top 5 Crime Types** : Bar chart showing the most common crimes for the selected neighborhoods, with interactive tooltip for exact counts.
- **KPI Cards** : Total crimes, least crime neighbourhood, and most common crime type
- **Neighbourhood & Year Filters** : Filter by specific Vancouver neighbourhoods and years (2023–2025)
- **Reset Filters** : Button that helps users resort back to default selections (Downtown Neighborhoods, where crime is most concentrated and year 2025, the most recent year)

## Installation

### 1. Clone the repository
```bash
git clone https://github.com/nourshawk/R-Shiny-Dash-Van-Crimes.git
cd R-Shiny-Dash-Van-Crimes
```

### 2. Install R dependencies

Open R or RStudio and run:
```r
install.packages(c("shiny", "leaflet", "dplyr", "ggplot2", "readr", "sf", "plotly"))
```

Or restore the exact package versions using renv:
```r
install.packages("renv")
renv::restore()
```

### 3. Run the app
```r
shiny::runApp("src/app.R")
```

Or from the terminal:
```bash
Rscript -e "shiny::runApp('src/app.R')"
```

### 4. Open the link in your browser
```
http://127.0.0.1:PORT
```

## Dataset

The dataset is sourced from the [Vancouver Police Department](https://geodash.vpd.ca/opendata/), covering crime incidents from 2023 to 2025.

## License

This project is licensed under the terms of the [MIT License](https://github.com/nourshawk/R-Shiny-Dash-Van-Crimes/blob/main/LICENSE).

## Attribution 

This project is directly attributed from the [VanCrimeWatch](https://github.com/UBC-MDS/DSCI-532_2026_4_VanCrimeWatch/tree/main) Python version.