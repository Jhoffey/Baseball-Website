# GCU Baseball Analytics – TrackMan Tools

This repo includes a set of R scripts and a Shiny app I built to help analyze TrackMan data for GCU Baseball. 
The goal is to give coaches, players, and analysts better tools to evaluate pitch quality both in terms of where pitches are located and how effective they are.

## What's Inside

### RVLocation.R
This script creates a custom Location+ score. It merges datasets, assigns run values to pitch calls and results, and uses XGBoost models to score pitch locations. 
It breaks things down by pitch type and batter handedness, then exports the final data to CSVs.


### RVStuff.R
This script builds a Stuff+ model based on pitch movement, spin rate, velocity, and release metrics. 
It trains separate models for each pitch type and outputs a normalized score centered around 100 that reflects the overall quality of each pitch.


### app.R
This is the Shiny app that pulls everything together. It allows users to:
- Filter and visualize pitcher or hitter data
- View heatmaps, zone plots, spin rate trends, and pitch movement
- Explore Stuff+ and Location+ scores interactively
- Compare performance by count, pitch type, or result
- View postgame reports in PDF format inside the app

Use case: A live dashboard designed for coaches, analysts, and players to explore performance and make data-informed decisions

## Getting Started

1. Make sure you have R and RStudio installed
2. Install the required packages

```r
install.packages(c("tidyverse", "shiny", "shinythemes", "shinydashboard", 
                   "DT", "plotly", "shinyWidgets", "xgboost", "caret", "pROC"))
