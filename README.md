# Early-Childhood-Development-Explorer
# UVA STAT 3280 Project

## Overview
This Shiny app helps users examine global differences in early learning resources using UNICEF data. It allows exploration of access to books, play materials and responsive caregiving across countries, and how these indicators relate to developmental inequality measures such as the early education wealth gap.

## Live App
[Launch app](https://miartan.shinyapps.io/early_childhood-1/)

## Features
- Interactive world map (Plotly choropleth)
- Country selection and “Select All” button
- Customizable scatter plots with regression lines
- Bar chart summaries by book access level
- Summary statistics table

## Instructions
1. **Select Countries** – choose one or more countries, or click “Select All.”
2. **Choose Variables** – adjust X-axis, Y-axis, and Map Variable.
3. **Interactive Map** – hover over countries to see values.
4. **Bar Chart** – average book access by category.
5. **Scatter Plot** – compare two indicators with a trend line.
6. **Summary Table** – descriptive statistics for selected variables.

## Dependencies
- shiny
- tidyverse
- readr
- janitor
- plotly

## Data
See `data/`
Dataset compiled and cleaned from UNICEF: https://www.unicef.org/early-childhood-development. 
