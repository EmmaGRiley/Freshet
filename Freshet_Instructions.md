---
output:
  word_document: default
  html_document: default
---
# Freshet – Spring Break-Up Report Instructions

## Overview

The Freshet package automates the generation of NWT Water Monitoring Bulletins during spring break-up season. It produces a formatted Word document containing water level summaries, hydrographs, gauge photos, air temperature plots, weather forecast screenshots, and high-resolution water level appendices.

The two main scripts are:

- **Spring_Break_Up.R** – The user-facing workflow script. Configure stations, communities, date ranges, and run the report from here.
- **Spring_Break_Up_Functions.R** – Contains all functions used by the workflow script. You should not need to edit this file during regular use.

Supporting scripts:

- **image_download_utils.R** – Functions for retrieving gauge photos from Water Survey of Canada.
- **Station_photos_lists.R** – Defines which stations have available gauge photos.

---

## Prerequisites

### Required R packages

The following packages must be installed before running the scripts:

```r
install.packages(c(
  "RSQLite", "tidyhydat", "dplyr", "sf", "leaflet", "ggplot2",
  "DBI", "waiter", "fasstr", "officer", "httr", "xml2",
  "lubridate", "knitr", "rmarkdown", "flextable", "tidyquant",
  "readxl", "webshot", "stringr", "ggpubr", "scales"
))
```

### tidyhydat database

Install and download the HYDAT database (only needed once):

```r
library(tidyhydat)
download_hydat()
```

### PhantomJS (for weather forecast screenshots)

PhantomJS is required by the `webshot` package to capture ECCC weather forecast screenshots.

1. Download PhantomJS from https://phantomjs.org/download.html
2. Extract it to: `C:/Users/first_lastname/Documents/Modelling/phantomjs/phantomjs/`
3. The script automatically adds this to the PATH at runtime.

### Additional dependencies

The following scripts are sourced from outside the Freshet folder and must exist:

- `C:/Users/first_lastname/Documents/R_Scripts/Functions/Workflows/nwtclimate_functions.R`
- `C:/Users/first_lastname/Documents/R_Scripts/Functions/Workflows/update_climate.R`

These handle downloading and updating ECCC climate data (air temperature). The climate data RDS file is stored at:
`C:/Users/first_lastname/Documents/R_Scripts/Packages/nwtclimate/data/ECCC_Climate_Data_Merged.rds`

### Required data files

These are included in the Freshet folder:

- `data/Historic_Peaks_rough.xlsx` – Historical peak water levels for the summary table.
- `Template_BreakUpReport.docx` – Word template for the report layout.
- `Template_FactorsToWatch.docx` – Word template appended to the Weather Data section.
- `maps/` – Basin maps (automatically inserted based on active stations).

---

## File setup

1. Ensure the Freshet folder is located at: `C:/Users/first_lastname/Documents/R_Scripts/Freshet/`
2. The script will automatically create a `Reports/` subfolder for output.
3. Ensure the `nwtclimate` and `hydroclim` packages are installed (either from GitHub or locally).

---

## Using Spring_Break_Up.R

Open `Spring_Break_Up.R` in RStudio. The script is organized into the following sections:

### 1. Hydrometric List

Comment or uncomment station numbers to control which stations appear in the report. Stations are grouped by river basin:

```r
Hydrometric_List <- c(
  # Slave River Basin
  "07KC001", # Peace River at Peace Point
  ...
  # Hay River Basin
  "07OB001", # Hay River at Hay River
  ...
)
```

**Important:** The order of stations matters – they are listed in order of break-up progression (upstream to downstream). Only uncomment stations that are relevant to the current monitoring period.

### 2. Last Report

Set the date/time of the previous report. This is used to calculate water level changes since the last report:

```r
Last_Report <- "2025-03-10 15:00:00"
```

### 3. Climate List

Uncomment communities to include air temperature plots in the report:

```r
Climate_List <- c(
  "High Level",
  "Fort Nelson",
  "Hay River"
  #"Fort Liard",
  ...
)
```

Communities must exist in the ECCC climate data RDS file.

### 4. Configurable start dates

Set the start date for climate and water level plots. Adjust these as the season progresses:

```r
climate_start_month <- 03   # Start month for air temperature plots
climate_start_day <- 01     # Start day for air temperature plots
plot_start_month <- "03"    # Start month for double level / high-res plots (string)
plot_start_day <- "01"      # Start day for double level / high-res plots (string)
```

**Note:** `climate_start_month` and `climate_start_day` are numeric; `plot_start_month` and `plot_start_day` are character strings (quoted).

### 5. Weather forecast URLs

Uncomment communities and add/edit URLs to capture ECCC seven-day forecast screenshots:

```r
Weather_URLs <- c(
  "High Level" = "https://weather.gc.ca/en/location/index.html?coords=...",
  "Hay River" = "https://weather.gc.ca/en/location/index.html?coords=60.815,-115.783"
  ...
)
```

To find the URL for a new community: go to https://weather.gc.ca, search for the community, and copy the URL from the address bar.

### 6. Running the report

The script runs two main steps:

1. **Water Levels Table** – Generates and prints a summary table of current water levels, 24hr/72hr changes, historical context, and previous year levels. Review this before generating the full report.

2. **Report Generation** – Creates a Word document at `Reports/Spring_Break_Up_Report_YYYY_MM_DD_HHMM.docx`.

After generation, open the Word document and:

- Right-click the Table of Contents and select **Update Field** > **Update entire table** to populate page numbers.
- Fill in the **Current Status** bullet points under each river basin section. These are left blank for manual entry.
- Review all plots and formatting.

---

## Report structure

The generated report contains the following sections:

1. **Title page** – NWT Water Monitoring Bulletin with date/time
2. **Current Status** – General overview (manual entry)
3. **Water Levels Summary** – Auto-generated table with current levels, changes, percentiles, and historical context
4. **Table of Contents** – Auto-generated (requires Update Field in Word)
5. **River basin sections** (one per active basin):
   - Current Status (manual entry)
   - Station Map (auto-inserted based on active stations)
   - Hydrometric Data (water level hydrographs)
   - Gauge Photos (from Water Survey of Canada)
6. **Weather Data**
   - Air temperature plots (from Climate_List)
   - Weather forecast screenshots (from Weather_URLs)
7. **Factors to Watch** – Appended from template
8. **Appendix A** – River Ice Imagery (manual entry)
9. **Appendix B** – High resolution and historic water level plots (auto-generated)

---

## Basin maps

Maps are automatically inserted based on which stations are active:

| Map file | Basin section | Condition |
|----------|--------------|-----------|
| `Hay_gauges.jpg` | Hay River | Any Hay River station active |
| `Liard_gauges.jpg` | Liard River | Any Liard River station active |
| `Mack_at_FS_gauges_V2.jpg` | Mackenzie River | 10FB006, 10FB007, or 10GC001 active (no 10KA001) |
| `Mack_Sahtu_V3.png` | Mackenzie River | 10KA001 (Norman Wells) active |
| `Mack_Peel_Delta_V2.png` | Beaufort Delta | Any Beaufort Delta station active |

There is no map for the Slave River / Great Slave Lake section. If additional maps are created, they must be added 
to the get_basin_maps function for automated insertion into the report.

---

## Water Levels Table notes

- **Current Level** displays as NA if no reading has been received in the last 24 hours.
- **Percentile Range** and **Historical Context** also display as NA when current level is unavailable or when fewer than 6 years of historical data exist for the current day of year.
- **Previous year level**: The function first attempts to retrieve realtime data from the previous year. If unavailable (common for seasonal stations), it falls back to historical daily data from the HYDAT database.
- Seasonal stations (e.g., Steen River, Sousa Creek) will show warnings about missing historical data for certain days of year – this is expected.

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| `unable to find inherited method for 'select'` | A package namespace conflict. All dplyr calls in the functions file should use `dplyr::select()`, `dplyr::filter()`, etc. |
| `PhantomJS not found` | Ensure the PhantomJS path is correct in `Spring_Break_Up.R` and that `phantomjs.exe` exists in the bin folder. |
| Climate plots are blank | Check that `update_climate.R` has been run recently and that the ECCC climate data RDS contains the current year's data. |
| Plots show wrong date range | Adjust `climate_start_month`/`climate_start_day` or `plot_start_month`/`plot_start_day` in `Spring_Break_Up.R`. |
| Weather forecast screenshot is distorted | Adjust the `vheight` parameter in the `webshot::webshot()` call in `Spring_Break_Up_Functions.R`. |
| Report fails for a single station | The station may not have data available. Check the console output for error messages. The report will skip failed stations and continue. |
