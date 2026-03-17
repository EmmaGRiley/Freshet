# Define user
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/")

#set time zone
current_datetime <- with_tz(Sys.time(), tzone = "America/Edmonton")
formatted_date <- format(current_datetime, "%Y_%m_%d")
formatted_time <- format(current_datetime, "%H%M")

#required libraries
library(RSQLite)
library(tidyhydat)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(DBI)
library(waiter)
library(fasstr)
library(officer)
library(tidyhydat)
library(httr)
library(xml2)
library(ggplot2)
library(lubridate)
library(knitr)
library(rmarkdown)
library(flextable) 
library(tidyquant)
library(readxl)
library(webshot)

#Source files/functions

freshet_path <- paste0(user, "Documents/R_Scripts/Freshet/")
report_path <- paste0(freshet_path, "Reports/")
# Generate report output path
output_path <- paste0(report_path, "Spring_Break_Up_Report_", formatted_date, "_",formatted_time, ".docx")
#input last report information (for water level change table)
Last_Report <- "2025-03-10 15:00:00"

dir.create(paste0(report_path))
dir.create(paste0(freshet_path))
source(paste0(freshet_path, "/Spring_Break_Up_Functions.R"))
source(paste0(freshet_path, "/image_download_utils.R"))
source(paste0(freshet_path, "/Station_photos_lists.R"))
source(paste0(user, "Documents/R_Scripts/Functions/Workflows/nwtclimate_functions.R"))
source(paste0(user, "/R_Scripts/Functions/Workflows/update_climate.R"))

# import csv of historical peaks
peak_data <- read_excel(paste0(freshet_path, "/data/Historic_Peaks_rough.xlsx"))

# Hydrometric List - comment out stations not required

Hydrometric_List <- c(
  
  # Slave River Basin
  #"07KC001", # Peace River at Peace Point
  #"07DD001", # Athabasca River at Embarras Airport
  #"07NB001", # Slave River at Fitzgerald
  
  # Great Slave Lake
  #"07SB001", # Great Slave Lake at Yellowknife Bay
  #"07OB002", # Great Slave Lake at Hay River
  
  # Hay River Basin
  "07OB001", # Hay River at Hay River
  "07OB008", # Hay River at the border
  "07OB004", # **Seasonal** Steen River near Steen River
  "07OB006", # **Seasonal** Lutose Creek near Steen River
  "07OB003", # **Seasonal** Hay River near Meander River
  "07OC001", # **Seasonal** Chinchaga River near High Level
  "07OA001" # **Seasonal** Sousa Creek near High Level
  
  # Liard River Basin
  #"10AA006", # Liard River below Scurvy Creek
  #"10AA001", # Liard River at Upper Crossing
  #"10BE001", # Liard River at Lower Crossing,
  #"10ED001", # Liard River at Fort Liard,
  #"10EB001", # South Nahanni River above Virginia Falls
  #"10ED002", # Liard River near the mouth
  #"10DA001", # Petitot River below highway 77

  # Mackenzie River Basin
  #"10FB001", # **Seasonal** Mackenzie River near Fort Providence
  #"10FB006", # Mackenzie River at Strong Point
  #"10FB007", # Mackenzie River at JMR
  #"10GC001",  # Mackenzie River at Fort Simpson
  #"10KA001", # Mackenzie River at Norman Wells
  #"10LA002",  # Arctic Red River near the mouth
  #"10KD001", # **Seasonal** Mackenzie River at Sans Sault Rapids
  #"10LD001", # **Seasonal** Mackenzie River at Fort Good Hope
  #"10LC002", #Mackenzie River (East Channel) at Inuvik
  #"10LC014",  # Mackenzie River at Arctic Red River
  #"10LC015", #Mackenzie River at Confluence East Channel
  #"10MC003", # Mackenzie River (Peel Channel) above Aklavik
  #"10MC008", # Mackenzie River (Middle Channel) below Raymond Channel
  #"10LC019", # Mackenzie River (Kumak Channel) below Middle Channel
  #"10LC002", # Mackenzie River (East Channel) at Inuvik
  #"10MC023", # Mackenzie River (Napoiak Channel) above Shallow Bay
  
  # Peel River Basin
  #"10MA001", # Peel River above Canyon Creek
  #"10MC002", # Peel River above Fort McPherson
  #"10MC002"  # **Seasonal** Peel River at Frog Creek

)

# List all communities to generate an air temperature plot

Climate_List <- c(

  "High Level",
  "Fort Nelson",
  "Hay River"
  #"Fort Liard",
  #"Fort Smith",
  #"Fort Simpson",
  #"Sambaa Ke",
  #"Norman Wells",
  #"Fort McPherson",
  #"Inuvik",
  #"Fort Good Hope",
  #"Yellowknife",
  #"Tulita",
  #"Aklavik",
  #"Tuktoyaktuk"

)

# Configurable start dates for climate and water level plots
climate_start_month <- 03
climate_start_day <- 01
plot_start_month <- "03"   # For double level and high-res water level plots
plot_start_day <- "01"

# Weather forecast URLs - uncomment communities as needed
# These are used to capture ECCC forecast screenshots in the report
Weather_URLs <- c(
  "High Level" = "https://weather.gc.ca/en/location/index.html?coords=58.516,-117.138",
  "Fort Nelson" = "https://weather.gc.ca/en/location/index.html?coords=58.806,-122.697",
  "Hay River" = "https://weather.gc.ca/en/location/index.html?coords=60.815,-115.783"
  #"Fort Liard" = "https://weather.gc.ca/en/location/index.html?coords=60.235,-123.472",
  #"Fort Smith" = "https://weather.gc.ca/en/location/index.html?coords=60.009,-111.883",
  #"Fort Simpson" = "https://weather.gc.ca/en/location/index.html?coords=61.863,-121.35",
  #"Sambaa Ke" = "https://weather.gc.ca/en/location/index.html?coords=60.443,-121.242",
  #"Norman Wells" = "https://weather.gc.ca/en/location/index.html?coords=65.282,-126.831",
  #"Fort McPherson" = "https://weather.gc.ca/en/location/index.html?coords=67.440,-134.861",
  #"Inuvik" = "https://weather.gc.ca/en/location/index.html?coords=68.360,-133.725",
  #"Aklavik" = "https://weather.gc.ca/en/location/index.html?coords=68.222,-135.009",
  #"Tuktoyaktuk" = "https://weather.gc.ca/en/location/index.html?coords=69.444,-133.025"
)

# Add PhantomJS to PATH for webshot (weather forecast screenshots)
phantomjs_path <- paste0(user, "Documents/Modelling/phantomjs/phantomjs/bin")
Sys.setenv(PATH = paste(phantomjs_path, Sys.getenv("PATH"), sep = ";"))

#View water level change table
water_levels_table <- WL_change_table(Hydrometric_List, Last_Report)
print(water_levels_table)

#Run the report function
tryCatch({
  generate_spring_breakup_report(
    Hydrometric_List = Hydrometric_List,
    output_path = output_path,
    Climate_List = Climate_List,
    climate_start_month = climate_start_month,
    climate_start_day = climate_start_day,
    plot_start_month = plot_start_month,
    plot_start_day = plot_start_day,
    Weather_URLs = Weather_URLs
  )
  message("Report generated successfully at: ", output_path)
}, error = function(e) {
  message("Error generating report: ", e$message)
})


#Non-automated method - good for smaller reports or redoing certain elements
path <- paste0(user, "Documents/NT_Hydrology/Figures/Breakup/")
year <- paste0(lubridate::year(Sys.Date()), "/")
dir.create(paste0(path))
dir.create(paste0(path, year))
dir.create(paste0(path, year, Sys.Date()))
save_path <- paste0(path, year, Sys.Date())

# Run a for loop

for(i in Hydrometric_List) {
  tryCatch({
    if(i == "07NB001") {
      hydro_plot_dayofyear(
        parameter = "Flow",
        select_years = c(2025, 2026),
        station_number = i,
        after_bennett = T,
        save = T,
        line_colours = c("dodgerblue", "blue4","green4", "red4", "purple4", "yellow4"),
        file_name = paste0(i, " Flow"))
    } else {
      hydro_plot_dayofyear(
    parameter = "Flow",
    select_years = c(2025, 2026),
    station_number = i,
    save = T,
    file_name = paste0(i, " Flow"))
    }
    if(i == "07SB001") {
      hydro_plot_dayofyear(
        parameter = "Level",
        select_years = c(2025, 2026),
        station_number = i,
        after_bennett = T,
        save = T,
        line_colours = c("dodgerblue", "blue4","green4", "red4", "purple4", "yellow4"),
        file_name = paste0(i, " Level"))
    } else {
      hydro_plot_dayofyear(
        parameter = "Level",
        select_years = c(2025, 2026),
        station_number = i,
        save = T,
        line_colours = c("dodgerblue", "blue4","green4", "red4", "purple4", "yellow4"),
        file_name = paste0(i, " Level"))
    }

    Double_Level_Plot(
      station_number = i,
      station_title = FALSE,
      start_month = climate_start_month,
      start_day = climate_start_day,
      end_month = lubridate::month(Sys.Date()),
      end_day = lubridate::day(Sys.Date()))

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }

# For loop for Climate stations (air temperature at ECCC airport stations)

directory = paste0(user, "Documents/R_Scripts/Packages/nwtclimate/data/")
merged_data = "ECCC_Climate_Data_Merged"
cd_rds <- readRDS(paste0(directory, merged_data))

cd_rds <- readRDS(paste0(data_path, merged_data, ".rds"))

for(i in Climate_List) {
  tryCatch({

    Temps(site = i,
          ymin = -40,
          ymax = 30,
          start_month = climate_start_month,
          start_day = climate_start_day,
          end_month = lubridate::month(Sys.Date()),
          end_day = lubridate::day(Sys.Date()))

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

                    
###################################################################################################

# Run each individual functions below:

###################################################################################################

Flows(
  station_number = "07OB008", 
  select_years = c(2025, 2026),
  log_scale = F,
  cumulative = F,
  line_colour_1 = "dodgerblue", 
  line_colour_2 = "blue4", 
  line_colour_3 = "black",
  line_size = 0.5,
  point_size = 0.5,
  plot_height = 10,
  plot_width = 16
  )

##################################################################################################

test<- Levels(
  station_number = "07OB001",
  select_years = c(2025, 2026), 
  line_colour_1 = "dodgerblue", 
  line_colour_2 = "blue4",
  line_colour_3 = "green4",
  plot_height = 10,
  plot_width = 16, 
  GSL_after_1972 = F
  
)

plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year")) 

p <- plotly::plotly_build(test)

p


###################################################################################################

Double_Level_Plot(
  station_number = "07OB008",
  station_title = FALSE,
  start_month = "03",
  start_day = "01",
  end_month = "03", 
  end_day = "15"
)

###################################################################################################

Flows_HighRes(
  station_number = "10EB001",
  previous_days = 20,
  plot_width = 16, 
  plot_height = 10,
  start_date = as.Date("2025-07-08"),
  end_date = as.Date("2025-07-13")
)

###################################################################################################

  # Levels_HighRes(
  # station_number = "10LD001",
  # previous_days = 53, 
  # y_min = 0,
  # y_max = 12,
  # plot_width = 18, 
  # plot_height = 11
  # )

####################################


test<- Levels.HighRes2(station_number = "10EB001",
                       y_min = NA,
                       y_max = NA,
                       plot_width = 18,
                       plot_height = 10,
                       start_date = as.Date("2025-07-08"),
                       end_date = as.Date("2025-07-13"))

plotly::ggplotly(test) %>%
  plotly::ggplotly(tooltip = c("y", "x", "Year")) 

p <- plotly::plotly_build(test)

p

###########################################

###################################################################################################

AirTemp_HighRes(
  station_number = "10ED001", 
  previous_days = 15
)

###################################################################################################

Temps(
  
  site = "Fort Simpson",
  variable = "mean",
  select_year = 2025,
  start_year = 1991,
  end_year = 2024,
  ymin = -30, 
  ymax = 30,
  start_month = 04,
  start_day = 01, 
  end_month = 05, 
  end_day = 31,
  percentile_plot = T,
  line_colour = "black",
  legend_position = "none",
  line_size = 0.5,
  point_size = 0.5,
  plot_width = 16, 
  plot_height = 10, 
  
)

###################################################################################################

  



