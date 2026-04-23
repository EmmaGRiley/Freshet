###################################################################################################

# Load tidyhydat library


library(tidyhydat)

###################################################################################################

# File path to save plots
user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")), "/")
savepath_Hydrometric <- paste0(user, "Documents/NT_Hydrology/Figures")
savepath_Hydrometric_data <- paste0(user, "Documents/NT_Hydrology/csv_files")

###################################################################################################

# Leap year function

is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

###################################################################################################

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

###################################################################################################

# Function for water year

wtr_yr <- function(dates, 
                   start.month) 
{
  dates.posix = as.POSIXlt(dates)
  offset = ifelse(dates.posix$mon >= start.month - 1, 1, 0)
  adj.year = dates.posix$year + 1900 + offset
  adj.year
}

###################################################################################################

wtr_yr_rv <- function(dates, 
                      start.month) 
{
  dates.posix = as.POSIXlt(dates)
  offset = ifelse(dates.posix$mon >= start.month - 1, 1, 0)
  adj.year = dates.posix$year + 1900 - offset
  adj.year
}

###################################################################################################

# Function to plot flows

Flows <- function(
  station_number = "07OB001",
  #data = read.csv("07OB001_to_append.csv"),
  select_years = c(2025, 2026),
  GSL_after_1972 = FALSE,
  export_csv = FALSE,
  cumulative = FALSE,
  historic = TRUE,
  log_scale = FALSE,
  percentile_plot = FALSE,
  colour_ramp = rainbow,
  line_colour_1 = "dodgerblue",
  line_colour_2 = "blue4",
  line_colour_3 = "black",
  legend_position = "top",
  line_size = 0.5,
  point_size = 0.5,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  plot_width = 18, 
  plot_height = 11,
  dpi = 300, 
  file_name = NA)

{
  station <- tidyhydat::hy_stations(station_number)
  leap.list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
    level.historic <- (tidyhydat::hy_daily_flows(station_number = station_number)
                       [,-c(3,5)])
    colnames(level.historic) <- c("STATION_NUMBER", "Date", "Level")
    
    if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
      
      level.real.time <- tidyhydat::realtime_ws(station_number = station_number, 
                                                   parameters = 47, 
                                                   start_date = as.Date(ifelse(max(lubridate::year(level.historic$Date)) == lubridate::year(Sys.Date() - 365), # This 365 may need to change to 730
                                                                       paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), 
                                                                       paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-"))),
                                                   end_date = as.Date(ifelse(lubridate::year(Sys.Date()) > max(select_years), 
                                                                     paste(max(select_years), "12", "31", sep = "-"),
                                                                     paste(Sys.Date()))))
      
      level.real.time <- level.real.time %>%
        dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
        dplyr::summarize(Date = mean(lubridate::date(Date)),
                         Level = mean(Value))
      level.real.time <- level.real.time[,-c(2,3)]
      
      # Need to add NaN for blank days first
      
      level.df <- dplyr::bind_rows(level.historic, level.real.time)
      
    } else {
      level.df <- level.historic
    }

  if(GSL_after_1972 == T) {
    level.df <- level.df %>%
      dplyr::filter(lubridate::year(Date) >= 1972)
  }
  
  # Add rows of missing dates
  level.df <- fasstr::fill_missing_dates(data = level.df, dates = "Date", value = "Level", pad_ends = F)
  
  # Remove Feb. 29 data
  
  level.df <- level.df[!(format(level.df$Date,"%m") == "02" & format(level.df$Date, "%d") == "29"), , drop = FALSE]
  
  if(export_csv == TRUE) {
    level.df
  } else {
    
    # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
    # Calculate percentiles (IQR, max/min)
    
    level.df <- level.df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap.list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date))) %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(prctile = (ecdf(Level)(Level)) * 100,
                    Max = max(Level, na.rm = TRUE),
                    Min = min(Level, na.rm = TRUE),
                    QP90 = quantile(Level, 0.90, na.rm = TRUE),
                    QP75 = quantile(Level, 0.75, na.rm = TRUE),
                    QP50 = quantile(Level, 0.50, na.rm = TRUE),
                    QP25 = quantile(Level, 0.25, na.rm = TRUE),
                    QP10 = quantile(Level, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    # Create a cumulative runoff column
    
    newcolumn <- level.df[,3] * 60 * 60 * 24
    cumlevel.df <- level.df
    cumlevel.df[,3] <- newcolumn
    
    # Run for loops to only remove NA's from data in years within select_years
    
    new.df <- data.frame()
    
    for(i in select_years) {
      cumlevel.new <- dplyr::filter(cumlevel.df, lubridate::year(cumlevel.df$Date) == i)
      cumlevel.new$Level[is.na(cumlevel.new$Level)] <- 0.0 # Replace NAs with 0s (helps with cumulative function)
      cumlevel.new$Level <- replace(cumlevel.new$Level, cumlevel.new$Date >= Sys.Date(), NA) # Replace 0s with NAs for all dates in the future
      new.df <- dplyr::bind_rows(new.df, cumlevel.new)
    }
    
    for(i in select_years){
      cumlevel.df <- cumlevel.df %>%
        dplyr::filter(lubridate::year(Date) != i)
    }
    
    cumlevel.df <- dplyr::bind_rows(cumlevel.df, new.df)
    
    
    cumlevel.df <- cumlevel.df %>%
      dplyr::mutate(Year = lubridate::year(Date)) %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(cum_level = cumsum(Level)) %>%
      dplyr::group_by(dayofyear) %>%
      dplyr::mutate(Max = max(cum_level, na.rm = TRUE),
                    Min = min(cum_level, na.rm = TRUE),
                    QP90 = quantile(cum_level, 0.90, na.rm = TRUE),
                    QP75 = quantile(cum_level, 0.75, na.rm = TRUE),
                    QP50 = quantile(cum_level, 0.50, na.rm = TRUE),
                    QP25 = quantile(cum_level, 0.25, na.rm = TRUE),
                    QP10 = quantile(cum_level, 0.10, na.rm = TRUE)) %>%
      subset(Year == select_years)

    # Find most recent complete year on dataset to use as IQR and max/min year
    
    complete.year <- level.df %>%
      dplyr::group_by(lubridate::year(Date)) %>%
      dplyr::summarize(n = length(Level))
    colnames(complete.year)[1] <- "Year"
    length.complete.year <- max(complete.year$n)
    complete.year <- complete.year %>%
      subset(n == length.complete.year)
    complete.year <- max(complete.year$Year)
    
    # Creata a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
    
    dummy_year_cum <- cumlevel.df %>%
      subset(lubridate::year(Date) == complete.year) %>%
      dplyr::select(-Level, -cum_level, -Year) %>%
      dplyr::mutate(cum_level = as.numeric(NA ),
                    Year_Real = NA,
                    prctile = NA) %>%
      dplyr::rename(Value = cum_level)
    
    dummy_year <- level.df %>%
      subset(lubridate::year(Date) == complete.year) %>%
      dplyr::select(-Level) %>%
      dplyr::mutate(Level = as.numeric(NA ),
                    Year_Real = NA,
                    prctile = NA) 
    
    # Create a blank data frame that will be filled with the for loop
    
    plot_years <- data.frame()
    plot_years_cum <- data.frame()
    
    # For loop to populate plot_years with data from each year in select_years
    
    for(i in select_years) {
      
      single_year <- level.df %>%
        subset(lubridate::year(Date) == i) %>%
        dplyr::mutate(Year = complete.year,
                      Month = lubridate::month(Date),
                      Day = lubridate::day(Date), 
                      Year_Real = i) %>%
        dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
        dplyr::select(-Date, -Month, -Day, -Year) %>%
        dplyr::rename(Date = Date_2,
                      Value = Level)
      
      single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
      plot_years <- dplyr::bind_rows(plot_years, single_year)
      
    }
    
    for(i in select_years) {
      
      single_year <- cumlevel.df %>%
        subset(Year == i) %>%
        dplyr::mutate(YearNew = complete.year,
                      Month = lubridate::month(Date),
                      Day = lubridate::day(Date), 
                      Year_Real = i) %>%
        dplyr::mutate(Date_2 = as.Date(paste(YearNew, Month, Day, sep = "-"))) %>%
        dplyr::select(-Date, -Month, -Day, -YearNew, -Level, -Year) %>%
        dplyr::rename(Date = Date_2,
                      Value = cum_level)
      
      single_year <- single_year[,c(1, 13, 2, 3:12)]
      plot_years_cum <- dplyr::bind_rows(plot_years_cum, single_year)
      
    }
    
    all.data.cum <- dplyr::bind_rows(plot_years_cum, dummy_year_cum) %>%
      dplyr::select(-dayofyear)
    
    legend_length_cum <- all.data.cum %>%
      dplyr::group_by(Year_Real) %>%
      dplyr::summarize(Mean = mean(Value))
    legend_length_cum <- length(legend_length_cum$Year_Real) - 1 # The '-1' accounts for the NA column
    all.data.cum$Year_Real <- as.numeric(all.data.cum$Year_Real)
    
    # Normal Data
    
    all.data <- dplyr::bind_rows(plot_years, dummy_year) %>%
      dplyr::select(-dayofyear, -Level)
    all.data <- all.data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
    
    # Code for number of factors in the legend
    
    legend_length <- all.data %>%
      dplyr::group_by(Year_Real) %>%
      dplyr::summarize(Mean = mean(Value))
    legend_length <- length(legend_length$Year_Real) - 1 # The '-1' accounts for the NA column
    all.data$Year_Real <- as.numeric(all.data$Year_Real)
    
    # Code to bold a specific year. Need to change the year in the first and fourth lines
    
    #adj_names = sort(setdiff(unique(new.data$Year), "2022"))
    #values = gg_color_hue(length(adj_names))
    #names(values) = adj_names
    #values = c(values, c("2022" = "black"))
    
    # Plot the graph
    
    if(cumulative == TRUE) {
      plot <- ggplot2::ggplot(all.data.cum, ggplot2::aes(x = Date, y = Value)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "IQR")) +
        ggplot2::scale_fill_manual(name = "", 
                                   values = c("Min - Max" = "gray85",
                                              "IQR" = "gray75")) +
        ggplot2::theme_classic() +
        ggplot2::geom_point(ggplot2::aes(y = QP75), shape = 19, colour = "gray70", size = 0.5) +
        ggplot2::geom_line(ggplot2::aes(y = QP75), colour = "gray70") +
        ggplot2::geom_point(ggplot2::aes(y = QP25), shape = 19, colour = "gray70", size = 0.5) +
        ggplot2::geom_line(ggplot2::aes(y = QP25), colour = "gray70") +
        ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                      # subtitle = "Flows",
                      x = "Month", y = expression(paste("Cumulative Discharge (m"^3,")"))) +
        ggplot2::scale_x_date(date_breaks = "1 months",
                              labels = scales::date_format("%b")) +
        tidyquant::coord_x_date(xlim = c(paste(complete.year, "-01-01", sep = ""),
                                         paste(complete.year, "-12-31", sep = ""))) +
        ggplot2::geom_point(colour = line_colour_1, shape = 19, size = point_size) + 
        ggplot2::geom_line(colour = line_colour_1, linewidth = line_size) +
        ggplot2::scale_colour_manual(name = "", 
                                     values = colour_ramp(legend_length),
                                     na.translate = FALSE) +
        ggplot2::theme(legend.position = legend_position)
      
      if (log_scale == TRUE) {
        plot <- plot +
          ggplot2::scale_y_continuous(trans = 'log10')
      } else if (log_scale == FALSE) {
        plot <- plot +
          ggplot2::scale_y_continuous()
      }
      
      if(is.na(file_name)) {
        
        ggplot2::ggsave(paste(station$STATION_NUMBER, min(select_years), "Flow.png", sep="_"), plot = plot, device = "png",
                        path = savepath_Hydrometric,
                        scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
        
        plot
        
      } else {
        
        ggplot2::ggsave(file_name, plot = plot, device = "png",
                        path = savepath_Hydrometric,
                        scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
        
        plot
        
      }
      
    } else {
      
      plot <- ggplot2::ggplot(all.data, ggplot2::aes(x = Date, y = Value)) + 
        ggplot2::theme_classic() +
        ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                      # subtitle = "Flows",
                      x = "Month", y = expression(paste("Discharge (m"^3, " s"^-1,")"))) +
        ggplot2::scale_x_date(date_breaks = "1 months",
                              labels = scales::date_format("%b")) +
        tidyquant::coord_x_date(xlim = c(paste(complete.year, "-01-01", sep = ""),
                                         paste(complete.year, "-12-31", sep = ""))) +
        ggplot2::theme(legend.position = legend_position,
                       legend.text = ggplot2::element_text(size = legend_text_size))
      
      if (historic == TRUE) {
        plot <- plot +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Average Range")) +
          ggplot2::scale_fill_manual(name = "", 
                                     values = c("Min - Max" = "gray85",
                                                "Average Range" = "gray75")) 
      }
      
      if (percentile_plot == TRUE && length(select_years) == 1) {
        plot <- plot + 
          ggplot2::geom_point(ggplot2::aes(colour = prctile), shape = 19, size = point_size) +
          ggplot2::geom_line(data= all.data, ggplot2::aes(colour = prctile, group = Year_Real), linewidth = line_size) +
          ggplot2::scale_colour_gradientn(name = paste0(select_years,
                                                        "\nPercentiles"),
                                          limits = c(0, 100),
                                          colours = rainbow(3)) +
          ggplot2::theme(legend.position = legend_position,
                         legend.text = ggplot2::element_text(size = 10))
        
      } else {
        
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(colour = factor(Year_Real)), shape = 19, size = point_size) + 
          ggplot2::geom_line(ggplot2::aes(colour = factor(Year_Real)), linewidth = line_size) +
          # Turn the following lines on (and turn off the line above) to bold a certain year. Need to change the year as well
          #ggplot2::geom_line(ggplot2::aes(colour = factor(Year_Real)), linewidth = line_size) +
          #ggplot2::geom_line(data = all.data[all.data$Year_Real == 2022,], linewidth = line_size + 1.5) +
          ggplot2::scale_colour_manual(name = "", 
                                       values = colour_ramp(legend_length),
                                       na.translate = FALSE) # eliminates 'NA' from legend
      }
      
      if (percentile_plot == FALSE && legend_length == 1) {
        plot <- plot +
          ggplot2::scale_colour_manual(name = "", 
                                       values = line_colour_1,
                                       na.translate = FALSE)
      }
      
      if (percentile_plot == FALSE && legend_length == 2) {
        plot <- plot +
          ggplot2::scale_colour_manual(name = "", 
                                       values = c(line_colour_1, line_colour_2),
                                       na.translate = FALSE)
      }
      
      if (log_scale == TRUE) {
        plot <- plot +
          ggplot2::scale_y_continuous(trans = 'log10')
      } else if (log_scale == FALSE) {
        plot <- plot +
          ggplot2::scale_y_continuous()
      }
      
      if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
        plot <- plot + 
          ggplot2::ylim(y_min, y_max) +
          ggplot2::geom_hline(yintercept = 6700, linetype = "dashed") +
          ggplot2::geom_hline(yintercept = 1300, linetype = "dashed")
      }
      
      if(percentile_plot == T && length(select_years) > 1) {
        print("ERROR: percentile_plot is only able to present one year of data. Reduce select_years to one year")
      } else {
        
        if(is.na(file_name)) {
          
          ggplot2::ggsave(paste0(station$STATION_NUMBER, "_", Sys.Date(), "_Flow.png"),
                          plot = plot, device = "png",
                          path = savepath_Hydrometric,
                          scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
          
          plot
          
        } else {
          
          ggplot2::ggsave(file_name, plot = plot, device = "png",
                          path = savepath_Hydrometric,
                          scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
          
          plot
          
        }
      }
      
    }
    
  }
  
}

###################################################################################################

# Function to plot levels

Levels <- function(
  station_number = "10MC002",
  select_years = c(2025, 2026),
  GSL_after_1972 = F,
  historic = TRUE,
  percentile_plot = FALSE,
  level_masl = FALSE,
  colour_ramp = rainbow,
  line_colour_1 = "dodgerblue",
  line_colour_2 = "blue4",
  line_colour_3 = "grey",
  legend_position = "top",
  line_size = 0.5,
  point_size = 0.5,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  plot_width = 18, 
  plot_height = 11,
  dpi = 300, 
  file_name = NA)

{
  station <- tidyhydat::hy_stations(station_number)
  leap.list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  level.historic <- (tidyhydat::hy_daily_levels(station_number = station_number)
                     [,-c(3,5)])
  colnames(level.historic) <- c("STATION_NUMBER", "Date", "Level")

  if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
    
    level.real.time <- tidyhydat::realtime_ws(station_number = station_number,
                                                 parameters = 46, 
                                                 start_date = as.Date(ifelse(max(lubridate::year(level.historic$Date)) == lubridate::year(Sys.Date() - 365), # This 365 may need to change to 730
                                                                     paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), 
                                                                     paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-"))),
                                                 end_date = as.Date(ifelse(lubridate::year(Sys.Date()) > max(select_years), 
                                                                   paste(max(select_years), "12", "31", sep = "-"),
                                                                   paste(Sys.Date()-1))))
    
    level.real.time <- level.real.time %>%
      dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(Date)),
                       Level = mean(Value))
    level.real.time <- level.real.time[,-c(2,3)]
    
    # Need to add NaN for blank days first
    
    level.df <- dplyr::bind_rows(level.historic, level.real.time)
    
  } else {
    level.df <- level.historic
  }
  
  # Add rows of missing dates
  level.df <- fasstr::fill_missing_dates(data = level.df, dates = "Date", pad_ends = F) 
  
  # Remove Feb. 29 data
  
  level.df <- level.df[!(format(level.df$Date,"%m") == "02" & format(level.df$Date, "%d") == "29"), , drop = FALSE]
  
  # Add a column for masl:
  if(level_masl == T) {
    conversion <- as.numeric(tidyhydat::hy_stn_datum_conv(station_number = station_number)[,4])
    level.df <- level.df %>%
      dplyr::mutate(Level = Level + conversion)
  }
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  
  level.df <- level.df %>%
    dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap.list, 
                                     ifelse(lubridate::month(Date) <= 2,
                                            lubridate::yday(Date),
                                            lubridate::yday(Date) - 1),
                                     lubridate::yday(Date))) %>%
    dplyr::group_by(dayofyear) %>%
    dplyr::mutate(prctile = (ecdf(Level)(Level)) * 100,
                  Max = max(Level, na.rm = TRUE),
                  Min = min(Level, na.rm = TRUE),
                  QP90 = quantile(Level, 0.90, na.rm = TRUE),
                  QP75 = quantile(Level, 0.75, na.rm = TRUE),
                  QP50 = quantile(Level, 0.50, na.rm = TRUE),
                  QP25 = quantile(Level, 0.25, na.rm = TRUE),
                  QP10 = quantile(Level, 0.10, na.rm = TRUE))
  
  # Find most recent complete year on dataset to use as IQR and max/min year
  
  complete.year <- level.df %>%
    dplyr::group_by(lubridate::year(Date)) %>%
    dplyr::summarize(n = length(Level))
  colnames(complete.year)[1] <- "Year"
  length.complete.year <- max(complete.year$n)
  complete.year <- complete.year %>%
    subset(n == length.complete.year)
  complete.year <- max(complete.year$Year)
  
  # Creata a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
  
  dummy_year <- level.df %>%
    subset(lubridate::year(Date) == complete.year) %>%
    dplyr::select(-Level) %>%
    dplyr::mutate(Level = as.numeric(NA ),
                  Year_Real = NA,
                  prctile = NA) 
  
  # Create a blank data frame that will be filled with the for loop
  
  plot_years <- data.frame()
  
  # For loop to populate plot_years with data from each year in select_years
  
  for(i in select_years) {
    
    single_year <- level.df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = complete.year,
                    Month = lubridate::month(Date),
                    Day = lubridate::day(Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
      dplyr::select(-Date, -Month, -Day, -Year) %>%
      dplyr::rename(Date = Date_2,
                    Value = Level)
    
    single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
    plot_years <- dplyr::bind_rows(plot_years, single_year)
    
  }
  
  all.data <- dplyr::bind_rows(plot_years, dummy_year) %>%
    dplyr::select(-dayofyear, -Level)
  all.data <- all.data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
  
  # Code for number of factors in the legend
  
  legend_length <- all.data %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::summarize(Mean = mean(Value))
  legend_length <- length(legend_length$Year_Real) - 1 # The '-1' accounts for the NA column
  all.data$Year_Real <- as.numeric(all.data$Year_Real)

  plot <- ggplot2::ggplot(all.data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                  # subtitle = "Water Levels",
                  x = "Month", y = "Water Level (m)") +
    ggplot2::scale_x_date(date_breaks = "1 months",
                          labels = scales::date_format("%b")) +
    tidyquant::coord_x_date(xlim = c(paste(complete.year, "-01-01", sep = ""),
                                     paste(complete.year, "-12-31", sep = ""))) +
    ggplot2::theme(legend.position = legend_position,
                   legend.text = ggplot2::element_text(size = legend_text_size)) +
    ggplot2::scale_y_continuous()
  
  if (historic == TRUE) {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
      #ggplot2::geom_ribbon(ggplot2::aes(ymin = QP10, ymax = QP90, fill = "10th - 90th Percentiles")) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Average Range")) +
      ggplot2::scale_fill_manual(name = "", 
                                 values = c("Min - Max" = "gray85",
                                            #"10th - 90th Percentiles" = "grey80",
                                            "Average Range" = "gray75")) +
      ggplot2::theme(legend.position = legend_position,
                     legend.title = ggplot2::element_text(size = legend_text_size),
                     legend.text = ggplot2::element_text(size = legend_text_size)) #+

  }
  
  if (percentile_plot == TRUE && length(select_years) == 1) {
    plot <- plot + 
      ggplot2::geom_point(ggplot2::aes(colour = prctile), shape = 19, size = point_size) +
      ggplot2::geom_line(data = all.data, ggplot2::aes(colour = prctile, group = Year_Real), linewidth = line_size) +
      ggplot2::scale_colour_gradientn(name = paste0(select_years,
                                                    "\nPercentiles"),
                                      limits = c(0, 100),
                                      colours = rainbow(3)) +
      ggplot2::theme(legend.position = legend_position)
    
  } else {
    
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(colour = factor(Year_Real)), shape = 19, size = point_size) + 
      ggplot2::geom_line(ggplot2::aes(colour = factor(Year_Real)), linewidth = line_size) +
      ggplot2::scale_colour_manual(name = "", 
                                   values = colour_ramp(legend_length),
                                   na.translate = FALSE) # eliminates 'NA' from legend
  }
  
  if (percentile_plot == FALSE && legend_length == 1) {
    plot <- plot +
      ggplot2::scale_colour_manual(name = "", 
                                   values = line_colour_1,
                                   na.translate = FALSE)
  }
  
  if (percentile_plot == FALSE && legend_length == 2) {
    plot <- plot +
      ggplot2::scale_colour_manual(name = "", 
                                   values = c(line_colour_1, line_colour_2),
                                   na.translate = FALSE)
  }
  
  if (percentile_plot == FALSE && legend_length == 3) {
    plot <- plot +
      ggplot2::scale_colour_manual(name = "", 
                                   values = c(line_colour_1, line_colour_2, line_colour_3),
                                   na.translate = FALSE)
  }
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max) #+ 
  }
  
  if(station_number == "07OB008") {
    plot <- plot +
      ggplot2::ylim(284, 291)
  }
  
  if(station_number == "10PA001") {
    plot <- plot +
      ggplot2::ylim(6, 7.4)
  }
  
  if(station_number == "10MC008") {
    plot <- plot +
      ggplot2::labs(title = "MACKENZIE RIVER BELOW RAYMOND CHANNEL (10MC008)", 
                    x = "Month", y = "Water Level (m)")
  }
  
  if(is.na(file_name)) {
    
    ggplot2::ggsave(paste0(station$STATION_NUMBER, "_", Sys.Date(), "_Level.png"),
                    plot = plot, device = "png",
                    path = savepath_Hydrometric,
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
    
    plot
    
  } else {
    
    ggplot2::ggsave(file_name, plot = plot, device = "png",
                    path = savepath_Hydrometric,
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
    
    plot
    
  }
  
}

###################################################################################################
#Levels high resolution function - version 2

Levels.HighRes2 <- function(station_number,
                            start_date = start_date,
                            end_date = end_date,
                            station_title = F,
                            GSL_after_1972 = F,
                            legend_position = "none",
                            line_size = 0.5,
                            point_size = 0.5,
                            legend_text_size = 8,
                            y_min = NA,
                            y_max = NA,
                            plot_width = 19, 
                            plot_height = 13,
                            dpi = 300)
{
  station <- tidyhydat::allstations %>%
    subset(STATION_NUMBER == station_number)
  
  flow.real <- tidyhydat::realtime_ws(station_number = station_number, 
                                      parameters = 46, 
                                      start_date = start_date,
                                      end_date = end_date)
  
  flow.real[,2] <- flow.real[,2] - (7*60*60) # Removes seven hours to convert to MT
  #flow.real$Date <- as.Date(flow.real$Date)
  
  plot <- ggplot2::ggplot(flow.real, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::geom_point(size = point_size) +
    #ggplot2::geom_line(linewidth = line_size) +
    ggplot2::theme_classic() 
  #ggplot2::labs(#title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
  #subtitle = "High Resolution Water Level Data",
  #x = "Date", y = "Water Level (m)")
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max)
  }
  
  plot
  
}

#########################################################################################################

Double_Level_Plot <- function(
  
  station_number = "07OB008",
  select_years = c(1900:paste0(lubridate::year(Sys.Date()))),
  start_month = "04",
  start_day = "01",
  end_month = "05", 
  end_day = "15",
  station_title = F,
  GSL_after_1972 = F,
  legend_position = "none",
  line_size = 0.5,
  point_size = 0.5,
  legend_text_size = 8,
  y_min = NA,
  y_max = NA,
  plot_width = 19, 
  plot_height = 13,
  dpi = 300
  
)

{
  Levels.HighRes2 <- function(station_number,
                              y_min = NA,
                              y_max = NA,
                              plot_width = 18,
                              plot_height = 10)
  {
    station <- tidyhydat::allstations %>%
      subset(STATION_NUMBER == station_number)

    flow.real <- tryCatch({
      tidyhydat::realtime_ws(station_number = station_number,
                             parameters = 46,
                             start_date = as.Date(paste(lubridate::year(Sys.Date()), start_month, start_day, sep = "-")),
                             end_date = Sys.Date())
    }, error = function(e) {
      message("Note: No high-res realtime data for ", station_number, ": ", e$message)
      return(NULL)
    })

    if(is.null(flow.real) || nrow(flow.real) == 0) {
      return(NULL)
    }

    flow.real[,2] <- flow.real[,2] - (7*60*60) # Removes seven hours to convert to MT

    plot <- ggplot2::ggplot(flow.real, ggplot2::aes(x = Date, y = Value)) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::theme_classic()

    if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
      plot <- plot +
        ggplot2::ylim(y_min, y_max)
    }

    plot

  }
  
  station <- tidyhydat::hy_stations(station_number)
  leap.list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  level.historic <- (tidyhydat::hy_daily_levels(station_number = station_number)
                     [,-c(3,5)])
  colnames(level.historic) <- c("STATION_NUMBER", "Date", "Level")
  
  if (max(select_years) >= lubridate::year(Sys.Date() - 730)) {
    
    level.real.time <- tidyhydat::realtime_ws(station_number = station_number,
                                                 parameters = 46, 
                                                 start_date = as.Date(ifelse(max(lubridate::year(level.historic$Date)) == lubridate::year(Sys.Date() - 365),
                                                                     paste(paste(lubridate::year(Sys.Date() - 365)), "01", "01", sep = "-"), 
                                                                     paste(paste(lubridate::year(Sys.Date() - 730)), "01", "01", sep = "-"))),
                                                 end_date = as.Date(ifelse(lubridate::year(Sys.Date()) > max(select_years), 
                                                                   paste(max(select_years), "12", "31", sep = "-"),
                                                                   paste(Sys.Date()))))
    
    level.real.time <- level.real.time %>%
      dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(Date)),
                       Level = mean(Value))
    level.real.time <- level.real.time[,-c(2,3)]
    
    # Need to add NaN for blank days first
    
    level.df <- dplyr::bind_rows(level.historic, level.real.time)
    
  } else {
    level.df <- level.historic
  }
  
  # Add rows of missing dates
  #level.df <- fasstr::fill_missing_dates(data = level.df, dates = "Date", value = "Level")
  
  # Remove Feb. 29 data
  
  level.df <- level.df[!(format(level.df$Date,"%m") == "02" & format(level.df$Date, "%d") == "29"), , drop = FALSE]
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  
  level.df <- level.df %>%
    dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap.list, 
                                     ifelse(lubridate::month(Date) <= 2,
                                            lubridate::yday(Date),
                                            lubridate::yday(Date) - 1),
                                     lubridate::yday(Date))) #%>%
  #dplyr::group_by(dayofyear) %>%
  #dplyr::mutate(prctile = (ecdf(Level)(Level)) * 100,
  #Max = max(Level, na.rm = TRUE),
  #Min = min(Level, na.rm = TRUE),
  #QP90 = quantile(Level, 0.90, na.rm = TRUE),
  #QP75 = quantile(Level, 0.75, na.rm = TRUE),
  #QP50 = quantile(Level, 0.50, na.rm = TRUE),
  #QP25 = quantile(Level, 0.25, na.rm = TRUE),
  #QP10 = quantile(Level, 0.10, na.rm = TRUE))
  
  # Find most recent complete year on dataset to use as IQR and max/min year
  
  complete.year <- level.df %>%
    dplyr::group_by(lubridate::year(Date)) %>%
    dplyr::summarize(n = length(Level))
  colnames(complete.year)[1] <- "Year"
  length.complete.year <- max(complete.year$n)
  complete.year <- complete.year %>%
    subset(n == length.complete.year)
  complete.year <- max(complete.year$Year)
  
  # Creata a 'dummy_year' data frame that will contain IQR, max/min for the most recent complete year
  
  dummy_year <- level.df %>%
    subset(lubridate::year(Date) == complete.year) %>%
    dplyr::select(-Level) %>%
    dplyr::mutate(Level = as.numeric(NA ),
                  Year_Real = NA,
                  prctile = NA) 
  
  # Create a blank data frame that will be filled with the for loop
  
  plot_years <- data.frame()
  
  # For loop to populate plot_years with data from each year in select_years
  
  for(i in select_years) {
    
    single_year <- level.df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = complete.year,
                    Month = lubridate::month(Date),
                    Day = lubridate::day(Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
      dplyr::select(-Date, -Month, -Day, -Year) %>%
      dplyr::rename(Date = Date_2,
                    Value = Level)
    
    single_year <- single_year[,c(1,5,3,2,4)]
    plot_years <- dplyr::bind_rows(plot_years, single_year)
    
  }
  
  dummy_year <- dummy_year[,c(1:5)]
  dummy_year <- dummy_year %>%
    dplyr::rename(Value = Level)
  
  all.data <- dplyr::bind_rows(plot_years, dummy_year) %>%
    dplyr::select(-dayofyear)
  #all.data <- all.data[,c(1, 2, 12, 11, 3, 4, 6:10, 5)]
  
  # Code for number of factors in the legend
  
  legend_length <- all.data %>%
    dplyr::group_by(Year_Real) %>%
    dplyr::summarize(Mean = mean(Value))
  legend_length <- length(legend_length$Year_Real) - 1 # The '-1' accounts for the NA column
  all.data$Year_Real <- as.numeric(all.data$Year_Real)
  
  new.data <- all.data %>%
    dplyr::filter(Date >= paste(complete.year, start_month, start_day, sep = "-")) %>%
    dplyr::filter(Date <= paste(complete.year, end_month, end_day, sep = "-"))
  
  # Color generating function found here:
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  
  # Dynamically generate default color values, but have 2026 =" black".
  
  adj_names = sort(setdiff(unique(new.data$Year), "2026"))
  values = gg_color_hue(length(adj_names))
  names(values) = adj_names
  values = c(values, c("2026" = "black"))
  
  if(length(unique(new.data$Year_Real)) <= 25) {
    legend_number <- 1
  } else if(length(unique(new.data$Year_Real)) <= 50) {
    legend_number <- 2
  } else if(length(unique(new.data$Year_Real)) <= 60) {
    legend_number <- 3
  } else if(length(unique(new.data$Year_Real)) <= 80) {
    legend_number <- 4
  } else if(length(unique(new.data$Year_Real)) <= 100) {
    legend_number <- 5
  } else {
    legend_number = 6
  }
  
  # Plot the graph
  
  plot <- ggplot2::ggplot(new.data, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::theme_classic() +
    ggplot2::labs(subtitle = "Historic Daily Water Levels",
                  x = "Date",
                  y = "Water Level (m)") +
    ggplot2::scale_x_date(date_breaks = "1 month",
                          labels = scales::date_format("%b %d")) +
    tidyquant::coord_x_date(xlim = c(paste(complete.year, start_month, start_day, sep = "-"),
                                     paste(complete.year, end_month, end_day, sep = "-"))) +
    ggplot2::theme(legend.position = legend_position,
                   legend.text = ggplot2::element_text(size = legend_text_size)) +
    ggplot2::scale_y_continuous() +
    #ggplot2::geom_point(ggplot2::aes(colour = factor(Year_Real)), shape = 19, size = point_size) + 
    ggplot2::geom_line(ggplot2::aes(colour = factor(Year_Real)), linewidth = line_size) +
    ggplot2::geom_line(data = new.data[new.data$Year_Real == 2026,], linewidth = line_size + 1) +
    ggplot2::scale_colour_manual(name = "", 
                                 values = values,
                                 na.translate = FALSE) # eliminates 'NA' from legend
  
  if(legend_position == "right") {
    plot <- plot +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = legend_number, bycol = TRUE)) # format legend to be two columns
  }
  
  if(legend_position == "top") {
    plot <- plot +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = legend_number, byrow = TRUE)) # format legend to be two columns
  }
  
  if(legend_position == "bottom") {
    plot <- plot +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = legend_number, byrow = TRUE)) # format legend to be two columns
  }
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max)
  }
  
  if(station_number == "07OB008") {
    plot <- plot +
      ggplot2::ylim(284, max(new.data$Value))
  }
  
  if(station_number == "10MC023") {
    plot <- plot +
      ggplot2::ylim(10.7, max(new.data$Value))
  }
  
  plot2 <- Levels.HighRes2(

    station_number = station_number, # Water Survey of Canada station number prior.
    y_min = y_min, # Minimum value on y-axis. Note: need to assign both y_min and y_max
    y_max = y_max, # Maximum value on y-axis. Note: need to assign both y_min and y_max
    plot_width = plot_width, # Output width of figure (cm)
    plot_height = plot_height # Output height of figure (cm)

  )

  # If no high-res data available, return only the historical plot
  if(is.null(plot2)) {
    message("No high-res data for ", station_number, " - returning historical plot only.")
    return(plot)
  }

  level_max <- max(new.data$Value, na.rm = T)
  
  if(station_number == "10MC023") {
    level_min <- 10.7
  } else {
    level_min <- min(new.data$Value, na.rm = T)
  }

  plot2 <- plot2 +
    ggplot2::ylim(level_min, level_max) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) 
  
    plot2 <- plot2 +
      ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"),
                    subtitle = paste0(lubridate::year(Sys.Date()), 
                                      " Water Levels (5 minute resolution)"),
                    x = "Date",
                    y = "Water Level")
    
    if(station_number == "07OB008") {
      plot2 <- plot2 +
        ggplot2::ylim(284, level_max) +
        ggplot2::geom_hline(yintercept = level_max, linetype = "dashed")
    }
    
    if(station_number == "07OB001") {
      plot2 <- plot2 +
        ggplot2::ylim(0.5, level_max) +
        ggplot2::geom_hline(yintercept = level_max, linetype = "dashed")
    }
    
    # if(station_number == "10FB006") {
    #   plot2 <- plot2 +
    #     ggplot2::ylim(6.9, level_max) +
    #     ggplot2::geom_hline(yintercept = level_max, linetype = "dashed")
    # }
    
    # if(station_number == "10GC001") {
    #   plot2 <- plot2 +
    #     ggplot2::ylim(4, 16.5) +
    #     ggplot2::geom_hline(yintercept = level_max, linetype = "dashed")
    # }
    
    if(station_number == "07OC001") {
      plot2 <- plot2 +
        ggplot2::ylim(0, level_max)
    }
    
    if(station_number == "07OA001") {
      plot2 <- plot2 +
        ggplot2::ylim(0, level_max)
    }
    
    if(station_number == "07OB006") {
      plot2 <- plot2 +
        ggplot2::ylim(0, level_max)
    }
    
    if(station_number == "07OB003") {
      plot2 <- plot2 +
        ggplot2::ylim(2.5,level_max)
    }
    
    if(station_number == "07OB004") {
      plot2 <- plot2 +
        ggplot2::ylim(3, level_max)
    }
    
    if(station_number == "10DA001") {
      plot2 <- plot2 +
        ggplot2::ylim(4.5, level_max)
    }
    
    if(station_number == "10MC008") {
      plot2 <- plot2 +
        ggplot2::ylim(11, level_max) +
        ggplot2::labs(title = "MACKENZIE RIVER BELOW RAYMOND CHANNEL (10MC008)", 
                          x = "Date", y = "Water Level (m)")
    }
    
    if(station_number == "10LC002") {
      plot2 <- plot2 +
        ggplot2::ylim(11, level_max)
    }

  plot_combine <- ggpubr::ggarrange(plot2, plot, nrow = 2, common.legend = TRUE, legend = legend_position) 
  
  ggplot2::ggsave(paste0(station$STATION_NUMBER, "_", Sys.Date(), ".png"), 
                  plot = plot_combine, device = "png", path = save_path,
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
  
  plot_combine
  
}

##################################################################################################

# Function for plotting temperatures relative to normal

Temps <- function(
  site = "Hay River",
  variable = "Mean", # Can be "Max", "Mean", or "Min"
  select_year = as.numeric(paste(lubridate::year(Sys.Date()))),
  start_month = 04,
  start_day = 01,
  end_month = 05,
  end_day = 31,
  start_year = 1991,
  end_year = (select_year - 1),
  ymin = -25,
  ymax = 30,
  line_colour = "blue",
  percentile_plot = TRUE,
  legend_position = "none",
  line_size = 0.5,
  point_size = 0.5,
  save = TRUE,
  plot_width = 14,
  plot_height = 7,
  dpi = 300)

{
  
  if(variable == "Mean" | variable =="mean"){variable = "mean_temp"}
  if(variable == "Min" | variable =="min"){variable = "min_temp"}
  if(variable == "Max" | variable =="max"){variable = "max_temp"}
  
  
  dy <- ifelse((is.leapyear(select_year) == TRUE), 2016, 2015)
  dylength <-ifelse(is.leapyear(select_year) == TRUE, 366, 365) 
  
  if(exists("cd_rds") == F) {
    
    directory = (paste0(user, "Documents/R_Scripts/Packages/nwtclimate/data/"))
    merged_data = "ECCC_Climate_Data_Merged.rds"
    cd_rds <- readRDS(paste0(directory, merged_data)) 
    
  }
  
  df_raw <- cd_rds %>%
    dplyr::filter(merged_name == site,
                  year %in% c(start_year:end_year, select_year)) %>%
    dplyr::filter(!(month == 2 & day == 29)) %>%
    dplyr::select(date, year, all_of(variable)) %>%
    dplyr::mutate(dayofyear = lubridate::yday(date)) 
  
  colnames(df_raw)[3] <- ("parameter")
  df_raw$parameter <- as.numeric(df_raw$parameter)
  
  # df <- df_raw %>%
  #   dplyr::filter(year != select_year) %>%
  #   dplyr::group_by(dayofyear) %>%
  #   dplyr::mutate(prctile = (ecdf(parameter)(parameter) * 100)  ,
  #                 Max = max(parameter, na.rm = TRUE),
  #                 QP90 = quantile(parameter, 0.90, na.rm = TRUE),
  #                 QP75 = quantile(parameter, 0.75, na.rm = TRUE),
  #                 QP50 = quantile(parameter, 0.50, na.rm = TRUE),
  #                 QP25 = quantile(parameter, 0.25, na.rm = TRUE),
  #                 QP10 = quantile(parameter, 0.10, na.rm = TRUE),
  #                 Min = min(parameter, na.rm = TRUE)) %>%
  #   subset(year == select_year)
  
  
  # Need to figure out how to calculate percentile for day of year.
  
  
#############
  
  df_select_prctile <- data.frame()
  
  for(i in unique(df_raw$dayofyear)) {
    
    df <- df_raw %>%
      dplyr::filter(dayofyear == i)  %>%
      dplyr::filter(!is.na(parameter))
    
    df_current <- df_raw %>%
      dplyr::filter(year == select_year) %>%
      dplyr::filter(dayofyear == i)
    
    df_current <- df_current %>%
      dplyr::mutate(prctile = sum(df$parameter <= df_current$parameter) / length(df$parameter) * 100)

    df_select_prctile <- dplyr::bind_rows(df_select_prctile, df_current)
    
  }
  
  df_select <- fasstr::fill_missing_dates(data = df_select_prctile, dates = date)
  df_select <- df_select[!(format(df_select$date,"%m") == "02" & format(df_select$date, "%d") == "29"), , drop = FALSE]
  
  ##############
  
  df_select <- df_select %>%
    dplyr::select(-date, -year) 
  
  col_1 <- as.numeric(c(1:365))
  col_2 <- as.numeric(rep(NA, 365))
  #col_3 <- as.numeric(rep(NA, 365))
  
 #  blank_df <- data.frame(col_1, col_2)
 #  colnames(blank_df) <- c("dayofyear", "parameter")
 #  
 # df_select <- dplyr::bind_rows(df_select, blank_df) 
 
 # df_select <- df_select %>%
 #   dplyr::group_by(dayofyear) %>%
 #   dplyr::summarize(parameter = mean(parameter, na.rm = T)) %>%
 #                    #prctile = mean(prctile, na.rm = T)) %>%
 #   dplyr::select(parameter)
 
 dummy_year <- df_raw %>%
    dplyr::filter(year != select_year) %>%
    dplyr::group_by(dayofyear) %>%
    dplyr::mutate(
      #prctile = (ecdf(parameter)(parameter) * 100),
      Max = max(parameter, na.rm = TRUE),
      QP90 = quantile(parameter, 0.90, na.rm = TRUE),
      QP75 = quantile(parameter, 0.75, na.rm = TRUE),
      QP50 = quantile(parameter, 0.50, na.rm = TRUE),
      QP25 = quantile(parameter, 0.25, na.rm = TRUE),
      QP10 = quantile(parameter, 0.10, na.rm = TRUE),
      Min = min(parameter, na.rm = TRUE)) 
  
  complete.year <- dummy_year %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(n = length(parameter)) #%>%
    #subset(n == dylength)
  complete.year <- max(complete.year$year)
  dummy_year <- dummy_year %>%
    subset(year == complete.year) %>%
    dplyr::select(-parameter) #%>%
    #dplyr::mutate(parameter = NA)
  
  df <- dplyr::bind_cols(dummy_year, df_select)
  
  df <- df %>%
    #dplyr::select(-year) %>%
    dplyr::mutate(year = select_year,
                  month = lubridate::month(date),
                  day = lubridate::day(date)) %>%
    #dplyr::select(-date) %>%
    dplyr::mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
    dplyr::select(-month, -day) #%>%
    # dplyr::group_by(Date) %>%
    # dplyr::summarize(parameter = mean(parameter, na.rm = TRUE),
    #                  prctile = mean(prctile, na.rm = T),
    #                  Max = mean(Max),
    #                  QP90 = mean(QP90),
    #                  QP75 = mean(QP75),
    #                  QP50 = mean(QP50),
    #                  QP25 = mean(QP25),
    #                  QP10 = mean(QP10),
    #                  Min = mean(Min)) %>%
    # dplyr::mutate(dayofyear = lubridate::yday(Date),
    #               Year = lubridate::year(Date))
  
  if(variable == "mean_temp" | variable =="mean"){title = "Mean"}
  if(variable == "min_temp" | variable =="min"){title = "Minimum"}
  if(variable == "max_temp" | variable =="max"){title = "Maximum"}
  df <- df[,-c(3,12)]
  
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = parameter)) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min, ymax = Max, fill = "Min - Max")) +
    #ggplot2::geom_ribbon(ggplot2::aes(ymin = QP10, ymax = QP90, fill = "10-90 Percentiles")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "Average Range")) +
    ggplot2::scale_fill_manual(name = "", 
                               values = c("Min - Max" = "gray85",
                                          #"10-90 Percentiles" = "gray80",
                                          "Average Range" = "gray70")) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste(select_year, site, "Daily", title, "Air Temperatures", sep = " "), 
                  #subtitle = paste0("Reference Period: ", start_year, " to ", end_year), 
                  x = "Month", y = "Air Temperature (\u00B0C)") +
    ggplot2::scale_x_date(date_breaks = "1 months",
                          labels = scales::date_format("%d %b")) +
    tidyquant::coord_x_date(xlim = c(paste(select_year, start_month, start_day, sep = "-"),
                                     paste(select_year, end_month, end_day, sep = "-"))) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylim(ymin, ymax)
  
  if (percentile_plot == TRUE) {
    plot <- plot + 
      ggplot2::geom_point(ggplot2::aes(colour = prctile), shape = 19, size = point_size) +
      ggplot2::geom_line(ggplot2::aes(colour = prctile), linewidth = line_size) +
      ggplot2::scale_colour_gradientn(name = "Historic Range \n(Percentile)",
                                      colours =  colorRampPalette(c("blue", "green", "red"))(50)) +
      ggplot2::theme(legend.position = legend_position)
    
  } else if (percentile_plot == FALSE) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(colour = factor(year)), linewidth = line_size) +
      ggplot2::geom_point(ggplot2::aes(colour = factor(year)), shape = 19, size = point_size) +
      ggplot2::scale_colour_manual(name = "",
                                   values = line_colour) + 
      ggplot2::theme(legend.position = legend_position)
  }
  
  if(save == TRUE) {
    ggplot2::ggsave(paste0(site, "_", Sys.Date(), "_Temperature.png"),
                    plot = plot, device = "png",
                    path = save_path,
                    scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = dpi)
  }

  plot
  
} 

##################################################################################################

# Function for plotting high resolution flow data

Flows_HighRes <- function(station_number = "07OB001",
                          previous_days = 15, 
                          y_min = NA,
                          y_max = NA,
                          plot_width = 16,
                          plot_height = 10,
                          start_date,
                          end_date)
{
  station <- tidyhydat::allstations %>%
    subset(STATION_NUMBER == station_number)
  
  flow.real <- tidyhydat::realtime_ws(station_number = station_number, 
                                         parameters = 47, 
                                         start_date = start_date,
                                         end_date = end_date)
  
  flow.real[,2] <- flow.real[,2] - (7*60*60) # Removes seven hours to convert to MT
  
  Flow <- flow.real %>%
    dplyr::filter(Date > (Sys.Date() - (60*60*24*previous_days)))
  
  plot <- ggplot2::ggplot(Flow, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                  subtitle = "High Resolution Flow Data",
                  x = "Date", y = expression(paste("Discharge (m"^3, " s"^-1,")")))
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max)
  }
  
  ggplot2::ggsave(paste0(station$STATION_NUMBER, Sys.Date(), "_Flows_HighRes.png"), 
                  plot = plot, device = "png", 
                  path = save_path, 
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 300)
  
  plot
  
}

##################################################################################################

# Function for plotting high resolution level data

Levels_HighRes <- function(station_number,
                           previous_days = 10, 
                           y_min = NA,
                           y_max = NA,
                           plot_width = 16,
                           plot_height = 10)
{
  station <- tidyhydat::allstations %>%
    subset(STATION_NUMBER == station_number)
  
  flow.real <- tidyhydat::realtime_ws(station_number = station_number, 
                                         parameters = 46, 
                                         start_date = lubridate::date(Sys.time() - 60*60*24*previous_days),
                                         end_date = Sys.Date())
  
  flow.real[,2] <- flow.real[,2] - (7*60*60) # Removes seven hours to convert to MT
  
  Flow <- flow.real %>%
    dplyr::filter(Date > (Sys.Date() - (60*60*24*previous_days)))
  
  plot <- ggplot2::ggplot(Flow, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    #ggplot2::geom_hline(yintercept = 344.88, linetype = "dashed", colour = "grey") +
    #ggplot2::geom_hline(yintercept = 344.68, linetype = "dashed") +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                  subtitle = "High Resolution Water Level Data",
                  x = "Date", y = "Water Level (m)")
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max)
  }
  
  ggplot2::ggsave(paste0(station$STATION_NUMBER, Sys.Date(), "_Levels_HighRes.png"),
                  plot = plot, device = "png", 
                  path = save_path, 
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 300)
  
  plot
  
}

##################################################################################################

# Function for plotting high resolution air temperature data

AirTemp_HighRes <- function(station_number,
                            previous_days = 10, 
                            y_min = NA,
                            y_max = NA,
                            plot_width = 14,
                            plot_height = 7)
{
  station <- tidyhydat::allstations %>%
    subset(STATION_NUMBER == station_number)
  
  flow.real <- tidyhydat::realtime_ws(station_number = station_number, 
                                         parameters = 1, 
                                         start_date = lubridate::date(Sys.time() - 60*60*24*previous_days),
                                         end_date = Sys.Date())
  
  flow.real[,2] <- flow.real[,2] - (7*60*60) # Removes seven hours to convert to MT
  
  Flow <- flow.real %>%
    dplyr::filter(Date > (Sys.Date() - (60*60*24*previous_days)))
  
  plot <- ggplot2::ggplot(Flow, ggplot2::aes(x = Date, y = Value)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(title = paste0(station$STATION_NAME, " (", station$STATION_NUMBER, ")"), 
                  subtitle = "Air Temperature",
                  x = "Date", y = "Air Temperature (?C)")
  
  if ((is.na(y_min) == F) && (is.na(y_max) == F)) {
    plot <- plot + 
      ggplot2::ylim(y_min, y_max)
  }
  
  ggplot2::ggsave(paste0(station$STATION_NUMBER, Sys.Date(), "_Temps_HighRes.png"),
                  plot = plot, device = "png", 
                  path = save_path, 
                  scale = 1, width = plot_width, height = plot_height, units = c("cm"), dpi = 300)
  
  plot
  
}

##################################################################################################

# Helper function to determine which basin map(s) to show
get_basin_maps <- function(basin_name, Hydrometric_List) {
  maps_path <- paste0(freshet_path, "maps/")
  maps <- list()

  if(basin_name == "Hay River") {
    maps[[1]] <- list(path = paste0(maps_path, "Hay_gauges.jpg"), width = 6.49, height = 5.07)
  }
  if(basin_name == "Liard River") {
    maps[[1]] <- list(path = paste0(maps_path, "Liard_gauges.jpg"), width = 6.49, height = 5.35)
  }
  if(basin_name == "Mackenzie River") {
    # Show Sahtu map if Norman Wells is active, otherwise FS map for upper stations
    if("10KA001" %in% Hydrometric_List) {
      maps[[1]] <- list(path = paste0(maps_path, "Mack_Sahtu_V3.png"), width = 6.5, height = 4.93)
    } else if(any(c("10FB007", "10FB006", "10GC001") %in% Hydrometric_List)) {
      maps[[1]] <- list(path = paste0(maps_path, "Mack_at_FS_gauges_V2.jpg"), width = 6.5, height = 5.03)
    }
  }
  if(basin_name == "Beaufort Delta") {
    maps[[1]] <- list(path = paste0(maps_path, "Mack_Peel_Delta_V2.png"), width = 6.5, height = 5.08)
  }

  return(maps)
}

##################################################################################################

#Generate spring break-up report

generate_spring_breakup_report <- function(Hydrometric_List, output_path, Climate_List = NULL,
                                            climate_start_month = 03, climate_start_day = 01,
                                            plot_start_month = "03", plot_start_day = "01",
                                            Weather_URLs = NULL, days_back) {
  
  doc <- officer::read_docx(path = paste0(freshet_path, "Template_BreakUpReport.docx"))
  
  current_datetime <- with_tz(Sys.time(), tzone = "America/Edmonton")
  day <- day(current_datetime)
  month <- format(current_datetime, "%B")
  time <- format(current_datetime, "%H:%M")
  
  formatted_datetime <- paste0(month, " ", day, ", ",
                               format(current_datetime, "%Y"), " at ", time)
  
  title_style <- officer::fp_text(
    font.size = 36,
    font.family = "Calibri Light",
    color = "#808285") #"#666666"
  
  # Create title content
  doc <- doc %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("NWT Water Monitoring Bulletin", title_style))) %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(paste0("– ", formatted_datetime), title_style))) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("")
  
  # Add Current Status section
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Current Status:", style = "heading 2") %>%
    officer::body_add_par("")
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Water Levels Summary:", style = "heading 2")
  
  # Generate and add the water levels table
  tryCatch({
    
    Last_Report <- as.POSIXct(Last_Report, tz = "America/Edmonton")
    
    water_levels_table <- WL_change_table(Hydrometric_List, Last_Report)
    
    doc <- doc %>%
      body_add_flextable(water_levels_table, align = "center") %>%
      officer::body_add_par("")  # Add spacing after table
  }, error = function(e) {
    message("Error generating water levels table: ", e$message)
    doc <- doc %>%
      officer::body_add_par("Water level data is currently unavailable.")
  })

  # Add Table of Contents
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Contents", style = "TOC Heading") %>%
    officer::body_add_toc(level = 3) %>%
    officer::body_add_break()

  # Create river basins list based on selected stations from hydrometric_list
  river_basins <- list()
  
  if (any(Hydrometric_List %in% Slave_River_GSL_List)) {
    river_basins[["Slave River and Great Slave Lake"]] <- intersect(Hydrometric_List, Slave_River_GSL_List)
  }
  if (any(Hydrometric_List %in% Hay_River_List)) {
    river_basins[["Hay River"]] <- intersect(Hydrometric_List, Hay_River_List)
  }
  if (any(Hydrometric_List %in% Liard_River_List)) {
    river_basins[["Liard River"]] <- intersect(Hydrometric_List, Liard_River_List)
  }
  if (any(Hydrometric_List %in% Mackenzie_River_List)) {
    river_basins[["Mackenzie River"]] <- intersect(Hydrometric_List, Mackenzie_River_List)
  }
  if (any(Hydrometric_List %in% Beaufort_Delta_List)) {
    river_basins[["Beaufort Delta"]] <- intersect(Hydrometric_List, Beaufort_Delta_List)
  }
  if (any(Hydrometric_List %in% Peel_River_list)) {
    river_basins[["Peel River"]] <- intersect(Hydrometric_List, Peel_River_list)
  }
  
  # Load all station data once at the beginning
  tryCatch({
    stations_data <- hy_stations() %>%
      dplyr::filter(STATION_NUMBER %in% Hydrometric_List)
  }, error = function(e) {
    message("Error loading HYDAT data: ", e$message)
    stations_data <- data.frame(STATION_NUMBER = character(), STATION_NAME = character())
  })
  
  # Define stations with images (copied from app, could remove all non-NT stations from this)
  Stations_w_images <- c(
    "05AAX08", "05BN012", "05CBX04", "05FE004",
    "07BE001","07CD005", "07FA006", "07FB008", "07FD010", "07GJ001","07HA001", "07KC001","07NB001", "07OB001", "07OB008",
    "08EE005","08FC003", "08GD004", "08JD006", "08LE027", "08MD013", "08ME023", "08MG025", "08NF001", "08NG002", "08NH119", "08NN026",
    "09AH001", "09BC002", "09CB001", "09CD001", "09DA001", "09DC006", "09EA003", "09EB001", "09FA001", "09FD003",
    "10AA001", "10ED001", "10ED002", "10FB006", "10GC001", "10KA001", "10LC014","10LD001","10MB004", "10MC002", "10MC008", "10ND002", "10PA001", "10PB001",
    "11AA001", "11AA005", "11AA031"
  )
  
  # Get current date for image retrieval
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Create empty lists to store plots and photos
  all_plots <- list()
  all_images <- list()
  
  # Loop through all stations - categorized by basin / river
  for (station in Hydrometric_List) {
    message("Processing station: ", station)
    
    
    # Create the hydrograph plot
    tryCatch({
      
      current_year <- lubridate::year(Sys.Date())
      last_year <- (current_year - 1)
      
      is_bennett <- station %in% Post_Bennett  # This will be TRUE for Post-Bennett stations (top of script_), FALSE otherwise
      
      p <- hydro_plot_dayofyear(
        parameter = "level",
        station_number = station,
        select_years = c(last_year, current_year),
        after_bennett = is_bennett,
        historic_min = NA,
        historic_max = last_year - 1,
        historic = T,
        log_scale = F,
        start_month = 01,
        start_day = 01,
        end_month = 12,
        end_day = 31,
        line_colours = c("dodgerblue", "blue4","green4", "red4", "purple4", "yellow4"),
        legend_position = "top",
        line_size = 0.3,
        point_size = 0,
        y_min = NA,
        y_max = NA,
        save = F,
        plot_width = 18,
        plot_height = 11,
        file_name = paste0("Hydrograph_", station)
      )
      
      all_plots[[station]] <- p
      
    }, error = function(e) {
      message("Error creating hydrograph for station ", station, ": ", e$message)
    })
    
    if (station %in% Stations_w_images) {
      tryCatch({
        
        # Retrieve all images from the last day (days_back = days_back)
        images_info <- retrieve_all_recent_images(station, days_back = days_back)
        
        # Extract image paths
        image_paths <- lapply(images_info, function(x) x$path)
        
        # Store image paths if any were found
        if (length(image_paths) > 0) {
          all_images[[station]] <- image_paths
        }
        
      }, error = function(e) {
        message("Image error for station ", station, ": ", e$message)
      })
    }
    
  }
  
  # Now organize by river basin
  for (basin_name in names(river_basins)) {
    stations <- river_basins[[basin_name]]
    
    # Add river basin heading
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par(basin_name, style = "heading 1")

    # Add Current Status placeholder (to be filled in manually in Word)
    doc <- doc %>%
      officer::body_add_par("Current Status:", style = "heading 2") %>%
      officer::body_add_par("")  # Empty paragraph for manual bullet points

    # Add basin map if available
    basin_maps <- get_basin_maps(basin_name, Hydrometric_List)
    if(length(basin_maps) > 0) {
      doc <- doc %>%
        officer::body_add_par("Station Map:", style = "heading 2")

      for(map_info in basin_maps) {
        if(file.exists(map_info$path)) {
          doc <- doc %>%
            officer::body_add_img(src = map_info$path, width = map_info$width, height = map_info$height) %>%
            officer::body_add_fpar(
              officer::fpar(
                officer::ftext("Above", officer::fp_text(font.family = "Times New Roman", italic = TRUE)),
                officer::ftext(": Map of Hydrometric Stations and nearby communities for the plots included in this section.",
                               officer::fp_text(font.family = "Times New Roman"))
              ),
              style = "Normal"
            ) %>%
            officer::body_add_par("")
        }
      }
    }

    # Add Hydrometric Data section
    doc <- doc %>%
      officer::body_add_par("Hydrometric Data:", style = "heading 2")
    
    # Add plots for this basin's stations
    for (station in stations) {
      if (!is.null(all_plots[[station]])) {
        # Add station heading and plot
        formatted_location_name <- get_formatted_location_name(station, stations_data)
        formatted_location_caption <- get_formatted_location_caption(station, stations_data)
        
        doc <- doc %>%
          officer::body_add_par(formatted_location_name, style = "heading 3") %>%
          officer::body_add_gg(all_plots[[station]], width = 6.36, height = 3.66)
        
        # Add hydrograph caption
        hydro_caption <- officer::fpar(
          officer::ftext("Above", officer::fp_text(font.family = "Times New Roman", italic = TRUE)),
          officer::ftext(paste0(" - Water level data for ", formatted_location_caption, ". Daily average levels for the previous year also are shown here."),
                         officer::fp_text(font.family = "Times New Roman"))
        )
        
        doc <- doc %>%
          officer::body_add_fpar(hydro_caption, style = "Normal")
        
      }
    }
    
    
    # Add Gauge Photos section
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par("Gauge photos:", style = "heading 2")
    
    # Add images for this basin's stations
    for (station in stations) {
      if (!is.null(all_images[[station]])) {
        # Add station heading
        formatted_location_name <- get_formatted_location_name(station, stations_data)
        formatted_location_caption <- get_formatted_location_caption(station, stations_data)
        doc <- doc %>%
          officer::body_add_par(formatted_location_name, style = "heading 3")
        
        # Add image and caption
        for (img_path in all_images[[station]]) {
          if (file.exists(img_path)) {
            # Get timestamp from filename
            img_filename <- basename(img_path)
            timestamp_str <- gsub(".*_(\\d{8}T\\d{6})Z\\.jpg", "\\1", img_filename)
            img_datetime <- as.POSIXct(timestamp_str, format = "%Y%m%dT%H%M%S", tz = "UTC")
            
            # Format datetime
            day <- day(img_datetime)
            month <- format(img_datetime, "%B")
            time <- format(img_datetime, "%H:%M")
            img_datetime_formatted <- paste0(month, " ", day, " at ", time)
            
            doc <- doc %>%
              officer::body_add_img(src = img_path, width = 6.5, height = 3.66
              ) %>%
              officer::body_add_fpar(
                officer::fpar(
                  officer::ftext("Above", officer::fp_text(font.family = "Times New Roman", italic = TRUE)),
                  officer::ftext(paste0(" - ", formatted_location_caption, " hydrometric gauge photo from ", img_datetime_formatted,
                                        ". Photo courtesy of Water Survey of Canada and GNWT."),
                                 officer::fp_text(font.family = "Times New Roman", font.size = 10))
                ),
                style = "Normal"
              )
          }
        }
      }
    }
    
    # Clear plots and images for this basin from memory
    for (station in stations) {
      all_plots[[station]] <- NULL
      all_images[[station]] <- NULL
    }
    
    # Add page break
    doc <- doc %>%
      officer::body_add_break()
  }
  
  # Add subheading for Weather Data
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Weather Data:", style = "heading 1") %>%
    officer::body_add_par("")

  # Add climate (air temperature) plots if Climate_List is provided
  if(!is.null(Climate_List) && length(Climate_List) > 0) {
    current_year <- lubridate::year(Sys.Date())
    current_month <- format(Sys.Date(), "%m")
    current_day_of_month <- as.numeric(format(Sys.Date(), "%d"))

    for(site in Climate_List) {
      message("Processing climate data for: ", site)
      tryCatch({
        temp_plot <- Temps(
          site = site,
          variable = "Mean",
          select_year = current_year,
          start_month = climate_start_month,
          start_day = climate_start_day,
          end_month = current_month,
          end_day = current_day_of_month,
          ymin = -40,
          ymax = 30,
          percentile_plot = TRUE,
          legend_position = "none",
          line_size = 0.5,
          point_size = 0.5,
          save = FALSE
        )

        doc <- doc %>%
          officer::body_add_par(paste0(site, " Air Temperature"), style = "heading 3") %>%
          officer::body_add_gg(temp_plot, width = 6.36, height = 3.66) %>%
          officer::body_add_fpar(
            officer::fpar(
              officer::ftext("Above", officer::fp_text(font.family = "Times New Roman", italic = TRUE)),
              officer::ftext(paste0(" - Daily mean air temperature for ", site,
                                    ". Shaded areas represent the historical range (1991-", current_year - 1, ")."),
                             officer::fp_text(font.family = "Times New Roman"))
            ),
            style = "Normal"
          ) %>%
          officer::body_add_par("")

      }, error = function(e) {
        message("Error creating temperature plot for ", site, ": ", e$message)
        doc <<- doc %>%
          officer::body_add_par(paste0("Temperature data for ", site, " is currently unavailable."))
      })
    }
  }

  # Add weather forecast screenshots if Weather_URLs provided
  if(!is.null(Weather_URLs) && length(Weather_URLs) > 0) {
    doc <- doc %>%
      officer::body_add_par("Weather Forecasts:", style = "heading 2") %>%
      officer::body_add_par("")

    for(community in names(Weather_URLs)) {
      message("Capturing weather forecast for: ", community)
      tryCatch({
        # Create temp file for screenshot
        temp_img <- tempfile(fileext = ".png")

        webshot::webshot(
          url = Weather_URLs[[community]],
          file = temp_img,
          vwidth = 1200,
          vheight = 1600,
          selector = "details.wxo-fcst",
          delay = 5
        )

        if(file.exists(temp_img) && file.info(temp_img)$size > 0) {
          doc <- doc %>%
            officer::body_add_par(paste0(community, " seven-day weather forecast:"), style = "Normal") %>%
            officer::body_add_img(src = temp_img, width = 6.5, height = 5.5) %>%
            officer::body_add_par("")
        }
      }, error = function(e) {
        message("Error capturing forecast for ", community, ": ", e$message)
        doc <<- doc %>%
          officer::body_add_par(paste0("Weather forecast for ", community, " could not be retrieved."))
      })
    }
  }

  # Add factors to watch section from its template
  doc <- doc %>%
    officer::body_add_docx(src = paste0(freshet_path, "Template_FactorsToWatch.docx"))
  
  
  
  # APPENDIX SECTIONS (and double level plots) - newly added Apr 23
  doc <- doc %>%
    body_add_par("Appendix A: River Ice Imagery", style = "heading 1") %>%
    body_add_par("", style = "Normal") %>%
    officer::body_add_par(" ")
  
  doc <- doc %>%
    body_add_par("Appendix B: High resolution and historic water level plots", style = "heading 1") %>%
    body_add_par("", style = "Normal")
  
  # list to store successful plots
  double_plots <- list()
  
  # create plots
  for(station in Hydrometric_List) {
    tryCatch({
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      current_month <- format(Sys.Date(), "%m")
      current_day <- as.numeric(format(Sys.Date(), "%d"))
      double_level_years <- (current_year - 19):current_year
      
      p2 <- Double_Level_Plot(
        station_number = station,
        select_years = double_level_years,
        station_title = FALSE,
        start_month = plot_start_month,
        start_day = plot_start_day,
        end_month = current_month,
        end_day = current_day,
        legend_position = "none",
        line_size = 0.5,
        point_size = 0.5,
        legend_text_size = 8,
        y_min = NA,
        y_max = NA,
        plot_width = 19,
        plot_height = 13,
        dpi = 300
      )
      
      # Only store successful plots
      if(!is.null(p2)) {
        double_plots[[station]] <- p2
      }
    }, error = function(e) {
      message("Error creating double level plot for station ", station, ": ", e$message)
    })
  }
  
  # only add subheadings for successfully created plots
  for(station in names(double_plots)) {
    location_name <- stations_data %>%
      dplyr::filter(STATION_NUMBER == station) %>%
      pull(STATION_NAME) %>%
      first() %>%
      {if(length(.) > 0) . else paste("Station", station)}
    
    formatted_location_name <- paste0(stringr::str_to_title(location_name), " (", station, ")")
    
    doc <- doc %>%
      officer::body_add_par(formatted_location_name, style = "heading 3") %>%
      officer::body_add_gg(double_plots[[station]], width = 6.36, height = 3.89) %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Above", officer::fp_text(font.family = "Times New Roman", italic = TRUE)),
          officer::ftext(paste0(" - The upper graph in the figure presents real time water level data at 5-minute resolution. The lower graph shows daily average levels relative to the previous 20 years."),
                         officer::fp_text(font.family = "Times New Roman"))
        ),
        style = "Normal"
      ) %>%
      officer::body_add_par("")
  }
  
  
  # Save the document
  print(doc, target = output_path)
  officer::body_end_section_continuous(doc)
}

############################################################################################################

#Station groupings helper function

Post_Bennett <- c(
  "07NB001", # SLAVE RIVER AT FITZGERALD
  "07OB002", # GREAT SLAVE LAKE AT HAY RIVER
  "07SB001" # GREAT SLAVE LAKE AT YELLOWKNIFE BAY
)

# Basin Lists
{
  Slave_River_GSL_List <- c(
    # Slave River Basin
    "07KC001", # Peace River at Peace Point
    "07DD001", # Athabasca River at Embarras Airport
    "07NB001", # Slave River at Fitzgerald
    
    # Great Slave Lake
    "07SB001", # Great Slave Lake at Yellowknife Bay
    "07OB002" # Great Slave Lake at Hay River
  )
  
  Hay_River_List <- c(
    # Hay River Basin
    "07OB001", # Hay River at Hay River
    "07OB008", # Hay River at the border
    "07OB004", # **Seasonal** Steen River near Steen River
    "07OB006", # **Seasonal** Lutose Creek near Steen River
    "07OB003", # **Seasonal** Hay River near Meander River
    "07OC001", # **Seasonal** Chinchaga River near High Level
    "07OA001" # **Seasonal** Sousa Creek near High Level
  )
  
  Liard_River_List <- c(
    # Liard River Basin
    "10AA006", # Liard River below Scurvy Creek
    "10AA001", # Liard River at Upper Crossing
    "10BE001", # Liard River at Lower Crossing,
    "10ED001", # Liard River at Fort Liard,
    "10EB001", # South Nahanni River above Virginia Falls
    "10ED002", # Liard River near the mouth
    "10DA001"
  )
  
  Mackenzie_River_List <- c(
    # Mackenzie River Basin (Upper + Sahtu)
    "10FB001", # Mackenzie River near Fort Providence
    "10FB006", # Mackenzie River at Strong Point
    "10FB007", # Mackenzie River at JMR
    "10GC001", # Mackenzie River at Fort Simpson
    "10KA001", # Mackenzie River at Norman Wells
    "10LA002", # Arctic Red River near the mouth
    "10KD001", # Mackenzie River at Sans Sault Rapids
    "10LD001"  # Mackenzie River at Fort Good Hope
  )

  Beaufort_Delta_List <- c(
    # Beaufort Delta
    "10LC002", # Mackenzie River (East Channel) at Inuvik
    "10LC014", # Mackenzie River at Arctic Red River
    "10LC015", # Mackenzie River at Confluence East Channel
    "10MC003", # Mackenzie River (Peel Channel) above Aklavik
    "10MC008", # Mackenzie River (Middle Channel) below Raymond Channel
    "10LC019", # Mackenzie River (Kumak Channel) below Middle Channel
    "10MC023"  # Mackenzie River (Napoiak Channel) above Shallow Bay
  )

  Peel_River_list <- c(
    # Peel River Basin
    "10MA001", # Peel River above Canyon Creek
    "10MC002", # Peel River above Fort McPherson
    "10MC022" # **Seasonal** Peel River at Frog Creek
  )
}

##########################################################################################################

#WL change table 

WL_change_table <- function(Hydrometric_List, Last_Report) {
  # Get current time and times for comparison
  current_time <- Sys.time()
  current_date <- as.Date(current_time)
  current_day <- as.numeric(format(current_date, "%j"))  # Day of year
  
  current_year <- as.numeric(format(Sys.time(), "%Y"))
  prev_year <- current_year - 1
  select_prev_year <- paste0(prev_year,":",  prev_year)
  
  Last_Report <- as.POSIXct(Last_Report, tz = "America/Edmonton")
  
  start_dt <- min(Sys.time() - hours(73), Last_Report)
  
  # Create empty list to store data for each station
  all_data <- list()
  prev_year_level_list <- list()

  # Debug: Print which stations we're processing
  print("Processing stations:")
  
  # Loop through each station
  for(station in Hydrometric_List) {
    print(paste("Getting data for station:", station))
    
    # Try to get data for each station
    tryCatch({
      # Get realtime data
      station_data <- tidyhydat::realtime_ws(
        station_number = station,
        parameters = 46,
        start_date = start_dt,
        end_date = Sys.time()
      )
      
      # Get historical statistics using hydro_calc_dayofyear
      historical_stats <- hydro_calc_dayofyear(
        station_number = station,
        parameter = "Levels",
        select_years = 1999:2023,
        historic_min = 1999,
        historic_max = 2023
      ) %>%
        dplyr::filter(DayofYear == current_day) %>%
        dplyr::reframe(
          STATION_NUMBER = first(STATION_NUMBER),  # Keep station identifier
          hist_min = first(Min),
          hist_max = first(Max),
          hist_median = first(Median),
          hist_mean = first(Mean),
          hist_p95 = first(P95),
          hist_p90 = first(P90),
          hist_p75 = first(P75),
          hist_p50 = first(P50),
          hist_p25 = first(P25),
          hist_p10 = first(P10),
          hist_p05 = first(P05),
          #Added by MA 2025-04-28 - count n of years with data for current day
          valid_years = sum(!is.na(Value)) # non-NA values in 'Value' col
        ) %>%
        dplyr::distinct()
      
      # Check for -Inf values in historical stats (occurs when seasonal stations
      # have no data for the current day of year, e.g. winter months for seasonal gauges).
      # These produce warnings from max()/min() but are handled by the valid_years check.
      if(any(historical_stats$hist_max == -Inf | historical_stats$hist_min == Inf, na.rm = TRUE)) {
        print(paste("Note:", station, "has no historical data for day", current_day,
                    "- this is expected for seasonal stations. Historical context will show as NA."))
      }
      if(historical_stats$valid_years[1] < 6) {
        print(paste("Note:", station, "has fewer than 6 years of data for day", current_day,
                    "(n =", historical_stats$valid_years[1], "). Historical context will show as NA."))
      }

      # Check if we have both realtime and historical data
      if(nrow(station_data) > 0 && nrow(historical_stats) > 0) {
        print(paste("Number of records:", nrow(station_data)))
        
        # Join historical stats with station data using STATION_NUMBER
        station_data <- station_data %>%
          dplyr::left_join(historical_stats, by = "STATION_NUMBER")
        
        all_data[[station]] <- station_data
      } else {
        print(paste("No data available for station:", station))
      }
    }, error = function(e) {
      print(paste("Error getting data for station", station, ":", e$message))
      return(NULL)
    })
    
    # Try to get last year's data for each station
    # First attempt: realtime_ws. Fallback: hydro_calc_daily (historical daily data).
    prev_val <- tryCatch({
      prev_year_station_data <- tidyhydat::realtime_ws(
        station_number = station,
        parameters = 46,
        start_date = Sys.time() - years(1) - hours(24),
        end_date = Sys.time() - years(1)
      )
      if(nrow(prev_year_station_data) > 0) {
        vals <- prev_year_station_data$Value[!is.na(prev_year_station_data$Value)]
        if(length(vals) > 0) tail(vals, 1) else NA_real_
      } else {
        NA_real_
      }
    }, error = function(e) {
      print(paste("Note: No realtime data available for", station, "for previous year. Trying historical daily data..."))
      NA_real_
    })

    # Fallback to hydro_calc_daily if realtime_ws returned no data
    if(is.na(prev_val)) {
      prev_val <- tryCatch({
        daily_data <- hydro_calc_daily(
          station_number = station,
          parameter = "Level",
          realtime_dl = FALSE
        )
        prev_yr_daily <- daily_data %>%
          dplyr::filter(WaterYear == prev_year, DayofYear == current_day)
        if(nrow(prev_yr_daily) > 0 && !is.na(prev_yr_daily$Value[1])) {
          print(paste("  -> Retrieved", prev_year, "level for", station, "from historical daily data."))
          prev_yr_daily$Value[1]
        } else {
          print(paste("  -> No historical daily data for", station, "on day", current_day, "of", prev_year))
          NA_real_
        }
      }, error = function(e) {
        print(paste("  -> Could not retrieve historical daily data for", station, ":", e$message))
        NA_real_
      })
    }

    prev_year_level_list[[station]] <- data.frame(
      STATION_NUMBER = station,
      Level_prev_year = prev_val,
      stringsAsFactors = FALSE
    )
  }
  
  # Check if we got any data
  if(length(all_data) == 0) {
    stop("No data available for any stations")
  }
  
  # Combine all station data that was successfully retrieved
  realtime_data <- dplyr::bind_rows(all_data)
  prev_year_level <- dplyr::bind_rows(prev_year_level_list)
  
  # Get station information only for stations with data
  available_stations <- names(all_data)
  station_info <- tidyhydat::hy_stations(station_number = available_stations)
  
  # Process the data
  level_summary <- realtime_data %>%
    dplyr::group_by(STATION_NUMBER) %>%
    dplyr::summarise(
      Current_Level = last(Value),
      Last_Reading_Time = max(Date),
      Level_24hr_ago = Value[which.min(abs(Date - (current_time - hours(24))))],
      Level_72hr_ago = Value[which.min(abs(Date - (current_time - hours(72))))],
      Level_last_report = Value[which.min(abs(Date - Last_Report))],
      hist_min = first(hist_min),
      hist_max = first(hist_max),
      hist_median = first(hist_median),
      hist_mean = first(hist_mean),
      hist_p95 = first(hist_p95),
      hist_p90 = first(hist_p90),
      hist_p75 = first(hist_p75),
      hist_p50 = first(hist_p50),
      hist_p25 = first(hist_p25),
      hist_p10 = first(hist_p10),
      hist_p05 = first(hist_p05),
      # Added by MA 2025-04-28: count for non-NA values - using p50 as proxy year for data completeness
      valid_years = first(valid_years),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # If no reading within the last 24 hours, set Current_Level to NA
      Current_Level = ifelse(as.numeric(difftime(current_time, Last_Reading_Time, units = "hours")) > 24, NA_real_, Current_Level),
      Change_24hr = Current_Level - Level_24hr_ago,
      Change_72hr = Current_Level - Level_72hr_ago,
      Change_since_last_report = Current_Level - Level_last_report,
      Percentile_Range = case_when(
        is.na(Current_Level) ~ "NA", # No recent data available
        valid_years < 6 ~ "NA", # MA added 2025-04-28 - hist context NA if min yrs of data on doy doesn't exist
        Current_Level > hist_max ~ "Above Max",
        Current_Level > hist_p95 & Current_Level <= hist_max ~ "95th-Max", # high
        Current_Level > hist_p90 & Current_Level <= hist_p95 ~ "90th-95th", # high
        Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "75th-90th", # above average
        Current_Level >= hist_p50 & Current_Level <= hist_p75 ~ "50th-75th", # average
        Current_Level >= hist_p25 & Current_Level <= hist_p50 ~ "25th-50th", # average
        Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "10th-25th", # below average
        Current_Level >= hist_p05 & Current_Level < hist_p10 ~ "5th-10th", # low
        Current_Level >= hist_min & Current_Level < hist_p05 ~ "Min-5th", # low
        Current_Level < hist_min ~ "Below Min"
      ),
      Historical_Context = case_when(
        is.na(Current_Level) ~ "NA", # No recent data available
        valid_years < 6 ~ "NA", # MA added 2025-04-28 - hist context NA if min yrs of data on doy doesn't exist
        Current_Level > hist_max ~ "Record High",
        Current_Level > hist_p95 & Current_Level <= hist_max ~ "Very High", # high
        Current_Level > hist_p90 & Current_Level <= hist_p95 ~ "High", # high
        Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "Above Average", # above average
        Current_Level >= hist_p25 & Current_Level <= hist_p75 ~ "Average", # average
        Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "Below Average", # below average
        Current_Level >= hist_p05 & Current_Level < hist_p10 ~ "Low", # low
        Current_Level >= hist_min & Current_Level < hist_p05 ~ "Very Low", # low
        Current_Level < hist_min ~ "Record Low"
      )
    )
  
  # format peak date
  peak_data <- peak_data %>%
    dplyr::mutate(
      peak_date = format(as.Date(peak_date), "%b %d, %Y")
    )

  level_summary <- level_summary %>%
    dplyr::left_join(peak_data %>% dplyr::select(STATION_NUMBER, peak_value, peak_date), by = "STATION_NUMBER") %>%
    dplyr::left_join(prev_year_level, by = "STATION_NUMBER") %>%
    dplyr::left_join(station_info %>% dplyr::select(STATION_NUMBER, STATION_NAME),
              by = "STATION_NUMBER") %>%
    dplyr::mutate(
      # Convert station names to title case
      STATION_NAME = stringr::str_to_title(STATION_NAME),
      Station = paste0(STATION_NAME, " (", STATION_NUMBER, ")")
    )
  level_summary <- level_summary %>%
    dplyr::select(Station, Current_Level, Change_24hr, Change_72hr, Change_since_last_report,
           Percentile_Range, Historical_Context, peak_value, peak_date, Level_prev_year) %>%
    dplyr::mutate(across(where(is.numeric), \(x) round(x, 2)))
  
  ft <- flextable(level_summary) %>%
    # for col headings
    set_header_labels(
      Station = stringr::str_wrap("Station Name (ID)", width = 17),
      Current_Level = stringr::str_wrap("Current Level (m)", width = 7),
      Change_24hr = stringr::str_wrap("24hr change (m)", width = 6),
      Change_72hr = stringr::str_wrap("72hr change (m)", width = 6),
      Change_since_last_report = stringr::str_wrap("Δ Last Report (m)", width = 6),
      Percentile_Range = stringr::str_wrap("Percentile Range", width = 9),
      Historical_Context = stringr::str_wrap("Historical Context (1999:2023)", width = 8),
      peak_value = stringr::str_wrap("Historical Peak (m)", width = 6),
      peak_date = stringr::str_wrap("Historical Peak Date", width = 8),
      Level_prev_year = stringr::str_wrap(paste0(prev_year, " Level (m)"), width = 5)
    ) %>%
    theme_vanilla() %>%
    # set col widths
    set_table_properties(layout = "fixed", width = 0.95) %>%
    # Reduce column widths
    width(j = "Station", width = 1.0) %>%
    width(j = c("Current_Level", "Change_24hr", "Change_72hr", "Change_since_last_report"),
          width = 0.4) %>%
    width(j = c("Percentile_Range", "Historical_Context"),
          width = 0.6) %>%
    width(j = c("peak_value", "Level_prev_year"),
          width = 0.4) %>%
    width(j = "peak_date",
          width = 0.5) %>%
    set_formatter(
      Station = function(x) stringr::str_wrap(x, width = 20),
      peak_date = function(x) stringr::str_wrap(x, width = 7)
    ) %>%
    align(j = "Station", align = "left", part = "all") %>%
    align(j = c("Current_Level", "Change_24hr", "Change_72hr", "Change_since_last_report",
                "Percentile_Range", "Historical_Context", "peak_value","peak_date","Level_prev_year"),
          align = "center", part = "all") %>%
    padding(padding.top = 0.2,
            padding.bottom = 0.2,
            padding.left = 0.1,
            padding.right = 0.1) %>%
    fontsize(size = 9, part = "all") %>%
    # Newly added - conditional formatting for WL changes > 1 m
    bold(i = ~ Change_24hr > 1.0, j = "Change_24hr", bold = TRUE, part = "body") %>%
    bold(i = ~ Change_72hr > 1.0, j = "Change_72hr", bold = TRUE, part = "body") %>%
    bold(i = ~ Change_since_last_report > 1.0, j = "Change_since_last_report", bold = TRUE, part = "body") %>%
    # Add conditional formatting for Percentile
    bg(i = ~ Percentile_Range == "Below Min", j = "Percentile_Range", bg = "#FF6666") %>%  # Bright red
    bg(i = ~ Percentile_Range == "Min-5th", j = "Percentile_Range", bg = "#FFB3B3") %>%  # Light red
    bg(i = ~ Percentile_Range == "5th-10th", j = "Percentile_Range", bg = "#FFD699") %>%  # Light orange
    bg(i = ~ Percentile_Range == "10th-25th", j = "Percentile_Range", bg = "#FFE6B3") %>%  # Very light orange
    bg(i = ~ Percentile_Range == "25th-50th", j = "Percentile_Range", bg = "#E6E6E6") %>%  # Light gray
    bg(i = ~ Percentile_Range == "50th-75th", j = "Percentile_Range", bg = "#E6E6E6") %>%  # Light gray
    bg(i = ~ Percentile_Range == "75th-90th", j = "Percentile_Range", bg = "#B3D9FF") %>%  # Very light blue
    bg(i = ~ Percentile_Range == "90th-95th", j = "Percentile_Range", bg = "#99CCFF") %>%  # Light blue
    bg(i = ~ Percentile_Range == "95th-Max", j = "Percentile_Range", bg = "#66B2FF") %>%  # Medium blue
    bg(i = ~ Percentile_Range == "Above Max", j = "Percentile_Range", bg = "#3399FF") %>%  # Bright blue
    # Add conditional formatting for Historical Context
    bg(i = ~ Historical_Context == "Record Low", j = "Historical_Context", bg = "#FF6666") %>%  # Bright red
    bg(i = ~ Historical_Context == "Very Low", j = "Historical_Context", bg = "#FFB3B3") %>%  # Light red
    bg(i = ~ Historical_Context == "Low", j = "Historical_Context", bg = "#FFD699") %>%  # Light orange
    bg(i = ~ Historical_Context == "Below Average", j = "Historical_Context", bg = "#FFE6B3") %>%  # Very light orange
    bg(i = ~ Historical_Context == "Average", j = "Historical_Context", bg = "#E6E6E6") %>%  # Light gray
    bg(i = ~ Historical_Context == "Above Average", j = "Historical_Context", bg = "#B3D9FF") %>%  # Very light blue
    bg(i = ~ Historical_Context == "High", j = "Historical_Context", bg = "#99CCFF") %>%  # Light blue
    bg(i = ~ Historical_Context == "Very High", j = "Historical_Context", bg = "#66B2FF") %>%  # Medium blue
    bg(i = ~ Historical_Context == "Record High", j = "Historical_Context", bg = "#3399FF") %>%  # Bright blue
    autofit() %>%
    add_footer_lines(
      values = c(
        "Note: Water levels are presented relative to an arbitrary datum. This means that the values on each gauge can be compared to different years but should not be used to compare water levels from one location to the next."
      )
    ) %>%
    fontsize(i = 1, j = 1, size = 9, part = "footer") %>%
    bold(i = 1, j = 1, part = "footer") %>%
    italic(i = 1, j = 1, part = "footer") %>%
    align(i = 1, j = 1, align = "left", part = "footer")
  
  return(ft)
  
  #print(ft)
}

##########################################################################################################

#format location name

get_formatted_location_name <- function(station, stations_data, filler_words = c("The", "Near", "At", "Above", "Below")) {
  location_name <- stations_data %>%
    dplyr::filter(STATION_NUMBER == station) %>%
    pull(STATION_NAME) %>%
    first() %>%
    {if(length(.) > 0) . else paste("Station", station)}
  
  formatted_name <- stringr::str_to_title(location_name)
  
  for (word in filler_words) {
    # Replace only if preceded by a space (not at start) and followed by space or end of string
    formatted_name <- gsub(
      paste0("(?<=\\s)", word, "(?=\\s|$)"),
      tolower(word),
      formatted_name,
      perl = TRUE
    )
  }
  
  return(paste0(formatted_name, " [", station, "]"))
  
}

##########################################################################################################

#format location caption

get_formatted_location_caption <- function(station, stations_data, filler_words = c("The", "Near", "At", "Above", "Below")) {
  location_name <- stations_data %>%
    dplyr::filter(STATION_NUMBER == station) %>%
    pull(STATION_NAME) %>%
    first() %>%
    {if(length(.) > 0) . else paste("Station", station)}
  
  formatted_name <- stringr::str_to_title(location_name)
  
  for (word in filler_words) {
    # Replace only if preceded by a space (not at start) and followed by space or end of string
    formatted_name <- gsub(
      paste0("(?<=\\s)", word, "(?=\\s|$)"),
      tolower(word),
      formatted_name,
      perl = TRUE
    )
  }
  
  return(paste0(formatted_name, " [", station, "]"))
}
