# R/image_download_utils.R
# new fun for downloading images

library(xml2)

download_wsc_image <- function(station_id, date, region, username, password) {
  base_url <- paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometric_additionalData/FieldData/", region, "/")
  save_dir <- paste0("data/images/", region, "/")
  
  print("\n=== Connection Details ===")
  print(paste("Base URL:", base_url))
  print(paste("Station ID:", station_id))
  print(paste("Input date:", date))
  
  # Get station timezone
  station_timezone <- get_station_timezone(station_id)
  if (is.null(station_timezone)) {
    station_timezone <- "UTC"
  }
  
  # Convert input date to local timezone
  local_date <- as.Date(date)
  local_start <- as.POSIXct(paste(local_date, "00:00:00"), tz = station_timezone)
  local_end <- as.POSIXct(paste(local_date, "23:59:59"), tz = station_timezone)
  
  # Convert to UTC for filename matching
  utc_start <- lubridate::with_tz(local_start, tzone = "UTC")
  utc_end <- lubridate::with_tz(local_end, tzone = "UTC")
  
  # Format dates for filename matching
  start_date_str <- format(utc_start, "%Y%m%d")
  end_date_str <- format(utc_end, "%Y%m%d")
  
  # Get directory listing
  print("\nAttempting directory listing...")
  dir_response <- httr::GET(
    base_url,
    httr::authenticate(username, password)
  )
  
  if (httr::status_code(dir_response) == 200) {
    html_content <- httr::content(dir_response, "text")
    doc <- read_html(html_content)
    links <- xml_find_all(doc, "//a")
    file_names <- xml_text(links)
    
    # Search for files between start and end dates
    matching_files <- grep(paste0(station_id, "_.*\\.jpg$"), file_names, value = TRUE)
    matching_files <- matching_files[grep(paste0("^", station_id, "_((", start_date_str, ")|(", end_date_str, "))"), matching_files)]
    
    if (length(matching_files) > 0) {
      downloaded_files <- list()
      
      for (file in matching_files) {
        img_url <- paste0(base_url, file)
        print(paste("Attempting to download:", img_url))
        
        img_response <- httr::GET(
          img_url,
          httr::authenticate(username, password)
        )
        
        if (httr::status_code(img_response) == 200) {
          # Extract UTC timestamp from filename
          timestamp_str <- gsub("Z\\.jpg$", "", 
                                gsub("^.*_", "", 
                                     gsub("T", " ", file)))
          utc_timestamp <- as.POSIXct(timestamp_str, format = "%Y%m%d %H%M%S", tz = "UTC")
          
          # Convert to local timezone
          local_timestamp <- lubridate::with_tz(utc_timestamp, tzone = station_timezone)
          
          # Only include if it falls within the local date range
          if (local_timestamp >= local_start && local_timestamp <= local_end) {
            # Create new filename with local timestamp
            local_date_str <- format(local_timestamp, "%Y%m%d")
            local_time_str <- format(local_timestamp, "%H%M%S")
            new_filename <- paste0(station_id, "_", local_date_str, "T", local_time_str, "Z.jpg")
            
            local_filename <- file.path(save_dir, new_filename)
            
            if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
            writeBin(img_response$content, local_filename)
            
            downloaded_files[[new_filename]] <- list(
              path = local_filename,
              timestamp = local_timestamp
            )
          }
        }
      }
      
      if (length(downloaded_files) > 0) {
        return(downloaded_files)
      }   
    }
  }
  return(NULL)
}

retrieve_image <- function(selected_station, selected_date, previous_day = FALSE) {
  image_list <- list()
  timestamp_list <- list()
  
  #source("R/Station_photos_lists.R") # commented out for report, and is already sourced in app code
  
  print(paste("Processing request for station:", selected_station))
  
  # Added Apr 21, 2025 - If previous_day is TRUE, subtract one day from the date
  if (previous_day) {
    selected_date <- as.Date(selected_date) - 1
    print(paste("Fetching images for previous day:", selected_date))
  }
  
  # Determine region
  region <- if (selected_station %in% WSC_images_NT) {
    "NT"
  } else if (selected_station %in% WSC_images_AB) {
    "AB"
  } else if (selected_station %in% WSC_images_BC) {
    "BC"
  } else if (selected_station %in% WSC_images_YT) {
    "YT"
  } else {
    stop(paste("Station", selected_station, "not found in any regional list"))
  }
  
  # Download image using our new function
  result <- download_wsc_image(
    station_id = selected_station,
    date = selected_date,
    region = region,
    username = "",
    password = ""
  )
  
  if (!is.null(result)) {
    
    # Added Apr 21, 2025 - Sort images by timestamp
    sorted_files <- names(result)[order(sapply(result, function(x) x$timestamp))]
    
    # Added Apr 21, 2025 - Extract paths and timestamps
    image_paths <- lapply(sorted_files, function(f) result[[f]]$path)
    image_timestamps <- lapply(sorted_files, function(f) result[[f]]$timestamp)
    
    # Added Apr 21, 2025 - Add to return lists
    image_list[[selected_station]] <- image_paths
    timestamp_list[[selected_station]] <- image_timestamps
    
  }
  
  return(list(images = image_list, timestamps = timestamp_list))
}

build_station_photo_history <- function(days_back = 10) {
  source("R/Station_photos_lists.R")
  
  # Initialize results list
  all_photo_dates <- list()
  
  # Calculate date range
  end_date <- Sys.Date()
  start_date <- end_date - days_back
  date_range <- format(seq(start_date, end_date, by = "day"), "%Y%m%d")
  
  # Function to process one region
  process_region <- function(station_list, region, existing_dates) {
    if (length(station_list) == 0) return(existing_dates)
    
    print(paste("Processing", region, "stations..."))
    
    base_url <- paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometric_additionalData/FieldData/", region, "/")
    
    # Get directory listing once for the region
    dir_response <- httr::GET(
      base_url,
      httr::authenticate("nhs_partner", "N0Lvr!p@")
    )
    
    if (httr::status_code(dir_response) == 200) {
      # Parse HTML content
      html_content <- httr::content(dir_response, "text")
      doc <- read_html(html_content)
      
      # Extract all links (file names)
      links <- xml_find_all(doc, "//a")
      file_names <- xml_text(links)
      
      # Process each station in the region
      for (station_id in station_list) {
        station_photos <- list()
        
        # Get station timezone
        station_timezone <- get_station_timezone(station_id)
        if (is.null(station_timezone)) {
          station_timezone <- "UTC"
        }

        # Check each date in our range
        for (date_str in date_range) {
          # Filter for this station's images on this date
          pattern <- paste0("^", station_id, "_", date_str)
          day_files <- grep(pattern, file_names, value = TRUE)
          
          if (length(day_files) > 0) {
            for (file in day_files) {
              # Extract datetime from filename
              datetime_str <- gsub("Z\\.jpg$", "", 
                                   gsub("^.*_", "", 
                                        gsub("T", " ", file)))
              
              # Parse as UTC
              utc_datetime <- as.POSIXct(datetime_str, format = "%Y%m%d %H%M%S", tz = "UTC")
              
              # Convert to local timezone
              local_datetime <- lubridate::with_tz(utc_datetime, tzone = station_timezone)
              
              # Get the local date
              local_date <- as.Date(local_datetime)
              
              # Add to station's photo list
              station_photos[[file]] <- list(
                datetime = local_datetime,  # Store local time
                filename = file,
                region = region
              )
            }
          }
        }
        
        # If we found any photos, add to results
        if (length(station_photos) > 0) {
          existing_dates[[station_id]] <- list(
            photos = station_photos,
            region = region,
            timezone = station_timezone
          )
          
          print(sprintf("Station: %s - Found %d photos between %s and %s", 
                        station_id, 
                        length(station_photos),
                        format(start_date, "%Y-%m-%d"),
                        format(end_date, "%Y-%m-%d")))
        } else {
          print(paste("No recent photos found for station:", station_id))
        }
      }
    }
    return(existing_dates)
  }
  
  # Process each region and accumulate results
  all_photo_dates <- process_region(WSC_images_NT, "NT", all_photo_dates)
  all_photo_dates <- process_region(WSC_images_AB, "AB", all_photo_dates)
  all_photo_dates <- process_region(WSC_images_BC, "BC", all_photo_dates)
  all_photo_dates <- process_region(WSC_images_YT, "YT", all_photo_dates)
  
  return(all_photo_dates)
}

#station_photos <- build_station_photo_history(days_back = 10)
check_station_photos <- function(station_id, check_date, station_photos) {
  # Convert check_date to Date if it's character
  if (is.character(check_date)) {
    check_date <- as.Date(check_date)
  }
  
  # Check if station exists in our data
  if (!station_id %in% names(station_photos)) {
    return(FALSE)
  }
  
  # Get station's photos
  station_data <- station_photos[[station_id]]
  
  # Get station timezone
  station_timezone <- get_station_timezone(station_id)
  if (is.null(station_timezone)) {
    station_timezone <- "UTC"
  }
  
  # Create a time range for the check date in local timezone
  local_start <- as.POSIXct(paste(check_date, "00:00:00"), tz = station_timezone)
  local_end <- as.POSIXct(paste(check_date, "23:59:59"), tz = station_timezone)
  
  # Check each photo's date
  for (photo in station_data$photos) {
    photo_datetime <- photo$datetime
    
    # Check if photo was taken within the local time range
    if (photo_datetime >= local_start && photo_datetime <= local_end) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}
# Function to get active stations for a specific date
get_active_stations <- function(check_date, station_photos) {
  active_stations <- list()
  
  for (station_id in names(station_photos)) {
    if (check_station_photos(station_id, check_date, station_photos)) {
      active_stations[[station_id]] <- station_photos[[station_id]]
    }
  }
  
  return(active_stations)
}


# New function to retrieve all images 
retrieve_all_recent_images <- function(station_id, days_back = 10) {
  images_info <- list()
  today <- Sys.Date()
  for (i in 0:(days_back - 1)) {
    date <- today - i
    result <- retrieve_image(station_id, as.character(date))
    if (!is.null(result$images[[station_id]]) && length(result$images[[station_id]]) > 0) {
      for (j in seq_along(result$images[[station_id]])) {
        images_info[[length(images_info) + 1]] <- list(
          path = result$images[[station_id]][[j]],
          timestamp = result$timestamps[[station_id]][[j]],
          date = date
        )
      }
    }
  }
  # Sort by timestamp descending (most recent first)
  images_info <- images_info[order(sapply(images_info, function(x) x$timestamp), decreasing = TRUE)]
  return(images_info)
}































# Test the function

# 
# station_id <- "10ED001"
# 
# 
# retrieve_all_recent_images(station_id, days_back=10)
# 

# # Test the functions
# test_date <- "2025-03-04"
# active <- get_active_stations(test_date, station_photos)
# print(paste("Number of stations with photos on", test_date, ":", length(active)))
# 


