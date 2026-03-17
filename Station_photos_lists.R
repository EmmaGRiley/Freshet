# R/Station_photos_lists.R

WSC_images_NT <- c(
  "07OB008", "07OB001", 
  "10ED001", "10ED002", "10FB006", "10GC001", "10KA001", "10LC014", "10LD001",
  "10MC002", "10MC008","10PA001",
  #"10KD001", # Mack river at sans sault rapids - last photo from Jan 28, 2025
  #"10LC015", # Mack River at confluence east channel - last photo from Oct 1, 2024 - NO LONGER found on url (last checked 2025-04-17)
  #"10MC003", # last photo from Dec 13, 2024 - NO LONGER found on url as of 2025-04-17
  #"10ND002, # TVC -  last photo from Mar 19, 2025
  "10PB001"
)

WSC_images_AB <- c(
  "07KC001",
  "07BE001", 
  #"07DAX04", # last photo taken Oct 17, 2024
  "07GJ001", # last photo taken Feb 28, 2025
  "05AAX08", "05BN012", "05CBX04", "05FE004", 
  "07NB001", "07CD005",  "07HA001",
  "11AA001", "11AA005", "11AA031" 
)

WSC_images_YT <- c(
  "10AA001", "10MB004",
  "09AH001", "09BC002", "09CB001", "09CD001", "09DA001", "09DC006","09EA003", "09EB001", "09FA001", "09FD003"
) 

WSC_images_BC <- c(
  "08EE005", # this stns photos are v sporadic
  #"08NM085", # last photo taken Aug 30, 2024 - NO LONG found on url as of 2025-04-17
  "07FA006", "07FB008", "07FD010", 
  "08CE001", "08CG001", "08FC003", "08GD004", "08JD006", "08LE027", "08MD013", "08ME023", "08MG025", "08NF001", "08NG002", "08NH119", "08NN026"
)

get_station_timezone <- function(station_id) {
  if (station_id %in% WSC_images_BC) {
    return("America/Vancouver")  # Pacific Time
  } else if (station_id %in% WSC_images_AB) {
    return("America/Edmonton")   # Mountain Time
  } else if (station_id %in% WSC_images_YT) {
    return("America/Whitehorse") # Yukon Time
  } else if (station_id %in% WSC_images_NT) {
    return("America/Yellowknife") # Mountain Time (NT)
  } else {
    return(NULL)
  }
}









# ALL STATIONS W IMAGES - incl YT, AB, BC

# Stations_w_images <- c(
#   "05AAX08", "05BN012", "05CBX04", "05FE004",
#   "07BE001","07CD005", "07FA006", "07FB008", "07FD010", "07GJ001","07HA001", "07KC001","07NB001", "07OB001", "07OB008",
#   "08EE005","08FC003", "08GD004", "08JD006", "08LE027", "08MD013", "08ME023", "08MG025", "08NF001", "08NG002", "08NH119", "08NN026",
#   "09AH001", "09BC002", "09CB001", "09CD001", "09DA001", "09DC006", "09EA003", "09EB001", "09FA001", "09FD003",
#   "10AA001", "10ED001", "10ED002", "10FB006", "10GC001", "10KA001", "10LC014","10LD001","10MB004", "10MC002", "10MC008", "10ND002", "10PA001", "10PB001",
#   "11AA001", "11AA005", "11AA031"
# )
