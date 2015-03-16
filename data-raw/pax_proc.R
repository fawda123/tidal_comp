# code for processing raw data, see email from R. Murphy on 3/13/15

# import meta and data
pax_meta <- read.csv('PAX_station_info.csv', header = TRUE, 
  stringsAsFactors = FALSE)
pax_data <- read.csv('PAX_TRIB_CHLAandSALINITY_85to14.csv', header = TRUE, 
  stringsAsFactors = FALSE)

# date as Date class
pax_data$date <- as.Date(pax_data$date, format = '%m/%d/%Y')
