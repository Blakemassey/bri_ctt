source("Scripts/functions_kml.R")
source("Scripts/functions_gps.R")

# Runs Python Script
system('python C:/Work/R/Projects/bri_ctt/Scripts/Python_ctt.py')

# Compiles Downloaded Data and Joins with Previous Records ---------------------
ctt_recent <- compile_ctt()
ctt_all <- join_ctt(ctt_recent)

# Filters Records By Date ------------------------------------------------------
start_date <- as.character(floor_date(now() - period(1, "week"), "day"))
ctt_sub <- filter_locations(df = ctt_all, individual = "",
  start = start_date, end = "")

# Plot Location By Time of Day -------------------------------------------------
plot_location_times(df = ctt_sub, breaks = "1 days", wrap = TRUE)

## Export KML of Locations -----------------------------------------------------
export_kml_baea(df = ctt_sub)

# Send Email -------------------------------------------------------------------

# Replace with one or more valid addresses
recipients <- c("bhmassey@eco.umass.edu", "blakemassey@hotmail.com")

c("ian.trefry@navy.mil", "Derek.Hengstenberg@tetratech.com",
  "Chris.desorbo@briloon.org", "Chris.persico@briloon.org",
  "Bill.hanson@briloon.org")

send_email(ctt_data = ctt_sub, to = recipients)
