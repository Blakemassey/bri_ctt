suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(rJava))
suppressPackageStartupMessages(library(mailR))

#' Add Solar Times
#'
#' Add sunrise, sunset, and solarnoon to a dataframe of location data
#'
#' @usage add_solar(df, id, tz)
#'
#' @param df Dataframe
#' @param by Column name to use to split, analyze, and merge data. Default is
#'   "id".
#' @param tz Column name of timezone. Default is "Etc/GMT+5".
#'
#' @return Dataframe with sunrise, sunset, and solarnoon times
#' @export
#'
#' @details Coordinates in "lat" and "long" must be WGS84, sunrise and
#'   solarnoon based on first location, sunset based on last location.
add_solar <- function(df = df,
                      by = "id",
                      tz = "Etc/GMT+5"){
  df <- df
  df$by <- df[,by]
  if( ! ("date" %in% colnames(df))) {
    df$date <- as.Date(df$datetime,tz = tz)
  }
  add_times <- function (df=df){
    first <- plyr::ddply(df, plyr::.(date), function(x) x[1, ])  # first records
    sunrise_coords <- cbind(first$long, first$lat)
    sunrise_datetime <- first$datetime
    sunrise <- maptools::sunriset(sunrise_coords, sunrise_datetime,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)
    solarnoon <- maptools::solarnoon(sunrise_coords, sunrise_datetime,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), POSIXct.out = TRUE)
    sunrise$date <- as.Date(sunrise$time, tz = tz)
    sunrise$sunrise <- sunrise$time
    sunrise <- subset(sunrise, select = c(date, sunrise))
    solarnoon$date <- as.Date(solarnoon$time, tz=tz)
    solarnoon$solarnoon <- solarnoon$time
    solarnoon <- subset(solarnoon, select = c(date, solarnoon))
    last <- plyr::ddply(df, plyr::.(date), function(x) x[(nrow(x)), ])  # last
    sunset_coords <- cbind(last$long, last$lat)
    sunset_datetime <- last$datetime
    sunset <- maptools::sunriset(sunset_coords, sunset_datetime, proj4string =
      sp::CRS("+proj=longlat +datum=WGS84"), direction = "sunset",
      POSIXct.out= TRUE)
    sunset$date <- as.Date(sunset$time, tz=tz)
    sunset$sunset <- sunset$time
    sunset <- subset(sunset, select = c(date, sunset))
    df <- merge(df, sunrise, by="date", all.x = TRUE)
    df <- merge(df, solarnoon, by="date", all.x = TRUE)
    df <- merge(df, sunset, by="date", all.x = TRUE)
    df$hr_before_sunrise <- df$sunrise - 3600  # subtracts an hour from sunset
    lubridate::tz(df$sunrise) <- tz  # sets timezone for sunrise times
    lubridate::tz(df$hr_before_sunrise) <- tz  # sets timezone for sunrise times
    df$hr_after_sunset <- df$sunset + 3600  # adds an hour to sunset
    lubridate::tz(df$sunset) <- tz  # sets timezone for sunrise times
    lubridate::tz(df$hr_after_sunset) <- tz  # sets timezone for sunrise times
  return(df)
  }
  df2 <- plyr::ddply(df, plyr::.(by), add_times)
  df2$by <- NULL
  return(df2)
}

#' Compile CTT downloads
#'
#' Compiles the downloaded CTT files
#'
#' @usage compile_ctt(tz)
#'
#' @param tz Timezone, default is "Etc/GMT+5".
#'
#' @return Dataframe of compiled files
#' @export
#'
#' @import dplyr
#'
compile_ctt <- function(tz = "Etc/GMT+5") {
  infile <- paste0("Data/CTT")
  filenames <- list.files(path=infile, full.names=TRUE)
  output <- paste0("into ctt_recent", sep="")
  writeLines(noquote(paste(c("Compiling files:", filenames, output))))
  units_list <- list()
  for (i in 1:length(filenames)){
    if(length(read.csv(filenames[i])) > 1) {
      print(i)
      suppressWarnings(unit <- read.csv(filenames[i], colClasses= c("character",
      "character", "character", "character", "numeric",  "numeric", "numeric",
      "integer", "integer", "numeric", "numeric", "numeric", "numeric",
      "numeric", "integer", "numeric"), header = TRUE, na.strings = ""))
      name <- paste('item:', i, sep='')
      units_list[[name]] <- unit
    }
  }
  suppressWarnings(df <- do.call("rbind", units_list))
  df <- subset(df, select=serial:alt)
  df[which(colnames(df) == "speed")] <- as.integer(round(df$speed))
  df[which(colnames(df) == "alt")] <- as.integer(round(df$alt))
  df$serial <- substr(df$serial, nchar(df$serial)-5+1, nchar(df$serial))
    # removes first 15 digits in serial, then convert to integer
  colnames(df)[2] <- "date"  # "GPS_date_DDMMYYYY" to "date"
  colnames(df)[3] <- "time"  # "GPS_utc_HH:MM:SS" to "time"
  colnames(df)[4] <- "datetime"  # "GPS_YYYY..." to "datetime"
  df$datetimeUTC <- as.POSIXct(df$datetime, tz="GMT", usetz=FALSE)
    # set time to UTC and convert to POSIXct format
  df$datetime <- format(df$datetimeUTC, tz=tz, usetz=FALSE)  # convert to EST
  df$datetime <- as.POSIXct(df$datetime, tz=tz, usetz=FALSE)  # convert to EST
  df$year <- as.numeric(strftime(df$datetime, format="%Y", usetz=FALSE))  # year
  df$date <- as.POSIXct(df$datetime, tz=tz, "%Y-%m-%d", usetz=FALSE)  # date
  df$date <- as.Date(df$datetime, tz=tz,"%Y-%m-%d", usetz=FALSE)  # only date
  if("lon" %in% colnames(df)) colnames(df)[which(names(df) == "lon")] <- "long"
  df$long <- as.numeric(gsub("W", "", df$long))
  df$long <- as.numeric(gsub("E", "", df$long))
  if (all(df$long >= 0)) df$long <- (df$long)*-1
  df$lat <- as.numeric(gsub("N", "", df$lat))
  df$lat <- as.numeric(gsub("S", "", df$lat))
  gps_data <- read.csv("Data/GPS/GPS_Deployments.csv",
    header=TRUE, as.is=TRUE, na.strings = "")
  gps_data <-  subset(gps_data, select=serial:notes)  # keeps serial:notes col
  gps_data$id <- NA
  date_cols <- c("on_hand","deployed", "end_data",  "failed",  "removed",
      "recovered")
  for (i in date_cols) {
    gps_data[,i] <- as.character(gps_data[,i])
    gps_data[,i] <- as.Date(gps_data[,i], "%Y%m%d")
  }
  gps_blank <- gps_data[0,]
  gps_blank[1:nrow(df),] <- NA
  gps_blank$serial <- NULL  # removes the redundant serial column
  df <- cbind(df, gps_blank)
  deployed <- gps_data[which(!is.na(gps_data$deploy_location)),]
  for (i in 1:nrow(deployed)) {
    record <- deployed[i,]
    if (is.na(record$end_data)) {
      end_date <- Sys.Date() + 1
    } else {
      end_date <- record$end_data
    }
    sv <- df$serial == record$serial & df$date > record$deployed &
      df$date < end_date
    print(paste0(record$deploy_location, " has ", sum(sv), " records."))
    record$id <- record$deploy_location
    record$serial <- NULL # prevents a redundant "serial.1" column
    df[sv, (ncol(df)-length(record)+1):ncol(df)] <- record[1,]
  }
  df <- subset(df, (!is.na(deploy_location)))
  df <- df %>% dplyr::arrange(deploy_location, datetime)
  xy <- cbind(df$long,df$lat) # 2 col for next step
  xy <- rgdal::project(xy, "+proj=utm +zone=19 ellps=WGS84")  # to WGS84 UTM N19
  colnames(xy) <- c("long_utm", "lat_utm")  # name columns
  xy <- round(xy)  # rounds lat and long to zero decimal places
  df <- cbind(df, xy)  # combines lat long with data
  df$long_utm <- as.integer(df$long_utm)
  df$lat_utm <- as.integer(df$lat_utm)
  drops <- c("time", "utc", "cog", "data_voltage", "capacity",
    "pow_voltage", "pow_timestamp", "barfcn", "dbm", "netnameasc",
    "serv_timestamp", "id_long",
    "datetimeUTC")  # vector of columns to drop
  df <- df[ ,!(names(df) %in% drops)]
  df <- subset(df, !is.na(df$lat))
  df <- subset(df, !is.na(df$long))
  df <- df[df[,"lat"] != 0,] # removes rows with 0 for latitude
  df <- df[df[,"long"] != 0,] # removes rows with 0 for longitude
  df <- unique(df)  # removes duplicate rows, e.g. 72179 2013-12-29 06:28:39
  df <- df[,c("id",setdiff(names(df),"id"))]  # puts "id" column first
  row.names(df) <- NULL
  return(df)
}

#' Create Colors Using Metadata
#'
#' Creates and/or displays dataframe of IDs and their associated colors
#'
#' @usage create_colors(file, metadata_id)
#'
#' @param file CSV file with columns for "id" and "icon_color"
#' @param metadata_id column name for unique identifier, default is "id"
#'
#' @return df with ID names and hexidecimal colors
#' @export
#'
#' @details  Used in several other functions
#'
create_colors <- function(file = "Data/GPS/GPS_Deployments.csv",
                          metadata_id = "bird_ID"){
  metadata <- read.csv(file, header=TRUE, as.is=TRUE, na.strings = "")
  metadata$id <- metadata[, metadata_id]
  id_colors <- metadata$icon_color
  names(id_colors) <- metadata$id
  return(id_colors)
}

#' Filter Location Data
#'
#' Used to filter full dataset to individual(s) within a specified date range.
#'
#' @usage filter_locations(df, id, individual, start, end)
#'
#' @param df Dataframe with locations.
#' @param id Column name of unique identifier. Default is "id".
#' @param individual String, individual/s (from id column) to keep, format
#'   should be c(id, id), default is to keep all.
#' @param start Start date filter, default is 2017-01-01.
#' @param end End date filter, default is current date.
#'
#' @return A dataframe with subsetted rows
#' @export
#'
#' @details Defaults are specific to my file directories and locations
filter_locations <- function(df = df,
                            id = "id",
                            individual = NULL,
                            start = NULL,
                            end = NULL){
  if (is.null(start) || start == ""){
    start <- "2017-01-01"
  }
  if (is.null(end) || end == ""){
    today <- Sys.Date()
    end <- format(today, format="%Y-%m-%d")
  }
  starts <- paste("Start date: ", start, sep="")
  ends <- paste("End date: ", end, sep="")
  writeLines(noquote(starts))
  writeLines(noquote(ends))
  end <- as.POSIXct(end)
  end <- trunc(end, "days") + 60*60*24
  df <- df[df$datetime >= as.POSIXct(start) & df$datetime <= end,]
  row.names(df) <- NULL
  if(all(is.null(individual)) || individual == ""){
    writeLines(noquote ("All individuals included"))
  } else {
    writeLines(noquote(paste("Filtered to individual(s):", individual, sep="")))
    df <- df[df[,id] %in% individual,]
    row.names(df) <- NULL
  }
  return(df)
}

#' Joins CTT Records
#'
#' Imports previous ctt data and merges with existing
#'
#' @usage join_ctt(ctt_recent)
#'
#' @param ctt_recent Exisiting file to merge with imported records.
#'
#' @return Dataframe
#' @export
#'
#' @details Defaults are specific to file directories and locations
#'
join_ctt <- function(ctt_recent = ctt_recent) {
  ctt_all_file <- file.path(getwd(),"Data/ctt_all.rds")
  if (file.exists(ctt_all_file)){
    ctt_all_import <- readRDS(file.path(getwd(),"Data/ctt_all.rds"))
  } else {
    ctt_all_import <- ctt_recent
  }
  ctt_all <- unique(rbind(ctt_recent, ctt_all_import))
  ctt_all <- ctt_all[with(ctt_all, order(id, datetime)), ]
  saveRDS(ctt_all, file = "Data/ctt_all.rds")
  return(ctt_all)
}


#' Plots Locations By Time
#'
#' Plots locations in relation to sunrise, sunset, and solarnoon.
#'
#' @usage plot_location_times(df, by, individual, color_factor, pal, b_pal,
#'   start, end, breaks, tz, addsolartimes, wrap)
#'
#' @param df Dataframe with id, lat, long, datetime.
#' @param by Column name to use to split, analyze, and merge data. Default is
#'   "id".
#' @param individual String, individual/s (from id column) to keep, format
#'   should be c(id, id), default is all
#' @param color_factor Column names, factor to determine location point color.
#' @param pal String, color palette for CreateVarsColors(). Default is NULL.
#' @param b_pal String, color palette from RColorBrewer for CreateVarsColors(),
#'   default is "Set1"
#' @param color_factor Column names, factor to determine location point color.
#' @param start Date, filter start date. Default is 1970-01-01.
#' @param end Date, filter end date. Default is current date.
#' @param breaks Breaks on x-axis, i.e., "7 days".
#' @param tz Timezome. Default "Etc/GMT+5".
#' @param addsolartimes Logical, whether to not to run the AddSolarTimes() on
#'   data first. Default FALSE.
#' @param wrap Logical, whether or not to wrap ribbon of panels into 2d.
#'   Default is TRUE.
#'
#' @return Plot of locations over a range of dates
#' @export
#'
plot_location_times <- function(df = df,
                                by = "id",
                                individual = "",
                                color_factor = NULL,
                                pal = NULL,
                                b_pal = "Set1",
                                start = NULL,
                                end = NULL,
                                breaks = "1 days",
                                tz = "Etc/GMT+5",
                                addsolartimes = TRUE,
                                wrap = TRUE) {
  df <- df
  df$by <- df[,by]
  by_colors <- create_colors()
  if ( ! (individual == "" || individual == "all" || is.null(individual))){
    df = df[which(df[,"id"] == individual), ]  # to extract individuals
  }
  if (addsolartimes == TRUE) {
    df <- add_solar(df = df, by = by,  tz = tz)
    df$by <- df[,by]  # AddSolorTimes removes df$by
  }
  df$loc_time <- format(df$datetime, format = "%H:%M:%S")
  df$loc_time <- as.POSIXct(df$loc_time, tz="GMT", format = "%H:%M:%S")
  df$sunrise <- format(df$sunrise, format = "%H:%M:%S")
  df$sunrise <- as.POSIXct(df$sunrise, tz="GMT", format = "%H:%M:%S")
  df$solarnoon <- format(df$solarnoon, format = "%H:%M:%S")
  df$solarnoon <- as.POSIXct(df$solarnoon, tz="GMT", format ="%H:%M:%S")
  df$sunset <- format(df$sunset, format = "%H:%M:%S")
  df$sunset <- as.POSIXct(df$sunset, tz="GMT", format = "%H:%M:%S")
  if(!is.null(start) && start != "") {
    start = as.POSIXct(start)
  } else {
    start = min(df$datetime)
  }
  if(!is.null(end) && end != "") {
    end = as.POSIXct(end)
  } else {
    end = max(df$datetime)
  }
  limits_x = c(start, end)
  p <- ggplot(data = df) +
    geom_line(aes(datetime, solarnoon), colour = "red", size = 2) +
    geom_line(aes(datetime, sunset), colour = "orange" , size = 2) +
    geom_line(aes(datetime, sunrise), colour = "orange", size = 2) +
    labs(title = "Locations in Relation to Sunrise and Sunset",
      x="Date", y="Time") +
    scale_y_datetime(breaks=scales::date_breaks("1 hour"), labels =
        scales::date_format("%H")) +
    scale_x_datetime(breaks=scales::date_breaks(breaks), labels =
        scales::date_format("%m/%d"),limits=limits_x)
  if(!is.null(color_factor)) {
    p <-  p + geom_point(aes(datetime, loc_time, colour = factor(color_factor),
     shape = factor(color_factor)), size = 2) +
      labs(shape=cf_name, colour=cf_name) +  scale_fill_brewer(palette="Set1")
  } else {
    p <- p + geom_point(aes(datetime, loc_time), size=2)
  }
  p <- p + theme_bw() + theme(plot.title = element_text(size = 22)) +
    theme(text = element_text(size = 18, colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
    theme(text = element_text(size = 20, colour = "black")) +
    theme(axis.text = element_text(colour = "black"))  +
    theme(panel.background = element_rect(fill = "gray90")) +
    theme(panel.grid.major = element_line(color = "gray80")) +
    theme(panel.grid.minor = element_line(color = "gray80")) +
    scale_colour_manual(values = by_colors)
  if (wrap == TRUE) {
  p + facet_wrap(~ by)
  } else {
  p
  }
}

send_email <- function(ctt_data = ctt_sub,
                       to = recipients){
  ctt_data <- ctt_data
  kml_file <- "Data/KML/BAEA Data.kml"
  start_date <- min(ctt_data$date)
  end_date <- max(ctt_data$date)
  kml_output <- file.path("Data/KML", paste0("BAEA_Data", "_",
    start_date, "_", end_date, ".kml"))
  file.copy(kml_file, kml_output, overwrite=TRUE)
  attachments <- kml_output
  mailr_file <- read.csv("Data/MailR/mailR.csv", header=FALSE,
    stringsAsFactors=FALSE)
  body <- paste0("All,", "\n\n",
    "The attached KML file is currently available Bald Eagle GPS data for ",
      start_date, " to ", end_date, ".\n\n",
    "Additional locations may be added for this time period as new data ",
    "becomes available.", "\n\n",
    "Thanks,", "\n",
    "Chris", "\n\n\n",
    "Chris DeSorbo", "\n",
    "Raptor Program Director", "\n",
    "Biodiversity Research Institute", "\n",
    "276 Canco Rd", "\n",
    "Portland, ME,  04103", "\n",
    "Office: (207) 839-7600  XT 115","\n",
    "Mobile: (207) 212-0794", "\n",
    "Chris.desorbo@briloon.org")
  subject <- paste("BAEA GPS Data:", start_date, "to", end_date)
  send.mail(from=mailr_file[1,2], to=to, subject=subject, body=body,
    smtp = list(host.name="smtp.gmail.com", port=465, user.name=mailr_file[1,1],
    passwd=mailr_file[1,3], ssl=TRUE), attach.files=attachments,
    authenticate=TRUE, send=TRUE)
}
