suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(require(plotKML))
suppressPackageStartupMessages(require(tools))

#' ExportKMLTelemetry
#'
#' Create a Google Earth KML file (points and multitrack) from lat/long
#'   coordinates.
#'
#' @usage ExportKMLTelemetry(df, id, datetime, lat, long, alt, alt_mode, speed,
#'   agl, behavior, point_color, point_metadata, point_pal, point_r_pal,
#'   point_b_pal, extrude, path, path_color, path_metadata, path_pal,
#'   path_r_pal, path_b_pal, arrow, icon_by_sex, labelscale, dateformat,
#'   timeformat, datetimeformat, file, output_dir)
#'
#' @param df input dataframe, must have id, lat, long, and datetime
#' @param id column name of unique identifier, data is split into unique paths
#'   and separate folders based on this parameter
#' @param datetime column name of datetime in POSIXct format or as a character
#'   in the format (\%Y/\%m/\%d \%H:\%M)
#' @param lat column name of latitude coordinates (WGS84, dec. degree)
#' @param long column name of longitude coordinates (WGS84, dec. degree)
#' @param alt input dataframe column name for altitude(m). Optional.
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "clampedToGround".
#' @param speed input dataframe column name for speed. Optional
#' @param agl input dataframe column name for "altitude above ground level",
#'   optional
#' @param behavior input dataframe column name for behavior. Optional
#' @param point_color column name that determines the color for each point, may
#'   be same as 'id' parameter, but may also be sex, behavior, season, etc.
#'   Default is 'id' parameter
#' @param point_metadata location of metadata .csv file. Metadata file must
#'   have a column that matches name of 'point_color'parameter and "icon_color"
#'   column with hexadecimal colors.
#' @param point_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param point_r_pal Specifc number of 'R_pal' color palette from the
#'   'PlotKML' Package (e.g., 1 = R_pal[[1]]). This parameter has priority over
#'   the 'b_pal' parameter for setting the colors. Default is NULL.
#' @param point_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param extrude logical, either FALSE (default) for no line, or TRUE which
#'   extends a line from the point to the ground.
#' @param path logical, to create Track paths. Default is TRUE.
#' @param path_color similar to 'point_color' parameter, but the value must
#'   have the same factor level structure as the id file, because each path is
#'   constructed for each id factor. Default will use 'id' parameter.
#' @param path_metadata location of metadata .csv file. Metadata file must have
#'   a column that matches name of 'path_color' parameter and an "icon_color"
#'   column with hexadecimal colors.
#' @param path_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param path_r_pal Specifc number of 'R_pal' color palette from the 'PlotKML'
#'   Package (e.g., 1 = R_pal[[1]]). This parameter has priority over the
#'   'b_pal' parameter for setting the colors. Default is NULL.
#' @param path_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param arrow logical, use arrow heads on path icons.
#' @param icon_by_sex logical, use different icons based on "sex" column
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param dateformat changes the format of the date in the Google Earth
#'   location pop-up windows. Default is "\%Y/\%m/\%d".
#' @param timeformat changes the format of the time in the Google Earth
#'   locations pop-up windows. Default is "\%I:\%M \%p".
#' @param datetimeformat changes the datetime format for the label of
#'   highlighted points. Default is "\%Y/\%m/\%d \%I:\%M \%p"
#' @param file filename of output KML file, default is name of input dataframe
#' @param output_dir name for folder in the KML file, default is working
#'   directory
#'
#' @return KML of points and multitracks
#' @export
#'
export_kml <- function (df,
                        id = "id",
                        datetime = "datetime",
                        lat = "lat",
                        long = "long",
                        alt = NULL,
                        alt_mode = "clampToGround",
                        speed = NULL,
                        agl = NULL,
                        behavior = NULL,
                        point_color = NULL,
                        point_metadata = NULL,
                        point_pal = NULL,
                        point_r_pal = NULL,
                        point_b_pal = "Set1",
                        extrude = FALSE,
                        path = TRUE,
                        path_color = NULL,
                        path_metadata = NULL,
                        path_pal = NULL,
                        path_r_pal = NULL,
                        path_b_pal = NULL,
                        arrow = TRUE,
                        icon_by_sex = FALSE,
                        labelscale = 0,
                        dateformat = "%Y-%m-%d",
                        timeformat = "%I:%M %p",
                        datetimeformat = "%Y-%m-%d %I:%M %p",
                        file = NULL,
                        output_dir = NULL) {
  if (is.null(output_dir) == TRUE) {
    if (!is.null(file)) {
      outfile <- file.path(getwd(), file)
    } else {
      outfile <- file.path(getwd(), deparse(substitute(df)))
    }
  } else {
    if (!is.null(file)) {
      outfile <- file.path(output_dir, file)
    } else {
      outfile <- file.path(output_dir, deparse(substitute(df)))
    }
  }
  if (file_ext(outfile) == "") {
    outfile <- paste0(outfile, ".kml")  # if object has no extension
  }
  df <- df
  df$id <- df[ ,id]
  df$lat <- df[ ,lat]
  df$long <- df[ ,long]
  if(!"sex" %in% colnames(df))  df$sex <- NA
  if (!is.null(alt)){
    df$desc_alt <- df[ ,alt]  # writes altitude to the "alt" column
    alt1 <- '\t\t\t\t\tAltitude: '  # first part of "Altitude" description
    alt2 <- '\n'  # second part of "Altitude" description
  } else {
    df$alt <- 0  # makes the altitude column a vector of NA
    df$desc_alt <- ""  # makes the altitude description blank values
    alt1 <- NULL  # prevents "Altitude" description from being written
    alt2 <- NULL  # prevents "Altitude" description from being written
  }
  if (!is.null(agl)) {
    df$desc_agl <- df[ ,agl]
    agl1 <- '\t\t\t\t\tAGL: '  # first part of the "Altitude" description
    agl2 <- '\n'  # second part of "Altitude" description
  } else {
    df$desc_agl <- ""  # makes the altitude column a vector of blank values
    agl1 <- NULL  # prevents "Altitude Above Ground Level" from being written
    agl2 <- NULL  # prevents "Altitude Above Ground Level" from being written
  }
  if (!is.null(speed)) {
    df$desc_speed <- df[,speed]  # writes speed to the "speed" column
    spd1 <- '\t\t\t\t\tSpeed: '  # writes first part of the "Speed" description
    spd2 <- '\n'  # writes second part of the "Speed" description
  } else {
    df$desc_speed <- ""  # makes the speed column a vector of blank values
    spd1 <- NULL  # prevents "Speed" description from being written
    spd2 <- NULL  # prevents "Speed" description from being written
  }
  if (!is.null(behavior)) {
    df$desc_behavior <- df[,behavior]  # writes behavior to "behavior" column
    beh1 <- '\t\t\t\t\tBehavior: '  # first part of the "Behavior" description
    beh2 <- '\n'  # second part of the "Behavior" description
  } else {
    df$desc_behavior <- ""  # makes the behavior column a vector of blank values
    beh1 <- NULL  # prevents "Behavior" description from being written
    beh2 <- NULL  # prevents "Behavior" description from being written
  }
  df$datetime <- strftime(df[,datetime],'%Y-%m-%d %H:%M:%S', tz =
    tz(df[,datetime]))
  df$datetime <- as.character(df$datetime)  # needed for KML parsing
  df$datetimebegin <- df$datetime
  end_times <- function(data) { # locates last time in data
    if (nrow(data) == 1) {
      data$datetime2 <- data$datetime[1]
    } else {
      data$datetime2 <- data$datetime[c(2:length(data$datetime),
      length(data$datetime))]
    }
  }
  ids <- as.character(df$id)  # as.character removes factor levels
  df_split <- split(df, ids)  # divides data by ids
  df_split <- lapply(df_split, end_times)
  datetimeend <- unsplit(df_split, ids)  # returns array of returned values
  df <- cbind(df, datetimeend)  # adds datetimeend column to original baea data
  ifelse(extrude == TRUE, extrude <- 1, extrude <- FALSE)
  placemark_point <- function(PN, X,  Y, Z, ZD,
                             AG, SP, BH, SX, PS,
                             ID, SD, ST, ED, ET,
                             DA, TM) {
    if (icon_by_sex == TRUE) PS <- paste0(PS,"-",SX)
    cat("\t<Placemark>\n",
      "\t\t<name>",PN, "</name>\n",
      "\t\t<TimeSpan>\n",
      "\t\t\t<begin>",SD ,"T" ,ST ,"</begin> " , "\n",
      "\t\t\t<end>", ED, "T", ET, "</end> ", "\n",
      "\t\t</TimeSpan>\n",
      "\t\t\t<Snippet></Snippet>", "\n",
      "\t\t\t\t<description>\n",
      "\t\t\t\t\tID: ", ID, "\n",
      "\t\t\t\t\tDate: ", DA, "\n",
      "\t\t\t\t\tTime: ", TM, "\n",
      "\t\t\t\t\tLongitude: ", X, "\n",
      "\t\t\t\t\tLatitude: ", Y, "\n",
      alt1, ZD, alt2,  # written when !is.null(agl)
      agl1, AG, agl2, # written when !is.null(agl)
      spd1, SP, spd2,  # written when !is.null(speed)
      beh1, BH, beh2, # written when !is.null(behavior)
      "\t\t\t\t</description>", "\n",
      "\t\t\t<styleUrl>#Point_",PS,"</styleUrl>\n",
      "\t\t\t<Point>\n",
      "\t\t\t\t<extrude>", extrude, "</extrude>\n",
      "\t\t\t\t<altitudeMode>", alt_mode, "</altitudeMode>\n",
      "\t\t\t\t\t<coordinates>", X, ",", Y, ",", Z, "</coordinates>\n",
      "\t\t\t</Point>\n",
      "\t</Placemark>\n",
      file = outfile, append = TRUE, sep = "")
    }
  if (file.exists(outfile)) file.remove(outfile)  # delete KML if already exists
  writeLines(noquote(c("Writing: ", outfile)))
  ## Title Section ##
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
  "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
  "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
  "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
  "<Document>\n", "\t<name>",file,"</name>\n",file = outfile,
  append = FALSE, sep = "")
  ## Icon Style Section ##
  if (is.null(point_color)) point_color <- id
  df$point_color <- df[ ,point_color]
  if (!is.null(point_metadata)) {
    point_colors <- create_colors(file=point_metadata,
      metadata_id=point_color)
    point_colors <- subset(point_colors, names(point_colors) %in%
      unique(df$point_color))
  } else {
    suppressWarnings(point_colors <- CreateColorsByVar(by=point_color, df=df,
      pal=point_pal, r_pal=point_r_pal, b_pal=point_b_pal))
  }
  if (icon_by_sex == TRUE) {
    point_colors_names <- c(sapply(names(point_colors), paste0, "-female"),
      sapply(names(point_colors), paste0,"-male"))
    point_colors <- rep(point_colors, 2)
    names(point_colors) <- point_colors_names
  }
  point_colors <- sapply(point_colors, col2kml)
  point_colors <- sapply(point_colors, substring, 4, 9)
  df$point_color <-df[,point_color]
  icon_scale <- 0.7
  hi_icon_label_scale <- 0.75
  ball_bg_color <- "ff333333"
  ball_text_color <- "ffffffff"
  mt_icon_href <- file.path("http://earth.google.com/images/kml-icons",
    "track-directional/track-0.png")
  icon_href <- "http://maps.google.com/mapfiles/kml/shapes/placemark_square.png"

  for (i in 1:length(point_colors)) {
    if (icon_by_sex == TRUE){
      if (grepl("male", names(point_colors)[i]) == TRUE)  icon_href <-
        "http://maps.google.com/mapfiles/kml/shapes/placemark_square.png"
      if (grepl("female", names(point_colors)[i]) == TRUE) icon_href <-
        "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
    }
    cat("\t<StyleMap id=\"Point_",names(point_colors)[i],"\">\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>normal</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",labelscale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>",icon_scale,"</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>highlight</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",hi_icon_label_scale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>0.8</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>", "\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>", "\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t</StyleMap>\n",
      file = outfile, append = TRUE, sep = "")
  }
  if (path ==TRUE) {
    if (is.null(path_color)) path_color <- id
    if (is.null(path_b_pal)) path_b_pal <- point_b_pal
    if (!is.null(path_metadata)) {
      path_colors <- create_colors(file=path_metadata,
        metadata_id=path_color)
      path_colors <- subset(path_colors, names(path_colors) %in%
        unique(df[,path_color]))
    } else {
      suppressWarnings(path_colors <- CreateColorsByVar(by=path_color, df=df,
        pal=path_pal, r_pal=path_r_pal, b_pal=path_b_pal))
    }
    path_colors <- sapply(path_colors, col2kml)
    path_colors <- sapply(path_colors, substring, 4, 9)
    if (icon_by_sex == TRUE) {
      path_colors_names <- c(sapply(names(path_colors), paste0, "-female"),
      sapply(names(path_colors), paste0,"-male"))
      path_colors <- rep(path_colors, 2)
      names(path_colors) <- path_colors_names
    }
    ifelse(arrow == TRUE, arrow <- 1, arrow <- 0)
  ## Style Map for Track ##
    for (i in 1:length(path_colors)) {
      cat("\t<StyleMap id=\"Track_",names(path_colors)[i],"\">\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>normal</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>0</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t<IconStyle>\n",
      "\t\t\t\t<scale>",arrow,"</scale>\n",
      "\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",mt_icon_href,"</href>\n",
      "\t\t\t\t</Icon>\n",
      "\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>",names(path_colors)[i]," - Path</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t<LineStyle>\n",
      "\t\t\t\t<color>dd",path_colors[i],"</color>\n",
      "\t\t\t\t<width>1</width>\n",
      "\t\t\t</LineStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>highlight</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>0</scale>\n",  # to show label, change value to >= 0.7
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t<IconStyle>\n",
      "\t\t\t\t<scale>1</scale>\n",
      "\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",mt_icon_href,"</href>\n",
      "\t\t\t\t</Icon>\n",
      "\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>",names(path_colors)[i]," - Path</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t<LineStyle>\n",
      "\t\t\t\t<color>ee",path_colors[i],"</color>\n",
      "\t\t\t\t<width>1</width>\n",
      "\t\t\t</LineStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t</StyleMap>\n",
      file = outfile, append = TRUE, sep = "")
    }  # end of Track icon loop
  } # end of (path == TRUE)

  ids <- as.character(unique(df$id))  # as.character removes factor levels
  for (i in ids) {
    sv = df$id %in% i
    unique_id <- as.character(unique(df$id[sv]))
    cat("<Folder>\n","<name>",unique_id,"</name>\n","<open>0</open>\n",
      file = outfile, append = TRUE, sep = "")
    cat("\t<Folder>\n","\t<name>",unique_id," - Locations</name>\n",
      "\t<open>0</open>\n", file = outfile, append = TRUE, sep = "")
    locs <- subset(df, id == unique_id)
    for (i in 1:nrow(locs)){
      loc <- locs[i,]
      PNs <- strftime(loc[, "datetimebegin"], datetimeformat)
      Xs <- loc[, "long"]
      Ys <- loc[, "lat"]
      Zs <- loc[, "alt"]
      ZDs <- loc[, "desc_alt"]
      SPs <- loc[, "desc_speed"]
      AGs <- loc[, "desc_agl"]
      BHs <- loc[, "desc_behavior"]
      SXs <- loc[, "sex"]
      PSs <- loc[, "point_color"]
      IDs <- unique_id
      SDs <- substring(loc$datetime, 1,10) #start date
      STs <- substring(loc$datetime, 12,16) #start time
      EDs <- substring(loc$datetimeend, 1,10) #end date
      ETs <- substring(loc$datetimeend, 12,19) #end time
      DAs <- strftime(loc[, "datetimebegin"], dateformat)
      TMs <- strftime(loc[, "datetimebegin"], timeformat)
      placemark_point(PNs, Xs, Ys, Zs, ZDs,
                     AGs,  SPs, BHs, SXs, PSs,
                     IDs,  SDs, STs, EDs,  ETs,
                     DAs, TMs)
    }
    cat("\t</Folder>\n", file = outfile, append = TRUE, sep = "")
    locs$Ts <- "T"
    locs$Zs <- "Z"
    locs$datetimedate <- substring(locs$datetime, 1,10) #start date
    locs$datetimetime <- substring(locs$datetime, 12,16) #start time
    whens <- locs[, c("datetimedate","Ts","datetimetime", "Zs")]
    sgmts <- locs[, c("long","lat","alt")]
    unique_id <- unique(locs$id)
    ifelse(icon_by_sex == TRUE, path_id <- paste0(unique_id, "-",
      unique(locs$sex)), path_id <- unique_id)
    bloc2 <- NULL
    if (path == TRUE) {
      bloc2 <- c(bloc2, paste(
        "\t<Placemark>\n",
        "\t\t<name>",unique_id," - Path</name>\n",
        "\t\t<styleUrl>#Track_",path_id,"</styleUrl>\n",
        "\t\t<gx:balloonVisibility>0</gx:balloonVisibility>\n",
        "\t\t<gx:Track>\n",
        "\t\t<altitudeMode>",alt_mode,"</altitudeMode>\n",
      paste(paste("\t\t\t\t\t<when>", apply(whens, 1, paste, collapse=""),
        sep=""), "</when>", collapse="\n"), "\n",
      paste(paste("\t\t\t\t\t<gx:coord>", apply(sgmts, 1, paste, collapse=" "),
        sep = ""),"</gx:coord>", collapse = "\n"),"\n",
        "\t\t</gx:Track>\n",
        "\t</Placemark>\n",
        sep = ""))
    }
    cat(bloc2, "\t</Folder>\n", file = outfile, append = TRUE)
  }
  cat("</Document>\n</kml>", file = outfile, append = TRUE)
}


#' Export BAEA telemetry data as KML
#'
#' Create a Google Earth KML file (points and multitrack) from lat/long
#'   coordinates of BAEA telemetry data. Uses the same functionality as
#'   export_kml(), but all the defaults are set for specific BAEA
#'   telemetry data from Maine
#'
#' @usage export_kml_baea(df, id, datetime, lat, long, alt, alt_mode,
#'   speed, agl, behavior, point_color, point_metadata, point_pal, point_r_pal,
#'   point_b_pal, extrude, path, path_color, path_metadata, path_pal,
#'   path_r_pal, path_b_pal, arrow, icon_by_sex, labelscale, dateformat,
#'   timeformat, datetimeformat, file, output_dir)
#'
#' @param df Input dataframe, must have id, lat, long, and datetime.
#' @param id Column name of unique identifier, data is split into unique paths
#'   and separate folders based on this parameter. Default is "id".
#' @param datetime Column name of datetime in POSIXct format or as a character
#'   in the format (\%Y/\%m/\%d \%H:\%M). Default is "datetime".
#' @param lat Column name of latitude coordinates (WGS84, dec. degree). Default
#'   "lat".
#' @param long Column name of longitude coordinates (WGS84, dec. degree).
#'   Default is "long".
#' @param alt Input dataframe column name for altitude(m). Optional. Default is
#'   "alt".
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "absolute".
#' @param speed input dataframe column name for speed. Optional. Default is
#'   "speed".
#' @param agl input dataframe column name for "altitude above ground level",
#'   optional. Default is NULL.
#' @param behavior input dataframe column name for behavior. Optional. Default
#'   is NULL.
#' @param point_color column name that determines the color for each point, may
#'   be same as 'id' parameter, but may also be sex, behavior, season, etc.
#'   Default is 'deploy_location".
#' @param point_metadata location of metadata .csv file. Metadata file must
#'   have a column that matches name of 'point_color'parameter and "icon_color"
#'   column with hexadecimal colors. Default is:
#'   "Data/GPS/GPS_Deployments.csv".
#' @param point_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param point_r_pal Specifc number of 'R_pal' color palette from the
#'   'PlotKML' Package (e.g., 1 = R_pal[[1]]). This parameter has priority over
#'   the 'b_pal' parameter for setting the colors. Default is NULL.
#' @param point_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param extrude Logical, either FALSE (default) for no line, or TRUE which
#'   extends a line from the point to the ground.
#' @param path Logical, to create Track paths. Default is TRUE.
#' @param path_color Similar to 'point_color' parameter, but the value must
#'   have the same factor level structure as the id file, because each path is
#'   constructed for each id factor. Default is "deploy_location".
#' @param path_metadata Location of metadata .csv file. Metadata file must have
#'   a column that matches name of 'path_color' parameter and an "icon_color"
#'   column with hexadecimal colors. Default is:
#'   "Data/GPS/GPS_Deployments.csv".
#' @param path_pal Name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param path_r_pal Specifc number of 'R_pal' color palette from the 'PlotKML'
#'   Package (e.g., 1 = R_pal[[1]]). This parameter has priority over the
#'   'b_pal' parameter for setting the colors. Default is NULL.
#' @param path_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param arrow Logical, use arrow heads on path icons. Default is TRUE.
#' @param icon_by_sex Logical, use different icons based on "sex" column.
#'   Default is FALSE.
#' @param labelscale Numeric, adjusts the size of the Google Earth location
#'   point labels. Default is 0, which hides the labels. To show labels, change
#'   to a value between 0.7-1.
#' @param dateformat String, changes the format of the date in the Google Earth
#'   location pop-up windows. Default is "\%Y/\%m/\%d".
#' @param timeformat String, changes the format of the time in the Google Earth
#'   locations pop-up windows. Default is "\%I:\%M \%p".
#' @param datetimeformat String, changes the datetime format for the label of
#'   highlighted points. Default is "\%Y/\%m/\%d \%I:\%M \%p"
#' @param file String, filename of output KML file, default is name of input
#'  dataframe. Default is: "BAEA Data.kml"
#' @param output_dir Name for folder in the KML file, default is working
#'   directory. Default is: "C:/Users/Blake/Desktop".
#'
#' @return KML of points and multitracks
#' @export
#'
export_kml_baea <- function (df,
                             id = "id",
                             datetime = "datetime",
                             lat = "lat",
                             long = "long",
                             alt = "alt",
                             alt_mode = "absolute",
                             speed = "speed",
                             agl = NULL,
                             behavior = NULL,
                             point_color = "deploy_location",
                             point_metadata = file.path("Data/GPS",
                               "GPS_Deployments.csv"),
                             point_pal = NULL,
                             point_r_pal = NULL,
                             point_b_pal = "Set1",
                             extrude = FALSE,
                             path = TRUE,
                             path_color = "deploy_location",
                             path_metadata = file.path("Data/GPS",
                               "GPS_Deployments.csv"),
                             path_pal = NULL,
                             path_r_pal = NULL,
                             path_b_pal = NULL,
                             arrow = TRUE,
                             icon_by_sex = FALSE,
                             labelscale = 0,
                             dateformat = "%Y-%m-%d",
                             timeformat = "%I:%M %p",
                             datetimeformat = "%Y-%m-%d %I:%M %p",
                             file = "BAEA Data.kml",
                             output_dir = "Data/KML") {
  export_kml(df=df, id=id, datetime=datetime, lat=lat, long=long,
    alt=alt, alt_mode=alt_mode, speed=speed, agl=agl, behavior=behavior,
    point_color=point_color, point_metadata=point_metadata, point_pal=point_pal,
    point_r_pal=point_r_pal, point_b_pal=point_b_pal, extrude=extrude,
    path=path, path_color=path_color, path_metadata=path_metadata,
    path_pal=path_pal, path_r_pal= path_r_pal, path_b_pal=path_b_pal,
    arrow=arrow, icon_by_sex=icon_by_sex, labelscale=labelscale,
    dateformat=dateformat, timeformat=timeformat, datetimeformat=datetimeformat,
    file=file, output_dir=output_dir)
}


