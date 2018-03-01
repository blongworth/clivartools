# General funtions for pulling profiles from database

# TODO: function to get profiles for a wheel

#' Read CLIVAR profile data for a wheel from DB
#'
#' @param wheel A character value with the wheel name
#'
#' @return A data frame with profile data
#' @export
getWheelCLIVAR <- function(wheel) {
	# validate input

  sql <- paste0("SELECT whpid, station, cast, depth, depth_corr, expocode,
                   niskin, wheel, wheel_pos, target.tp_num,
                   graphite.osg_num, sample_name, graphite.ws_num,
		   ws_method_num, fm_corr, sig_fm_corr
		 FROM snics_results
		 JOIN target ON snics_results.tp_num = target.tp_num
		 JOIN graphite on target.osg_num = graphite.osg_num
		 JOIN woce_rec_num on target.rec_num = woce_rec_num.rec_num
		 JOIN water_strip on graphite.ws_num = water_strip.ws_num
		 WHERE wheel = '", wheel, "'"
  )

  con <- amstools::conNOSAMS()
  data <- RODBC::sqlQuery(con, sql)
  RODBC::odbcClose(con)
  amstools::checkDB(data)
  dplyr::arrange(data, whpid, station, depth) %>%
    dplyr::mutate(f_modern = fm_corr, f_ext_error = sig_fm_corr)
}

#' Read CLIVAR profile data from DB
#'
#' @param cruise A character value with the whpid cruise name
#' @param station A numeric with the station number. Retrieve
#' all stations if not given.
#'
#' @return A data frame with profile data
#' @export
#' @importFrom dplyr arrange mutate "%>%"
getProfile <- function(cruise, station) {
	# validate input

  sql <- paste0("SELECT woce_rec_num.whpid,
                   woce_rec_num.station,
                   woce_rec_num.cast, depth, depth_corr,
                   woce_rec_num.expocode, collection_date,
                   latitude, longitude, ws_delta_c13,
                   niskin, wheel_id, wheel_position, target.tp_num,
                   graphite.osg_num, target_name, graphite.ws_num,
                   ws_method_num, f_modern , f_int_error, f_ext_error
                 FROM os
                 JOIN target ON os.tp_num = target.tp_num
                 JOIN wheel_pos ON os.tp_num = wheel_pos.tp_num
                 JOIN graphite ON target.osg_num = graphite.osg_num
                 JOIN woce_rec_num ON target.rec_num = woce_rec_num.rec_num
                 JOIN woce_loc ON woce_rec_num.expocode = woce_loc.expocode
                   AND woce_rec_num.station = woce_loc.station
                   AND woce_rec_num.cast = woce_loc.cast
                 JOIN water_strip ON graphite.ws_num = water_strip.ws_num
                 WHERE woce_rec_num.whpid IN (", paste(shQuote(cruise, type = "sh"),
            		                          collapse = ", "), ")")

  if (!missing(station)) {
    sql <- paste(sql, "AND woce_rec_num.station IN (",paste(station, collapse = ", "), ")")
  }

  con <- amstools::conNOSAMS()
  data <- RODBC::sqlQuery(con, sql)
  RODBC::odbcClose(con)
  amstools::checkDB(data)

  data %>%
    arrange(depth, wheel_id) %>%
    mutate(rep_err = pmax(f_int_error, f_ext_error))
}

#' Read CLIVAR profile data from snics_results in DB
#'
#' @param cruise A character value with the whpid cruise name
#' @param station A numeric with the station number
#'
#' @return A data frame with profile data
#' @export
getProfileSR <- function(cruise, station) {
	# validate input

  sql <- paste0("SELECT whpid, station, cast, depth, depth_corr, expocode,
                   niskin, wheel, wheel_pos, target.tp_num,
                   graphite.osg_num, sample_name, graphite.ws_num,
		   ws_method_num, fm_corr, sig_fm_corr
		 FROM snics_results
		 JOIN target ON snics_results.tp_num = target.tp_num
		 JOIN graphite on target.osg_num = graphite.osg_num
		 JOIN woce_rec_num on target.rec_num = woce_rec_num.rec_num
		 JOIN water_strip on graphite.ws_num = water_strip.ws_num
		 WHERE whpid = '", cruise, "'
		   AND station = ", station
  )

  con <- amstools::conNOSAMS()
  data <- RODBC::sqlQuery(con, sql)
  RODBC::odbcClose(con)
  amstools::checkDB(data)
  dplyr::arrange(data, depth, wheel)
}

#' Read cruise ids from database
#'
#' @return A data frame with cruise names (whpid)
#' @export
getCruises <- function() {

  # TODO: select by date
  sql <- paste0("SELECT DISTINCT whpid FROM woce_rec_num
		               WHERE EXISTS
                     (SELECT * FROM os
                        WHERE woce_rec_num.rec_num = os.rec_num)
	                 ORDER BY whpid")

  con <- amstools::conNOSAMS()
  cruises <- RODBC::sqlQuery(con, sql)
  RODBC::odbcClose(con)

  cruises <- as.character(cruises$whpid)
  # TODO: weed out bad entries
  cruises <- cruises[!is.na(cruises)]
  cruises[cruises != " "]
}

#' Read stations for a cruise from database
#'
#' @param cruise A character vector with cruise name (whpid)
#' @param hasdata Checks that data is in os table if TRUE
#'
#' @return A data frame with station names
#' @export
getStations <- function(cruise, hasdata = TRUE) {

  # TODO: validate cruise

  stopifnot(is.character(cruise))

  if (hasdata) {
  sql <- paste0("SELECT DISTINCT station FROM woce_rec_num
                  WHERE EXISTS
                      (SELECT * FROM os
                         WHERE woce_rec_num.rec_num = os.rec_num
                         AND whpid = '", cruise, "')
                  ORDER BY station")
  } else {
  sql <- paste0("SELECT DISTINCT station from woce_rec_num
                  WHERE whpid = '", cruise, "'
                  ORDER BY station")

  }
  con <- amstools::conNOSAMS()
  stations <- RODBC::sqlQuery(con, sql)
  RODBC::odbcClose(con)
  amstools::checkDB(stations)
  # TODO: still getting stations with no data

  stations$station
}

