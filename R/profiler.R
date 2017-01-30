
# load libraries
library(RODBC)
library(dplyr)
library(ggplot2)
library(amstools)

# read wheel data from database
getProfile <- function(cruise, station) {
	# validate input

  sql <- paste0("SELECT whpid, station, cast, depth, depth_corr, expocode,
                   niskin, wheel, wheel_pos, target.tp_num,
                   graphite.osg_num, sample_name, graphite.ws_num,
		   ws_method_num, f_modern, f_ext_error
		 FROM os
		 JOIN target ON os.tp_num = target.tp_num
		 JOIN snics_results ON os.tp_num = snics_results.tp_num
		 JOIN graphite on target.osg_num = graphite.osg_num
		 JOIN woce_rec_num on target.rec_num = woce_rec_num.rec_num
		 JOIN water_strip on graphite.ws_num = water_strip.ws_num
		 WHERE whpid = '", cruise, "'
		   AND station = ", station
  )

  con <- amstools::conNOSAMS()
  data <- sqlQuery(con, sql)
  odbcClose(con)
  amstools::testDB(data)
  data %>% arrange(depth, wheel)
}

# read cruise ids from database
getCruises <- function() {
  
  # TODO: select by date
  sql <- paste0("SELECT DISTINCT whpid FROM woce_rec_num ORDER BY whpid
		   WHERE EXISTS
                      (SELECT * FROM os
                         WHERE woce_rec_num.rec_num = os.rec_num)
		")

  con <- amstools::conNOSAMS()
  cruises <- sqlQuery(con, sql)
  odbcClose(con)

  # TODO: weed out bad entries

  cruises
}

# read stations for a cruise from database
getStations <- function(cruise, hasdata = TRUE) {

  # TODO: validate cruise
  if (hasdata) {
  sql <- paste0("SELECT DISTINCT station from woce_rec_num
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
  stations <- sqlQuery(con, sql)
  odbcClose(con)
  amstools::testDB(stations)
  # TODO: still getting stations with no data

  stations
}

