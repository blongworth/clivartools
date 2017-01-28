
# load libraries
library(RODBC)
library(dplyr)
library(ggplot2)
library(amstools)

# read wheel data from database
getProfile <- function(cruise, station) {
  sql <- paste0("SELECT whpid, station, cast, depth, depth_corr, expocode,
                   niskin, wheel, wheel_pos, target.tp_num,
      		         graphite.osg_num, sample_name, graphite.ws_num, ws_method_num, fm_corr, sig_fm_corr
        FROM snics_results
          JOIN target ON snics_results.tp_num = target.tp_num
          JOIN graphite on target.osg_num = graphite.osg_num
          JOIN woce_rec_num on target.rec_num = woce_rec_num.rec_num
          JOIN water_strip on graphite.ws_num = water_strip.ws_num
        WHERE whpid = '", cruise, "'
	        AND station = ", station
  )

  con <- amstools::conNOSAMS()
  clivar <- sqlQuery(con, sql)
  odbcClose(con)

  clivar %>% arrange(depth, wheel)
}

# read cruise ids from database
getCruises <- function() {

  sql <- paste0("SELECT DISTINCT whpid from woce_rec_num")
  con <- amstools::conNOSAMS()
  cruises <- sqlQuery(con, sql)
  odbcClose(con)

  cruises
}

# read stations for a cruise from database
getStations <- function(cruise) {

  # validate cruise
  sql <- paste0("SELECT DISTINCT station from woce_rec_num
                  WHERE whpid = '", cruise, "'")
  con <- amstools::conNOSAMS()
  cruises <- sqlQuery(con, sql)
  odbcClose(con)

  cruises
}

