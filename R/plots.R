#' Plot a depth profile from a station
#'
#' @param profile A datatable in the format of getProfile output
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
# function to plot profile from dataframe returned by getProfile
plotProfile <- function(profile) {
  p <- dplyr::mutate(profile, depth = ifelse(depth_corr == -99, depth, depth_corr))
  ggplot(p, aes(depth, f_modern, shape = wheel, color = wheel)) +
    geom_errorbar(aes(ymin=f_modern-f_ext_error,
                      ymax=f_modern+f_ext_error)) + geom_line() +
    geom_point(size=2) + #facet_grid(station ~ .) +
    coord_flip() + scale_x_reverse()
}

#' Plot CLIVAR data from a wheel
#'
#' @param wheel A wheel name in format: "USAMS010101".
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
# function to plot profile from dataframe returned by getProfile
plotWheelProfile <- function(wheel) {

  # get profile data for a wheel

  #plot using plotProfile
  clivartools::plotProfile(profile)
}
