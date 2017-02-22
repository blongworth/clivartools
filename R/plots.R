#' Plot a depth profile from a station
#'
#' @param profile A datatable in the format of getProfile output
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
# function to plot profile from dataframe returned by getProfile
plotProfile <- function(profile) {
    profile %>% dplyr::mutate(depth = ifelse(depth_corr == -99, depth, depth_corr)) %>%
  ggplot(aes(depth, f_modern, shape = wheel, color = wheel)) +
    geom_errorbar(aes(ymin=f_modern-f_ext_error,
                      ymax=f_modern+f_ext_error)) + geom_line() +
    geom_point(size=2) + #facet_grid(station ~ .) +
    coord_flip() + scale_x_reverse()
}

#' Plot a depth profile from a station
#'
#' @param profile A datatable in the format of getProfile output
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
# function to plot profile from dataframe returned by getProfile
plotWheelProfile <- function(profile) {
    profile %>% dplyr::mutate(depth = ifelse(depth_corr == -99, depth, depth_corr)) %>%
  ggplot(aes(depth, f_modern)) +
    geom_errorbar(aes(ymin=f_modern-f_ext_error,
                      ymax=f_modern+f_ext_error)) + geom_line() +
    geom_point(size=2) + facet_grid(station ~ .) +
    coord_flip() + scale_x_reverse()
}
