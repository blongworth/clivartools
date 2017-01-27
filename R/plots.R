# function to plot profile from dataframe returned by getProfile
plotProfile <- function(profile) {
    profile %>% mutate(depth = ifelse(depth_corr == -99, depth, depth_corr)) %>%
  ggplot(aes(depth, fm_corr, shape = wheel, color = wheel)) +
    geom_errorbar(aes(ymin=fm_corr-sig_fm_corr,
                      ymax=fm_corr+sig_fm_corr)) + geom_line() +
    geom_point(size=2) + #facet_grid(station ~ .) +
    coord_flip() + scale_x_reverse()
}
