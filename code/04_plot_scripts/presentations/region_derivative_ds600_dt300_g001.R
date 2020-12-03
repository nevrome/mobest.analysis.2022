library(magrittr)
library(ggplot2)

load("data/gpr/temporal_change_age_resampling.RData")

#### derivative estimator curves ####

p_estimator <- ggplot() +
  geom_line(
    data = iwrs_age_resampling_run,
    aes(
      x = z, y = mean_change_combined, 
      color = gpr_mean_sd_norm,
      group = independent_table_id
    )
  ) +
  geom_line(
    data = iwrs_age_total,
    aes(
      x = z, y = movavg_mean#, alpha = 1 - mean_sd_norm
    ),
    color = "#228B95", size = 1
  ) +
  geom_ribbon(
    data = iwrs_age_total,
    aes(
      x = z, ymin = movavg_mean - movavg_sd, ymax = movavg_mean + movavg_sd
    ),
    fill = "#228B95", alpha = 0.4
  ) +
  facet_wrap(dplyr::vars(region_id)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 40, hjust = 1),
    strip.text = element_text(size = 12), 
    legend.text = element_text(size = 10)
  ) +
  xlab("time in years calBC/calAD") +
  ylab("Change in MDS space [Euclidean MDS distance/50 years]") +
  scale_color_gradientn(
    colours = c("red", "grey", "grey")
  ) +
  guides(
    color = guide_colorbar(title = "Mean GPR SD", barwidth = 20)
  ) +
  scale_x_continuous(breaks = c(-7000, -5000, -3000, -1000, 1000)) +
  scale_alpha_continuous(guide = FALSE) +
  coord_cartesian(ylim = c(0, 0.02))

shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

p_estimator_fixed_legend_position <- shift_legend3(p_estimator)

ggsave(
  paste0("plots/region_derivative_ds600_dt300_g001.png"),
  plot = p_estimator_fixed_legend_position,
  device = "png",
  scale = 0.3,
  dpi = 300,
  width = 1000, height = 450, units = "mm",
  limitsize = F
)

