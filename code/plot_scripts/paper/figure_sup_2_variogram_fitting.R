library(ggplot2)

load("data/parameter_exploration/variogram/all_distances.RData")

# binning
d_binned <- d_all %>%
  dplyr::mutate(
    geo_dist_cut = cut(
      geo_dist, breaks = c(seq(0, max(geo_dist), 100), max(geo_dist)), 
      include.lowest	= T, labels = F
    ) * 100,
    time_dist_cut = cut(
      time_dist, breaks = c(seq(0, max(time_dist), 100), max(time_dist)), 
      include.lowest	= T, labels = F
    ) * 100
  ) %>%
  dplyr::group_by(geo_dist_cut, time_dist_cut) %>%
  dplyr::summarise(
    n = dplyr::n(),
    C1_resid = mean(C1_dist_resid^2, na.rm = T),
  ) %>%
  dplyr::ungroup()

var_model <- function(cov0, rho, g, r) {
  2 * (cov0 - cov0*exp(-(r / rho)^2)) + g
}

p <- ggplot(dplyr::filter(d_binned, geo_dist_cut == 500)) +
  geom_line(
    mapping = aes(x = time_dist_cut, y = C1_resid)
  ) +
  stat_function(
    fun = function(r) {var_model(0.25, 70000, 0.001, r)}, 
    mapping = aes(color = "A"),
    linetype = "dashed", 
    size = 0.9
  ) + 
  stat_function(
    fun = function(r) {var_model(25, 700000, 0.001, r)}, 
    mapping = aes(color = "B"),
    linetype = "dashed", 
    size = 1.2
  ) +
  scale_colour_manual("Models", values = c("red", "blue"), labels = list(
    latex2exp::TeX("$Cov_0 = 0.25, \\rho = 70000$"),
    latex2exp::TeX("$Cov_0 = 25, \\rho = 700000$")
  )) +
  theme_bw() +
  xlab("temporal distance: 100y bins") +
  ylab("mean squared euclidean distance along C1")

ggsave(
  "plots/figure_sup_2_variogram_fitting.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
