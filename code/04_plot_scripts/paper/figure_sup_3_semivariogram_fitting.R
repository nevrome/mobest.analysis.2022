library(ggplot2)

load("data/parameter_exploration/targeted/binned_distances.RData")

var_model <- function(cov0, rho, g, r) {
  2 * (cov0 - cov0*exp(-(r / rho)^2)) + g
}

p <- ggplot(dplyr::filter(d_binned, geo_dist_cut == 450)) +
  geom_line(
    mapping = aes(x = time_dist_cut, y = C1_mds_u_dist_resid)
  ) +
  stat_function(
    fun = function(r) {var_model(0.10, 100000, 0.00055, r)}, 
    mapping = aes(color = "A"),
    linetype = "dashed", 
    size = 0.9
  ) + 
  stat_function(
    fun = function(r) {var_model(10, 1000000, 0.00055, r)}, 
    mapping = aes(color = "B"),
    linetype = "dashed", 
    size = 1.2
  ) +
  scale_colour_manual("Models", values = c("red", "blue"), labels = list(
    latex2exp::TeX("$Cov_0 = 0.10, \\rho = 100000$"),
    latex2exp::TeX("$Cov_0 = 10, \\rho = 1000000$")
  )) +
  theme_bw() +
  xlab("temporal distance: 100y bins") +
  ylab("half mean squared distance along C1")

ggsave(
  "plots/figure_sup_3_semivariogram_fitting.pdf",
  plot = p,
  device = "pdf",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 130, units = "mm",
  limitsize = F
)
