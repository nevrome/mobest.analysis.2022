library(ggplot2)

load("data/parameter_exploration/variogram/binned_data.RData")

var_model <- function(cov0, rho, g, r) {
  2 * (cov0 - cov0*exp(-(r / rho)^2)) + g
}

p <- ggplot(dplyr::filter(d_binned, time_dist_cut == 500)) +
  geom_line(
    mapping = aes(x = geo_dist_cut, y = mean_sq_pca_dist)
  ) +
  stat_function(
    fun = function(r) {var_model(0.25, 35000, 0.001, r)}, 
    mapping = aes(color = "A"),
    linetype = "dashed", 
    size = 0.9
  ) + 
  stat_function(
    fun = function(r) {var_model(25, 350000, 0.001, r)}, 
    mapping = aes(color = "B"),
    linetype = "dashed", 
    size = 1.2
  ) +
  scale_colour_manual("Models", values = c("red", "blue"), labels = list(
    latex2exp::TeX("$Cov_0 = 0.25, \\rho = 35000$"),
    latex2exp::TeX("$Cov_0 = 25, \\rho = 350000$")
  )) +
  theme_bw() +
  xlab("spatial distance: 100km bins") +
  ylab("mean squared euclidean distance in PC1 & PC2 PCA space")

ggsave(
  "plots/figure_sup_2_variogram_fitting.jpeg",
  plot = p,
  device = "jpeg",
  scale = 0.6,
  dpi = 300,
  width = 300, height = 200, units = "mm",
  limitsize = F
)
