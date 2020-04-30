library(magrittr)
library(ggplot2)

mds <- readr::read_delim("data/mds/1240K_HumanOrigins.pruned.mds", " ", trim_ws = T) %>%
  dplyr::select(-X8)

inquant <- function(x) {
  x >= quantile(x, probs = 0.025) & x <= quantile(x, probs = 0.975)
}

mds <- mds %>%
  dplyr::mutate(
    C1_in_95 = inquant(C1),
    C2_in_95 = inquant(C2),
    C3_in_95 = inquant(C3),
    C4_in_95 = inquant(C4),
    in_95 = C1_in_95 & C2_in_95 & C3_in_95 & C4_in_95
  )

load("data/anno_1240K.RData")
anno_1240K

mds2 <- mds %>% dplyr::left_join(
  anno_1240K, by = c("IID" = "instance_id")
)

# SG vs. Capture (data_type)

hu <- mds2 %>%
  dplyr::filter(data_type %in% c("Shotgun", "1240K")) %>%
  dplyr::group_by(
    data_type, in_95
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
  ) %>%
  dplyr::group_by(
    data_type
  ) %>% 
  dplyr::mutate(
    percent = round(n / sum(n), 2),
  )

p1 <- hu %>% ggplot(aes(x = data_type, y = n, fill = in_95)) +
  geom_bar(
    position = position_stack(),
    stat = "identity"
  ) +
  geom_text(
    aes(label = percent), position = position_stack(vjust = 0.5), size = 5
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle("Shotgun vs. Capture")

# coverage

p2 <- mds2 %>%
  ggplot(aes(x = in_95, y = coverage, fill = in_95)) +
  geom_jitter(width = 0.3, alpha = 0.2) +
  geom_boxplot(width = 0.1) +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  guides(fill = FALSE) +
  ggtitle("Coverage")

# snps_hit_on_autosomes

p3 <- mds2 %>%
  ggplot(aes(x = in_95, y = snps_hit_on_autosomes, fill = in_95)) +
  geom_jitter(width = 0.3, alpha = 0.2) +
  geom_boxplot(width = 0.1) +
  scale_y_log10(labels = scales::comma) +
  theme_bw() +
  guides(fill = FALSE) +
  ggtitle("# SNPs on autosomes")

# merge plots

p <- cowplot::plot_grid(p1, p2, p3, nrow = 1)

ggsave(
  paste0("plots/mds_outlier_analysis.jpeg"),
  plot = p,
  device = "jpeg",
  scale = 0.7,
  dpi = 300,
  width = 400, height = 230, units = "mm",
  limitsize = F
)

# linear model

out_meas <- function(x) {
  low_quant <- quantile(x, probs = 0.025)
  up_quant <- quantile(x, probs = 0.975)
  ifelse(x < low_quant, abs(x - low_quant), ifelse(x > up_quant, abs(up_quant - x), 0))
}

ou <- mds2 %>%
  dplyr::mutate(
    C1_out = out_meas(C1),
    C2_out = out_meas(C2),
    C3_out = out_meas(C3),
    C4_out = out_meas(C4)
  ) %>% 
  dplyr::filter(!C1_in_95, C1_out < 0.05)
  

ou %>%
  ggplot(aes(x = coverage, y = C1_out)) +
  geom_point() +
  geom_smooth(method = "lm")

ou %>%
  ggplot(aes(x = snps_hit_on_autosomes, y = C1_out)) +
  geom_point() +
  geom_smooth(method = "lm")

prior_sample_1 <- tibble::tibble(
  alpha = rnorm(100, median(ou$C1_out), sd(ou$C1_out)),
  beta = rnorm(100, 0.00000001, 0.00000002)
)

ggplot() +
  geom_point(
    data = ou,
    mapping =  aes(x = snps_hit_on_autosomes, y = C1_out)
  ) +
  geom_abline(
    intercept = prior_sample_1$alpha,
    slope = prior_sample_1$beta,
    alpha = 0.1,
    color = "red"
  )

prior_sample_2 <- tibble::tibble(
  alpha = rnorm(100, median(ou$C1_out), sd(ou$C1_out)),
  beta = rnorm(100, 0.001, 0.002)
)

ggplot() +
  geom_point(
    data = ou,
    mapping =  aes(x = coverage, y = C1_out)
  ) +
  geom_abline(
    intercept = prior_sample_2$alpha,
    slope = prior_sample_2$beta,
    alpha = 0.1,
    color = "red"
  )

fit <- rstan::stan(
  file = "code/mds/lm.stan", 
  data = list(
    N = nrow(ou),
    x1 = ou$snps_hit_on_autosomes,
    x2 = ou$coverage,
    y = ou$C1_out,
    alpha_mean = median(ou$C1_out),
    alpha_sd = sd(ou$C1_out),
    beta_x1_mean = 0.00000001,
    beta_x1_sd = 0.00000002,
    beta_x2_mean = 0.001,
    beta_x2_sd = 0.002
  ),
  chains = 1,
  cores = 1
)

ex <- rstan::extract(fit)

ggplot() +
  geom_point(
    data = ou,
    mapping =  aes(x = snps_hit_on_autosomes, y = C1_out)
  ) +
  geom_abline(
    intercept = ex$alpha,
    slope = ex$beta_x1,
    alpha = 0.01,
    color = "red"
  )

ggplot() +
  geom_point(
    data = ou,
    mapping =  aes(x = coverage, y = C1_out)
  ) +
  geom_abline(
    intercept = ex$alpha,
    slope = ex$beta_x2,
    alpha = 0.01,
    color = "red"
  )


