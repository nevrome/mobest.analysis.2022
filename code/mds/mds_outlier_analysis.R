library(magrittr)

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
  ) %>% 
  dplyr::select(IID, in_95)

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

