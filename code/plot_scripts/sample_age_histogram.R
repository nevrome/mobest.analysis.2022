library(magrittr)
library(ggplot2)

load("data/anno_1240K_and_anno_1240K_HumanOrigins_filtered.RData")
anno <- anno_1240K_and_anno_1240K_HumanOrigins_filtered

plot <- ggplot() +
  geom_histogram(
    data = anno,
    aes(x = calage_center),
    binwidth = 100
  ) +
  theme_bw() +
  xlab("time calBC") +
  ylab("number of samples per 100y bin") +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

ggsave(
  paste0("plots/age_distribution_histogram.jpeg"),
  plot = plot,
  device = "jpeg",
  scale = 0.3,
  dpi = 300,
  width = 550, height = 550, units = "mm",
  limitsize = F
)

