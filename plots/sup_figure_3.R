library("tidyverse")
library("viridis")

nucella_shape <- read_delim("../data/shell/shell_shape.csv")
df_sal <- read_delim("../data/env/salinity_comp.csv")

# Median salinity per sampling site
df_sal_m2 <- df_sal %>%
  group_by(location) %>%
  summarise(median_sal = median(sal, na.rm = TRUE))

nucella_sal <- merge(nucella_shape, df_sal_m2, by = "location")
nucella_sal$median_sal <- round(nucella_sal$median_sal, 2)

p1 <-
  ggplot(nucella_sal,
         aes(as.factor(median_sal), aragonite_thickness_mm, fill = median_sal)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 11),
    axis.text.x = element_text(
      size = 11,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(.3, .3, .2, .5), "cm")
  ) +
  ylab(expression(atop(Aragonite ~ layer, thickness ~ "(mm)"))) +
  ylim(0, 0.35) +
  xlab(expression(paste(Salinity ~ "(psu)")))

p2 <-
  ggplot(nucella_sal,
         aes(as.factor(median_sal), calcite_thickness_mm, fill = median_sal)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 11),
    axis.text.x = element_text(
      size = 11,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(.3, .3, .2, .5), "cm")
  ) +
  ylab(expression(atop(Calcite ~ layer, thickness ~ "(mm)"))) +
  ylim(0.2, 1.5) +
  xlab(expression(paste(Salinity ~ "(psu)")))

p3 <-
  ggplot(
    nucella_sal,
    aes(
      as.factor(median_sal),
      calcite_thickness_mm / aragonite_thickness_mm,
      fill = median_sal
    )
  ) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 11),
    axis.text.x = element_text(
      size = 11,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 11),
    plot.margin = unit(c(.3, .3, .2, .5), "cm")
  ) +
  ylab(expression(atop(
    Calcite ~ "/" ~ Aragonite, layer ~ thickness ~ "(mm)"
  ))) +
  ylim(0, 20) +
  xlab(expression(paste(Salinity ~ "(psu)")))

pdf("sup_figure_3.pdf", height = 7, onefile = F)
cowplot::plot_grid(
  p1,
  p2,
  p3,
  ncol = 2,
  label_size = 11,
  labels = c("(a)", "(b)", "(c)")
)
dev.off()
