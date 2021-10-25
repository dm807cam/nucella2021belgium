library("tidyverse")
library("cowplot")
library("scales")
library("RColorBrewer")
library("Momocs")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("../imp_func.R", local = TRUE)

# Import data
nucella_shape <- read_delim("../data/shell/shell_shape.csv")
gam_PC1 <- readRDS("../data/shell/GAM_shape_PC1.rds")
gam_PC2 <- readRDS("../data/shell/GAM_shape_PC2.rds")
gam_PC3 <- readRDS("../data/shell/GAM_shape_PC3.rds")
gam_PC4 <- readRDS("../data/shell/GAM_shape_PC4.rds")
nucella_coe <- readRDS("../data/shell/nucella_coe_object.rds")

nucella_pca <- PCA(nucella_coe)

# PC Contribution Extreme Plot
PC_contrib <- PCcontrib(nucella_pca, nax = 1:4, sd.r = c(-3, 0, 3))
# Plot PC contribution
PC_contrib$gg + geom_polygon(aes(fill = shp), colour = "black", size = 1) +
  theme_harmony() +
  scale_fill_viridis() +
  ylab("PCs") +
  xlab(expression(paste("Mean" %+-% "3" * sigma)))

PC.1.1 <- PC_contrib$shp$`1.1`[1:2]
PC.1.1$id <- "a"
PC.3.1 <- PC_contrib$shp$`3.1`[1:2]
PC.3.1$id <- "b"
PC1 <- rbind(PC.1.1, PC.3.1)

gg1 <- ggplot(PC1, aes(x, y, colour = id)) +
  geom_polygon(size = 0.5, fill = NA) +
  theme_harmony() +
  scale_color_manual(values = c(
    brewer.pal(n = 4, name = "Spectral")[1],
    brewer.pal(n = 4, name = "Spectral")[4]
  )) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC1$x)),
    y = mean(range(PC1$y)),
    fontface = "plain",
    size = 2,
    label = paste("PC 1 \n", round(nucella_pca$eig[1] * 100, 2), "%", sep = "")
  ) +
  xlab(NULL) +
  ylab(NULL)

PC.1.2 <- PC_contrib$shp$`1.2`[1:2]
PC.1.2$id <- "1.2"
PC.3.2 <- PC_contrib$shp$`3.2`[1:2]
PC.3.2$id <- "3.2"
PC2 <- rbind(PC.1.2, PC.3.2)

gg2 <- ggplot(PC2, aes(x, y, colour = id)) +
  geom_polygon(size = 0.5, fill = NA) +
  theme_harmony() +
  scale_color_manual(values = c(
    brewer.pal(n = 4, name = "Spectral")[1],
    brewer.pal(n = 4, name = "Spectral")[4]
  )) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC2$x)),
    y = mean(range(PC2$y)),
    fontface = "plain",
    size = 2,
    label = paste("PC 2 \n", round(nucella_pca$eig[2] * 100, 2), "%", sep = "")
  ) +
  xlab(NULL) +
  ylab(NULL)

PC.1.4 <- PC_contrib$shp$`1.4`[1:2]
PC.1.4$id <- "1.4"
PC.3.4 <- PC_contrib$shp$`3.4`[1:2]
PC.3.4$id <- "3.4"
PC4 <- rbind(PC.1.4, PC.3.4)

gg4 <- ggplot(PC4, aes(x, y, colour = id)) +
  geom_polygon(size = 0.5, fill = NA) +
  theme_harmony() +
  scale_color_manual(values = c(
    brewer.pal(n = 4, name = "Spectral")[1],
    brewer.pal(n = 4, name = "Spectral")[4]
  )) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC4$x)),
    y = mean(range(PC4$y)),
    fontface = "plain",
    size = 2,
    label = paste("PC 4 \n", round(nucella_pca$eig[4] * 100, 2), "%", sep = "")
  ) +
  xlab(NULL) +
  ylab(NULL)

# Predict shape PC1
pdat_pc1 <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = median(nucella_shape$shell_height),
                   location = "ZEE"
                 ))

pred_pc1 <- predict(
  gam_PC1,
  newdata = pdat_pc1,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_pc1 <- data.frame(
  year = pdat_pc1$year,
  pred_pc1 = pred_pc1$fit,
  se_pc1 = pred_pc1$se.fit
)

# Predict shape PC2
pdat_pc2 <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = median(nucella_shape$shell_height),
                   location = "ZEE"
                 ))

pred_pc2 <- predict(
  gam_PC2,
  newdata = pdat_pc2,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_pc2 <- data.frame(
  year = pdat_pc2$year,
  pred_pc2 = pred_pc2$fit,
  se_pc2 = pred_pc2$se.fit
)

# Predict shape PC3
pdat_pc3 <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = median(nucella_shape$shell_height),
                   location = "ZEE"
                 ))

pred_pc3 <- predict(
  gam_PC3,
  newdata = pdat_pc3,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_pc3 <- data.frame(
  year = pdat_pc3$year,
  pred_pc3 = pred_pc3$fit,
  se_pc3 = pred_pc3$se.fit
)

# Predict shape PC4
pdat_pc4 <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = median(nucella_shape$shell_height),
                   location = "ZEE"
                 ))

pred_pc4 <- predict(
  gam_PC4,
  newdata = pdat_pc4,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_pc4 <- data.frame(
  year = pdat_pc4$year,
  pred_pc4 = pred_pc4$fit,
  se_pc4 = pred_pc4$se.fit
)

sum_pc1 <- summary(gam_PC1)
sub1_1 <- ggplot(PC.1.1, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub2_1 <- ggplot(PC.3.1, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub_p1 <-
  plot_grid(sub2_1,
            NULL,
            sub1_1,
            rel_heights = c(.3, .4, .3),
            ncol = 1)

p1 <- ggplot() +
  geom_line(predframe_pc1,
            mapping = aes(year, pred_pc1),
            linetype = 2) +
  geom_ribbon(
    predframe_pc1,
    mapping = aes(year,
                  ymin = pred_pc1 - se_pc1,
                  ymax = pred_pc1 + se_pc1),
    alpha = 0.3
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  ) +
  theme_science() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  ) +
  ylab("shape-PC1")

pan_1 <- plot_grid(p1, sub_p1, rel_widths = c(.8, .2), align = "h")

sum_pc2 <- summary(gam_PC2)
sub1_2 <- ggplot(PC.1.2, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub2_2 <- ggplot(PC.3.2, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub_p2 <-
  plot_grid(sub2_2,
            NULL,
            sub1_2,
            rel_heights = c(.3, .4, .3),
            ncol = 1)

p2 <- ggplot() +
  geom_line(predframe_pc2,
            mapping = aes(year, pred_pc2),
            linetype = 2) +
  geom_ribbon(
    predframe_pc2,
    mapping = aes(year,
                  ymin = pred_pc2 - se_pc2,
                  ymax = pred_pc2 + se_pc2),
    alpha = 0.3
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  ) +
  theme_science() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  ) +
  ylab("shape-PC2")

pan_2 <- plot_grid(p2, sub_p2, rel_widths = c(.8, .2), align = "h")

sum_pc4 <- summary(gam_PC4)
sub1_4 <- ggplot(PC.1.4, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub2_4 <- ggplot(PC.3.4, aes(x, y)) +
  geom_polygon(fill = "lightgrey",
               size = 0.3,
               colour = "black") +
  theme_void() +
  coord_fixed()
sub_p4 <-
  plot_grid(sub2_4,
            NULL,
            sub1_4,
            rel_heights = c(.3, .4, .3),
            ncol = 1)

p4 <- ggplot() +
  geom_line(predframe_pc4,
            mapping = aes(year, pred_pc4),
            linetype = 2) +
  geom_ribbon(
    predframe_pc4,
    mapping = aes(year,
                  ymin = pred_pc4 - se_pc4,
                  ymax = pred_pc4 + se_pc4),
    alpha = 0.3
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(c)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  ) +
  theme_science() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  ) +
  ylab("shape-PC4")

pan_4 <- plot_grid(p4, sub_p4, rel_widths = c(.8, .2), align = "h")

ggs <- cowplot::plot_grid(
  gg1,
  gg2,
  gg4,
  NULL,
  ncol = 2,
  label_fontface = "plain",
  labels = c("(d)", "", "", ""),
  label_size = 10
)

pdf("figure_5.pdf",
    height = 3.5,
    width = 4.5,
    onefile = F)
cowplot::plot_grid(pan_1, pan_2, pan_4, ggs,
                   ncol = 2)
dev.off()
