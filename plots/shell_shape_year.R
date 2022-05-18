library("tidyverse")
library("patchwork")
library("scales")
library("ggsci")
library("grid")
library("Momocs")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
nucella_shape <- read_delim("data/shell/shell_shape.csv")
gam_PCs <- readRDS("data/shell/model_out/GAM_shape.rds")
nucella_coe <- readRDS("data/shell/shape_out/shell_coe_object.rds")

# Prepare shell shape plots -----------------------------------------------
nucella_pca <- PCA(nucella_coe)

# PC Contribution Extreme Plot
PC_contrib <- PCcontrib(nucella_pca, nax = 1:5, sd.r = c(-3, 0, 3))
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
  scale_color_manual(values = c("#003C67FF", "#CD534CFF")) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC1$x)),
    y = mean(range(PC1$y)),
    fontface = "plain",
    size = 3,
    label = paste("PC 1 \n", round(nucella_pca$eig[1] * 100, 2), "%", sep = "")
  ) +
  theme(text = element_text(size = 10, family="Times")) +
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
  scale_color_manual(values = c("#003C67FF", "#CD534CFF")) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC2$x)),
    y = mean(range(PC2$y)),
    fontface = "plain",
    size = 3,
    label = paste("PC 2 \n", round(nucella_pca$eig[2] * 100, 2), "%", sep = "")
  ) +
  theme(text = element_text(size = 10, family="Times")) +
  xlab(NULL) +
  ylab(NULL)

PC.1.3 <- PC_contrib$shp$`1.3`[1:2]
PC.1.3$id <- "1.3"
PC.3.3 <- PC_contrib$shp$`3.3`[1:2]
PC.3.3$id <- "3.3"
PC3 <- rbind(PC.1.3, PC.3.3)

gg3 <- ggplot(PC3, aes(x, y, colour = id)) +
  geom_polygon(size = 0.5, fill = NA) +
  theme_harmony() +
  scale_color_manual(values = c("#003C67FF", "#CD534CFF")) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC2$x)),
    y = mean(range(PC2$y)),
    fontface = "plain",
    size = 3,
    label = paste("PC 3 \n", round(nucella_pca$eig[2] * 100, 2), "%", sep = "")
  ) +
  theme(text = element_text(size = 10, family="Times")) +
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
  scale_color_manual(values = c("#003C67FF", "#CD534CFF")) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC4$x)),
    y = mean(range(PC4$y)),
    fontface = "plain",
    size = 3,
    label = paste("PC 4 \n", round(nucella_pca$eig[4] * 100, 2), "%", sep = "")
  ) +
  theme(text = element_text(size = 10, family="Times")) +
  xlab(NULL) +
  ylab(NULL)

PC.1.5 <- PC_contrib$shp$`1.5`[1:2]
PC.1.5$id <- "1.5"
PC.3.5 <- PC_contrib$shp$`3.5`[1:2]
PC.3.5$id <- "3.5"
PC5 <- rbind(PC.1.5, PC.3.5)

gg5 <- ggplot(PC5, aes(x, y, colour = id)) +
  geom_polygon(size = 0.5, fill = NA) +
  theme_harmony() +
  scale_color_manual(values = c("#003C67FF", "#CD534CFF")) +
  coord_fixed() +
  annotate(
    "text",
    x = mean(range(PC4$x)),
    y = mean(range(PC4$y)),
    fontface = "plain",
    size = 3,
    label = paste("PC 5 \n", round(nucella_pca$eig[4] * 100, 2), "%", sep = "")
  ) +
  theme(text = element_text(size = 10, family="Times")) +
  xlab(NULL) +
  ylab(NULL)

# Predict shape PCs from GAM ----------------------------------------------

# year
pdat_year <- expand.grid(year = seq(min(nucella_shape$year), max(nucella_shape$year), 
                              by = ((max(nucella_shape$year) - min(nucella_shape$year))/(5000 - 1))),
                    shell_height = mean(nucella_shape$shell_height))

pdat_year <- data.frame(year = rep(pdat_year$year, 5), 
                       shell_height = rep(pdat_year$shell_height, 5), 
                       location = "ZEE",
                       fshape = rep(c("PC1", "PC2", "PC3", "PC4", "PC5"), each = 5000))

pred_year <- predict(gam_PCs, newdata = pdat_year, exclude = c("s(location)"), 
                    type = "response", se.fit = T)

predframe_year <- data.frame(year = pdat_year$year,
                            shell_height = pdat_year$shell_height, 
                            fshape = pdat_year$fshape, 
                            preds = pred_year$fit, se = pred_year$se.fit)

#shell height
pdat_sh <- expand.grid(shell_height = seq(min(nucella_shape$shell_height), max(nucella_shape$shell_height), 
                                    by = ((max(nucella_shape$shell_height) - min(nucella_shape$shell_height))/(5000 - 1))),
                         year = mean(nucella_shape$year))

pdat_sh <- data.frame(year = rep(pdat_sh$year, 5), 
                        shell_height = rep(pdat_sh$shell_height, 5), 
                        location = "ZEE",
                        fshape = rep(c("PC1", "PC2", "PC3", "PC4", "PC5"), each = 5000))

pred_sh <- predict(gam_PCs, newdata = pdat_sh, exclude = c("s(location)"), 
                     type = "response", se.fit = T)

predframe_sh <- data.frame(year = pdat_sh$year,
                             shell_height = pdat_sh$shell_height, 
                             fshape = pdat_sh$fshape, 
                             preds = pred_sh$fit, se = pred_sh$se.fit)



# Plot figures ------------------------------------------------------------
scaleFUN <- function(x) sprintf("%.1f", x)
p1 <- predframe_year %>% 
  filter(fshape=="PC1") %>% 
  ggplot() + 
  geom_smooth(aes(year, preds, color=..y..), lwd = 0.5, se = F) +
  geom_smooth(aes(year, preds + 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  geom_smooth(aes(year, preds - 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  theme_science() + 
  scale_colour_gradient(low = "#003C67FF", high = "#CD534CFF") +
  scale_y_continuous(limits = c(0.2,0.7)) +
  theme(text = element_text(size = 10, family="Times"),
        axis.title.x = element_blank(),
        legend.position = "none") + 
  
  xlab("year") + ylab("shape-PC1")

p2 <- predframe_year %>% 
  filter(fshape=="PC2") %>% 
  ggplot() + 
  geom_smooth(aes(year, preds, color=..y..), lwd = 0.5, se = F) +
  geom_smooth(aes(year, preds + 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  geom_smooth(aes(year, preds - 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  theme_science() + 
  scale_colour_gradient(low = "#003C67FF", high = "#CD534CFF") +
  scale_y_continuous(limits = c(0.2,0.7)) +
  theme(text = element_text(size = 10, family="Times"),
        axis.title.x = element_blank(),
        legend.position = "none") + 
  xlab("year") + ylab("shape-PC2")

p3 <- predframe_year %>% 
  filter(fshape=="PC3") %>% 
  ggplot() + 
  geom_smooth(aes(year, preds, color=..y..), lwd = 0.5, se = F) +
  geom_smooth(aes(year, preds + 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  geom_smooth(aes(year, preds - 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  theme_science() + 
  scale_colour_gradient(low = "#003C67FF", high = "#CD534CFF") +
  scale_y_continuous(limits = c(0.2,0.7)) +
  theme(text = element_text(size = 10, family="Times"),
        axis.title.x = element_blank(),
        legend.position = "none") + 
  xlab("year") + ylab("shape-PC3")

p4 <- predframe_year %>% 
  filter(fshape=="PC4") %>% 
  ggplot() + 
  geom_smooth(aes(year, preds, color=..y..), lwd = 0.5, se = F) +
  geom_smooth(aes(year, preds + 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  geom_smooth(aes(year, preds - 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  theme_science() + 
  scale_colour_gradient(low = "#003C67FF", high = "#CD534CFF") +
  scale_y_continuous(limits = c(0.2,0.7)) +
  theme(text = element_text(size = 10, family="Times"),
        axis.title.x = element_blank(),
        legend.position = "none") + 
  xlab("year") + ylab("shape-PC4")

p5 <- predframe_year %>% 
  filter(fshape=="PC5") %>% 
  ggplot() + 
  geom_smooth(aes(year, preds, color=..y..), lwd = 0.5, se = F) +
  geom_smooth(aes(year, preds + 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  geom_smooth(aes(year, preds - 1.96 * se), col = 1, lty = 2, lwd = 0.5, se = F, alpha=0.7) +
  theme_science() + 
  scale_colour_gradient(low = "#003C67FF", high = "#CD534CFF") +
  scale_y_continuous(limits = c(0.2,0.7)) +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") + 
  xlab("year") + ylab("shape-PC5")

pc_panel_l <- (gg1 / gg2 / gg3 /gg4 / gg5) + plot_layout(heights = c(1,1,1,1,0.9))

eff_panel_r <- (p1 / p2 / p3 / p4 / p5) + plot_layout(heights = c(1,1,1,1,1))

png("plots/shell_shape_year.png", width = 80, height = 180, unit = "mm", res = 1200)

(pc_panel_l | eff_panel_r) + plot_layout(widths = c(0.6,1))

grid.text("(a)", x = 0.075, y = 0.96, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(b)", x = 0.075, y = 0.77, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(c)", x = 0.075, y = 0.575, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(d)", x = 0.075, y = 0.385, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(e)", x = 0.075, y = 0.195, gp=gpar(fontsize=10, fontfamily = "Times"))

summary(gam_PCs)

grid.text("n.s.", x = 0.88, y = 0.96, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("*", x = 0.88, y = 0.758, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.58, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.385, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.195, gp=gpar(fontsize=10, fontfamily = "Times"))

dev.off()

# Export to vector for journal publication
library(Cairo)
cairo_ps("plots/shell_shape_year.eps", family = "Times", width = 80/25.4, height = 180/25.4)
(pc_panel_l | eff_panel_r) + plot_layout(widths = c(0.6,1))

grid.text("(a)", x = 0.075, y = 0.96, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(b)", x = 0.075, y = 0.77, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(c)", x = 0.075, y = 0.575, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(d)", x = 0.075, y = 0.385, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("(e)", x = 0.075, y = 0.195, gp=gpar(fontsize=10, fontfamily = "Times"))

summary(gam_PCs)

grid.text("n.s.", x = 0.88, y = 0.96, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("*", x = 0.88, y = 0.758, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.58, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.385, gp=gpar(fontsize=10, fontfamily = "Times"))
grid.text("n.s.", x = 0.88, y = 0.195, gp=gpar(fontsize=10, fontfamily = "Times"))

dev.off()
