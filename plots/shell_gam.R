library("tidyverse")
library("patchwork")
library("scales")
library("RColorBrewer")
library("magick")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
nucella_shape <- read_delim("data/shell/shell_shape.csv")
gam_aperture_size <- readRDS("data/shell/model_out/GAM_aperture_size.rds")
gam_aragonite <-
  readRDS("data/shell/model_out/GAM_aragonite_thickness.rds")
gam_calcite <- readRDS("data/shell/model_out/GAM_calcite_thickness.rds")

# Predict aperture size ---------------------------------------------------
pdat_apt <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = mean(nucella_shape$shell_height),
                   location = "ZEE"
                 ))

pred_apt <- predict(
  gam_aperture_size,
  newdata = pdat_apt,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_apt <- data.frame(
  year = pdat_apt$year,
  pred_apt = pred_apt$fit,
  se_apt = pred_apt$se.fit
)


# Predict aragonite layer thickness ---------------------------------------
pdat_aragonite <- with(nucella_shape,
                       data.frame(
                         year = seq(min(year), max(year), length = 1000),
                         shell_height = mean(nucella_shape$shell_height),
                         location = "ZEE"
                       ))

pred_aragonite <- predict(
  gam_aragonite,
  newdata = pdat_aragonite,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_aragonite <- data.frame(
  year = pdat_aragonite$year,
  pred_aragonite = pred_aragonite$fit,
  se_aragonite = pred_aragonite$se.fit
)


# Predict calcite layer thickness -----------------------------------------
pdat_calcite <- with(nucella_shape,
                     data.frame(
                       year = seq(min(year), max(year), length = 1000),
                       shell_height = median(nucella_shape$shell_height),
                       location = "ZEE"
                     ))

pred_calcite <- predict(
  gam_calcite,
  newdata = pdat_calcite,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_calcite <- data.frame(
  year = pdat_calcite$year,
  pred_calcite = pred_calcite$fit,
  se_calcite = pred_calcite$se.fit
)


# Plot predictions --------------------------------------------------------
coeff_arag <- 5
a.diff <- max(predframe_calcite$pred_calcite) - min(predframe_calcite$pred_calcite)
b.diff <- max(predframe_apt$pred_apt) - min(predframe_apt$pred_apt)
a.min <- min(predframe_calcite$pred_calcite)+1.12
b.min <- min(predframe_apt$pred_apt)

p2 <- ggplot() +
  # Calcite Layer
  geom_line(predframe_calcite, mapping = aes(year, exp(pred_calcite)), colour = brewer.pal(n = 4, name = "RdBu")[4], linetype = 2) +
  geom_ribbon(predframe_calcite, mapping = aes(year, ymin = exp(pred_calcite - se_calcite), ymax = exp(pred_calcite + se_calcite)),
    fill = brewer.pal(n = 4, name = "RdBu")[4], alpha = 0.3) +
  # Aperture size
  geom_line(predframe_apt, mapping = aes(year, (pred_apt - b.min) / b.diff * a.diff + a.min), colour = brewer.pal(n = 4, name = "BrBG")[1], linetype = 2) +
  geom_ribbon(predframe_apt, mapping = aes(year, ymin = (((pred_apt - se_apt) - b.min) / b.diff * a.diff + a.min),
              ymax = (((pred_apt + se_apt) - b.min) / b.diff * a.diff + a.min)), fill = brewer.pal(n = 4, name = "BrBG")[1], alpha = 0.3) +
  # Aragonite Layer
  geom_line(predframe_aragonite, mapping = aes(year, exp(pred_aragonite) * coeff_arag), colour = brewer.pal(n = 4, name = "RdBu")[1], linetype = 2) +
  geom_ribbon(predframe_aragonite,mapping = aes(year, ymin = exp(pred_aragonite - se_aragonite) * coeff_arag, ymax = exp(pred_aragonite + se_aragonite) * coeff_arag,
                                                fill = brewer.pal(n = 4, name = "RdBu")[1]), alpha = 0.3) +
  theme_science() +
  theme(text = element_text(size = 10, family="Times"),
        axis.title.y.left = element_text(colour = brewer.pal(n = 4, name = "RdBu")[4]),
        axis.title.y.right = element_text(colour = brewer.pal(n = 4, name = "BrBG")[1]),
        legend.position = "none",
        plot.margin = unit(c(0.3, 0.3, 0, 0.3), "cm")) +
  scale_y_continuous(name = expression(paste(Calcite ~ layer ~ "(" * mm * ")")), limits = c(0.5,1.3),
                     sec.axis = sec_axis(trans = ~((. -a.min) * b.diff / a.diff) + b.min, 
                                         name = expression(paste(Aperture ~ size ~ "(" * mm ^ 2 * ")"))))

p3 <- ggplot(predframe_aragonite, aes(year, exp(pred_aragonite))) + 
        geom_blank() + 
        theme_bw() +
        theme(text = element_text(size = 10, family="Times"),
              axis.line.x=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y.left = element_text(colour = brewer.pal(n = 4, name = "RdBu")[1]),
              axis.title.x=element_blank(),
              axis.line.y = element_line(size=0.25)) +
  scale_y_continuous(name = expression(paste(Aragonite ~ layer ~ "(" * mm * ")")), limits = c(0.5 / coeff_arag, 1.3 / coeff_arag))

png("plots/shell_gam.png", width = 120, height = 80, unit = "mm", res = 1200)
(p3 + p2)  + plot_layout(widths = c(0.05,1))
dev.off()

# Export to vector for journal publication
library(Cairo)
cairo_ps("plots/shell_gam.eps", family = "Times", width = 120/25.4, height = 80/25.4)
(p3 + p2)  + plot_layout(widths = c(0.05,1))
dev.off()
