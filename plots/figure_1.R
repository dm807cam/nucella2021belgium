library("tidyverse")
library("cowplot")
library("scales")
library("RColorBrewer")
library("magick")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("../imp_func.R", local = TRUE)

# Import data
nucella_shape <- read_delim("../data/shell/shell_shape.csv")
gam_shell_height <- readRDS("../data/shell/GAM_shell_height.rds")
gam_aperture_size <- readRDS("../data/shell/GAM_aperture_size.rds")
gam_aragonite <-
  readRDS("../data/shell/GAM_aragonite_thickness.rds")
gam_calcite <- readRDS("../data/shell/GAM_calcite_thickness.rds")

# Predict shell height ----------------------------------------------------
pdat_height <- with(nucella_shape,
                    data.frame(year = seq(min(year), max(year), length = 1000),
                               location = "ZEE"))

pred_height <- predict(
  gam_shell_height,
  newdata = pdat_height,
  exclude = "location",
  type = "response",
  se.fit = T
)

predframe_height <- data.frame(
  year = pdat_height$year,
  pred_height = pred_height$fit,
  se_height = pred_height$se.fit
)


# Predict aperture size ---------------------------------------------------
pdat_apt <- with(nucella_shape,
                 data.frame(
                   year = seq(min(year), max(year), length = 1000),
                   shell_height = median(nucella_shape$shell_height),
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
                         shell_height = median(nucella_shape$shell_height),
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

# Import overview image to ggplot
p1 <-
  cowplot::ggdraw() + cowplot::draw_image("g619.png", x = 0, y = 0)

# Build thickness plot
sum_arg <- summary(gam_aragonite)
sum_cal <- summary(gam_calcite)
coeff <- 5
p2 <- ggplot() +
  geom_line(
    predframe_calcite,
    mapping = aes(year, exp(pred_calcite)),
    colour = brewer.pal(n = 4, name = "RdBu")[4],
    linetype = 2
  ) +
  geom_ribbon(
    predframe_calcite,
    mapping = aes(
      year,
      ymin = exp(pred_calcite - se_calcite),
      ymax = exp(pred_calcite + se_calcite)
    ),
    fill = brewer.pal(n = 4, name = "RdBu")[4],
    alpha = 0.3
  ) +
  geom_line(
    predframe_aragonite,
    mapping = aes(year, exp(pred_aragonite) * coeff),
    colour = brewer.pal(n = 4, name = "RdBu")[1],
    linetype = 2
  ) +
  geom_ribbon(
    predframe_aragonite,
    mapping = aes(
      year,
      ymin = exp(pred_aragonite - se_aragonite) * coeff,
      ymax = exp(pred_aragonite + se_aragonite) * coeff,
      fill = brewer.pal(n = 4, name = "RdBu")[1]
    ),
    alpha = 0.3
  ) +
  annotate(
    geom = "text",
    label = paste(
      "edf: ",
      round(sum_cal$edf[1], 3),
      ", ",
      "F: ",
      round(sum_cal$chi.sq[1], 3),
      ", ",
      "p: ",
      if (round(sum_cal$s.pv[1], 3) >= 0.001) {
        round(sum_cal$s.pv[1], 3)
      } else {
        "< 0.001"
      },
      sep = ""
    ),
    x = -Inf,
    y = Inf,
    size = 2.5,
    hjust = -0.05,
    vjust = 1.5,
    fontface = "italic",
    colour = brewer.pal(n = 4, name = "RdBu")[4]
  ) +
  annotate(
    geom = "text",
    label = paste(
      "edf: ",
      round(sum_arg$edf[1], 3),
      ", ",
      "F: ",
      round(sum_arg$chi.sq[1], 3),
      ", ",
      "p: ",
      if (round(sum_arg$s.pv[1], 3) >= 0.001) {
        round(sum_arg$s.pv[1], 3)
      } else {
        "< 0.001"
      },
      sep = ""
    ),
    x = -Inf,
    y = Inf,
    size = 2.5,
    hjust = -0.05,
    vjust = 3,
    fontface = "italic",
    colour = brewer.pal(n = 4, name = "RdBu")[1]
  ) +
  theme_science() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y.left = element_text(colour = brewer.pal(n = 4, name = "RdBu")[4]),
    axis.title.y.right = element_text(colour = brewer.pal(n = 4, name = "RdBu")[1]),
    legend.position = "none",
    plot.margin = unit(c(0.3, 0.3, 0, 0.3), "cm")
  ) +
  scale_y_continuous(name = expression(paste(Calcite ~ layer ~ "(" * mm * ")")),
                     sec.axis = sec_axis(trans = ~ . / coeff, name = expression(paste(
                       Aragonite ~ layer ~ "(" * mm * ")"
                     ))))

# Build shell height and aperture size plot
sum_height <- summary(gam_shell_height)
sum_apt <- summary(gam_aperture_size)
coeff2 <- 0.2
p3 <- ggplot() +
  geom_line(
    predframe_height,
    mapping = aes(year, pred_height),
    colour = brewer.pal(n = 4, name = "BrBG")[4],
    linetype = 2
  ) +
  geom_ribbon(
    predframe_height,
    mapping = aes(year,
                  ymin = pred_height - se_height,
                  ymax = pred_height + se_height),
    fill = brewer.pal(n = 4, name = "BrBG")[4],
    alpha = 0.3
  ) +
  geom_line(
    predframe_apt,
    mapping = aes(year, pred_apt * coeff2),
    colour = brewer.pal(n = 4, name = "BrBG")[1],
    linetype = 2
  ) +
  geom_ribbon(
    predframe_apt,
    mapping = aes(
      year,
      ymin = (pred_apt - se_apt) * coeff2,
      ymax = (pred_apt + se_apt) * coeff2
    ),
    fill = brewer.pal(n = 4, name = "BrBG")[1],
    alpha = 0.3
  ) +
  annotate(
    geom = "text",
    label = paste(
      "edf: ",
      round(sum_height$edf[1], 3),
      ", ",
      "F: ",
      round(sum_height$chi.sq[1], 3),
      ", ",
      "p: ",
      if (round(sum_height$s.pv[1], 3) >= 0.001) {
        round(sum_height$s.pv[1], 3)
      } else {
        "< 0.001"
      },
      sep = ""
    ),
    x = -Inf,
    y = Inf,
    size = 2.5,
    hjust = -0.05,
    vjust = 1.5,
    fontface = "italic",
    colour = brewer.pal(n = 4, name = "BrBG")[4]
  ) +
  annotate(
    geom = "text",
    label = paste(
      "edf: ",
      round(sum_apt$edf[1], 3),
      ", ",
      "F: ",
      round(sum_apt$chi.sq[1], 3),
      ", ",
      "p: ",
      if (round(sum_apt$s.pv[1], 3) >= 0.001) {
        round(sum_apt$s.pv[1], 3)
      } else {
        "< 0.001"
      },
      sep = ""
    ),
    x = -Inf,
    y = Inf,
    size = 2.5,
    hjust = -0.05,
    vjust = 3,
    fontface = "italic",
    colour = brewer.pal(n = 4, name = "BrBG")[1]
  ) +
  theme_science() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(colour = brewer.pal(n = 4, name = "BrBG")[4]),
    axis.title.y.right = element_text(colour = brewer.pal(n = 4, name = "BrBG")[1]),
    legend.position = "none",
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  ) +
  scale_y_continuous(name = expression(paste(Shell ~ height ~ "(" * mm * ")")),
                     sec.axis = sec_axis(trans = ~ . / coeff, name = expression(paste(
                       Aperture ~ size ~ "(" * mm ^ 2 * ")"
                     ))))

# Combine plots to panel
pan_pre <- cowplot::plot_grid(
  p2,
  p3,
  ncol = 1,
  rel_heights = c(0.85, 1),
  align = "v",
  labels = c("(b)", "(c)"),
  label_size = 11
)

# Add imported plot to panel
pdf("figure_1.pdf",
    height = 6,
    width = 3,
    onefile = F)
cowplot::plot_grid(
  p1,
  pan_pre,
  ncol = 1,
  rel_heights = c(0.8, 1),
  labels = c("(a)", ""),
  label_size = 11
)
dev.off()
