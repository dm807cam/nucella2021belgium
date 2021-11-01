library("tidyverse")
library("cowplot")
library("cmocean")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("../imp_func.R", local = TRUE)

m1 <- readRDS("../data/GAM_height_global.rds")
m2 <- readRDS("../data/GAM_aperture_global.rds")
m3 <- readRDS("../data/GAM_aragonite_global.rds")
m4 <- readRDS("../data/GAM_calcite_global.rds")
m5 <- readRDS("../data/GAM_shape_PC1_global.rds")
m6 <- readRDS("../data/GAM_shape_PC2_global.rds")
m7 <- readRDS("../data/GAM_shape_PC3_global.rds")
m8 <- readRDS("../data/GAM_shape_PC4_global.rds")

m1d <-
  data.frame(anova(m1)$s.table) %>% rownames_to_column(var = "variables")
m1d$model <- "Shell height"
m1d$dev_expl <- round(summary(m1)$dev * 100, 0)
m2d <-
  data.frame(anova(m2)$s.table) %>% rownames_to_column(var = "variables")
m2d$model <- "Aperture size"
m2d$dev_expl <- round(summary(m2)$dev * 100, 0)
m3d <-
  data.frame(anova(m3)$s.table) %>% rownames_to_column(var = "variables")
m3d$model <- "Aragonite thickness"
m3d$dev_expl <- round(summary(m3)$dev * 100, 0)
m4d <-
  data.frame(anova(m4)$s.table) %>% rownames_to_column(var = "variables")
m4d$model <- "Calcite thickness"
m4d$dev_expl <- round(summary(m4)$dev * 100, 0)
m5d <-
  data.frame(anova(m5)$s.table) %>% rownames_to_column(var = "variables")
m5d$model <- "Shape-PC1"
m5d$dev_expl <- round(summary(m5)$dev * 100, 0)
m6d <-
  data.frame(anova(m6)$s.table) %>% rownames_to_column(var = "variables")
m6d$model <- "Shape-PC2"
m6d$dev_expl <- round(summary(m6)$dev * 100, 0)
#m7d <- data.frame(anova(m7)$s.table) %>% rownames_to_column(var = "variables")
#m7d$model <- "Shape PC3"
#m7d$dev_expl <- round(summary(m1)$dev*100,0)
m8d <-
  data.frame(anova(m8)$s.table) %>% rownames_to_column(var = "variables")
m8d$model <- "Shape-PC4"
m8d$dev_expl <- round(summary(m8)$dev * 100, 0)

mod_tab <-
  rbind(m1d, m2d, m3d, m4d, m5d, m6d, m8d) %>% mutate(p.value = round(p.value, 3))
mod_tab$p.value[mod_tab$p.value > 0.05] <- NA
mod_tab$p.value_text <- mod_tab$p.value
mod_tab$p.value_text[mod_tab$p.value_text == 0] <- "< 0.001"

dev_expl <- ggplot(mod_tab, aes(model, 1, fill = dev_expl)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = paste(dev_expl, "%")), size = 2, colour = "white") +
  scale_fill_cmocean(
    name = "dense",
    start = 0.2,
    end = 0.8,
    direction = 1,
    na.value = "transparent"
  ) +
  theme_science() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.3, 0.3, 0, 0.3), "cm")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

sig <- ggplot(mod_tab, aes(model, variables, fill = p.value)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = p.value_text), size = 2, colour = "white") +
  scale_fill_cmocean(
    name = "amp",
    start = 0,
    end = 0.8,
    direction = -1,
    na.value = "transparent"
  ) +
  theme_science() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.1, 0.3, 0, 0.3), "cm")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

pdf("figure_6.pdf",
    height = 3.5,
    width = 3.5,
    onefile = F)
plot_grid(
  dev_expl,
  sig,
  rel_heights = c(.12, .88),
  ncol = 1,
  align = "v"
)
dev.off()