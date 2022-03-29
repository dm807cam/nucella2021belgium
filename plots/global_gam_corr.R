library("tidyverse")
library("patchwork")
library("cmocean")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

m1 <- readRDS("data/shell/model_out/Global_GAM_aperture_global.rds")
m2 <- readRDS("data/shell/model_out/Global_GAM_aragonite_global.rds")
m3 <- readRDS("data/shell/model_out/Global_GAM_calcite_global.rds")
m4 <- readRDS("data/shell/model_out/Global_GAM_shape_PCs.rds")

# Aperture size
m1d <- data.frame(anova(m1)$s.table) %>% rownames_to_column(var = "variables")
m1d$model <- "Aperture size"
m1d$dev_expl <- round(summary(m1)$dev * 100, 0)

# Aragonite layer thickness
m2d <- data.frame(anova(m2)$s.table) %>% rownames_to_column(var = "variables")
m2d$model <- "Aragonite layer"
m2d$dev_expl <- round(summary(m2)$dev * 100, 0)

# Calcite layer thickness
m3d <-data.frame(anova(m3)$s.table) %>% rownames_to_column(var = "variables")
m3d$model <- "Calcite layer"
m3d$dev_expl <- round(summary(m3)$dev * 100, 0)

# shape PCs
m4d <- data.frame(anova(m4)$s.table) %>% rownames_to_column(var = "variables")
m4d$model <- "Shape-PCs"
m4d$dev_expl <- round(summary(m4)$dev * 100, 0)

tmp <- do.call(rbind, str_split(m4d$variables, ':'))
colnames(tmp) <- c("variables", "model")

m4d <- m4d %>% select(-variables, -model)
m4d <- cbind(tmp,m4d) 

m4d_pc1 <- m4d %>% filter(model == "fshapePC1" | model =="s(location)") 
m4d_pc1$model <- "shape-PC1"

m4d_pc2 <- m4d %>% filter(model == "fshapePC2" | model =="s(location)")
m4d_pc2$model <- "shape-PC2"

m4d_pc3 <- m4d %>% filter(model == "fshapePC3" | model =="s(location)")
m4d_pc3$model <- "shape-PC3"

m4d_pc4 <- m4d %>% filter(model == "fshapePC4" | model =="s(location)")
m4d_pc4$model <- "shape-PC4"

m4d_pc5 <- m4d %>% filter(model == "fshapePC5" | model =="s(location)")
m4d_pc5$model <- "shape-PC5"

mod_tab <- rbind(m1d, m2d, m3d, m4d_pc1, m4d_pc2, m4d_pc3, m4d_pc4, m4d_pc5) %>% mutate(p.value = round(p.value, 3))
mod_tab$p.value[mod_tab$p.value > 0.05] <- NA
mod_tab$p.value_text <- mod_tab$p.value
mod_tab$p.value_text[mod_tab$p.value_text == 0] <- "< 0.001"

mod_tab$variables <- sub("_", " ", mod_tab$variables)
mod_tab$variables <- sub("ST", "SST", mod_tab$variables)

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
    text = element_text(size = 10, family="Times"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.3, 0.3, 0, 0.3), "cm")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

sig <- mod_tab %>% 
  mutate(variables = fct_relevel(variables, 
                           "s(location)", "s(shell height)",
                           "s(WS PC1)", "s(WS PC2)", "s(SST PC1)", 
                           "s(SST PC2)", "s(AT PC2)")) %>% 
  ggplot(aes(model, variables, fill = p.value)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = p.value_text), size = 2, colour = "white") +
  scale_fill_cmocean(
    name = "amp",
    start = 0,
    end = 0.8,
    direction = -1,
    na.value = "white"
  ) +
  theme_science() +
  theme(
    panel.background = element_rect(fill="#868686B2"),
    text = element_text(size = 10, family="Times"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.1, 0.3, 0, 0.3), "cm")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))


png("plots/global_gam_corr.png", width = 90, height = 80, unit = "mm", res = 1200)

(dev_expl/sig) + plot_layout(height = c(0.13,1))

dev.off()

