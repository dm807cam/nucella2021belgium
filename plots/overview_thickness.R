library("tidyverse")
library("scales")
library("ggsci")
library("patchwork")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
nucella_shape <- read_delim("data/shell/shell_shape.csv")

# Change abbreviation of Oostende from OST to OOS as requested by Thierry.
nucella_shape$location[nucella_shape$location == "OST"] <- "OOS"

# Make overview plot for measured shell parameters ------------------------
p1 <- ggplot(nucella_shape, aes(year, aperture_ellipses, colour=location)) + 
  geom_point(pch=21) + 
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(year, aperture_ellipses),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", 
        axis.title.x = element_blank()) + 
  ylab(label = expression(paste(Aperture ~ size ~ "(" * mm ^ 2 * ")"))) + 
  scale_color_jco()

p2 <- ggplot(nucella_shape, aes(shell_height, aperture_ellipses, colour=location)) + 
  geom_point(pch=21) + 
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(shell_height, aperture_ellipses),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", 
        axis.title = element_blank()) +
  scale_color_jco()

p3 <- nucella_shape %>% 
  mutate(location = fct_relevel(location, 
                            "OOS", "BLA", "ZEE", "DUI", "KNO", "ZWA", "ZOU")) %>%
  ggplot(aes(location, aperture_ellipses, group=location, colour=location)) + 
  geom_boxplot() + 
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", axis.title = element_blank()) +
  scale_color_jco()

p4 <- ggplot(nucella_shape, aes(year, calcite_thickness_mm, colour=location)) + 
  geom_point(pch=21)+ 
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(year, calcite_thickness_mm),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", axis.title.x = element_blank()) +
  ylab(label = expression(paste(Calcite ~ layer ~ "(" * mm * ")"))) + 
  scale_color_jco()

p5 <- ggplot(nucella_shape, aes(shell_height, calcite_thickness_mm, colour=location)) + 
  geom_point(pch=21) + 
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +  
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(shell_height, calcite_thickness_mm),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(legend.position="none", axis.title = element_blank()) +
  scale_color_jco()

p6 <- nucella_shape %>% 
  mutate(location = fct_relevel(location, 
                            "OOS", "BLA", "ZEE", "DUI", "KNO", "ZWA", "ZOU")) %>%
  ggplot(aes(location, calcite_thickness_mm, group=location, colour=location)) + 
  geom_boxplot() + 
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", axis.title = element_blank()) +
  scale_color_jco()

p7 <- ggplot(nucella_shape, aes(year, aragonite_thickness_mm, colour=location)) + 
  geom_point(pch=21) +  
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(year, aragonite_thickness_mm),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none") +
  ylab(label = expression(paste(Aragonite ~ layer ~ "(" * mm * ")"))) + 
  xlab("sampling year") +
  scale_color_jco()

p8 <- ggplot(nucella_shape, aes(shell_height, aragonite_thickness_mm, colour=location)) + 
  geom_point(pch=21) + 
  geom_smooth(method="loess", span=3, colour="red", se=F, size= 0.5) +
  geom_smooth(subset(nucella_shape, location =="ZEE"), mapping=aes(shell_height, aragonite_thickness_mm),method="loess", span=3, se=F, size= 0.5) +
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", axis.title.y = element_blank()) +
  xlab("shell height (mm)") +
  scale_color_jco()

p9 <- nucella_shape %>% 
  mutate(location = fct_relevel(location, 
                            "OOS", "BLA", "ZEE", "DUI", "KNO", "ZWA", "ZOU")) %>%
  ggplot(aes(location, aragonite_thickness_mm, group=location, colour=location)) + 
  geom_boxplot() + 
  theme_science() + 
  theme(text = element_text(size = 10, family="Times"),
        legend.position="none", axis.title.y = element_blank()) +
  xlab("sampling site") +
  scale_color_jco()

png("plots/overview_thickness.png", width = 180, height = 160, unit = "mm", res = 1200)

(p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9)

dev.off()