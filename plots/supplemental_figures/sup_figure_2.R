library("rnaturalearth")
#devtools::install_github("ropensci/rnaturalearthhires")
library("tidyverse")
library("marmap")
library("ggrepel")
library("viridis")
library("ggpubr")
library("ggspatial")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
df_sal <- read_delim("data/env/salinity_comp.csv")
nc_data <-
  readGEBCO.bathy("data/env/GEBCO/gebco_2020_n52.0_s51.0_w2.0_e4.0.nc")
bat_df <- marmap::as.xyz(nc_data) %>%
  rename(long = 1, lat = 2, bat = 3)

# Change abbreviation of Oostende from OST to OOS as requested by Thierry.
df_sal$location[df_sal$location == "OST"] <- "OOS"

# Define sampling sites
Oostende <- c("OOS", 2.92, 51.23)
Blankenberge <- c("BLA", 3.13, 51.32)
Zeebrugge <- c("ZEE", 3.19, 51.33)
Duinbergen <- c("DUI", 3.256, 51.347)
Knokke.Heist <- c("KNO", 3.28, 51.35)
Zwarte.Polder <- c("ZWA", 3.439, 51.39)
Zoutelande <- c("ZOU", 3.475, 51.5)

locations <-
  tibble(
    location = c("OOS", "BLA", "ZEE", "DUI", "KNO", "ZWA", "ZOU"),
    long = c(2.92, 3.13, 3.19, 3.256, 3.28, 3.439, 3.475),
    lat = c(51.23, 51.32, 51.33, 51.347, 51.35, 51.39, 51.5)
  )

# Import world map as sf
world <- ne_countries(scale = 10, returnclass = "sf")

# Median salinity per long/lat
df_sal_m <- df_sal %>%
  group_by(lat, long) %>%
  summarise(median_sal = as.numeric(median(sal, na.rm = TRUE)),
            n = n())

# Median salinity per sampling site
df_sal_m2 <- df_sal %>%
  group_by(location) %>%
  summarise(median_sal = median(sal, na.rm = TRUE))

df_sal <- merge(df_sal, df_sal_m2, by = "location")

# Plot sampling site map

p1 <- ggplot(data = world) +
  geom_tile(bat_df, mapping = aes(long,
                                  lat,
                                  fill = bat)) +
  geom_point(df_sal_m, mapping = aes(long,
                                     lat,
                                     size = n,
                                     colour = median_sal)) +
  scale_color_viridis_c() +
  scale_fill_gradient2(low = "dodgerblue4",
                       high = "lightblue",
                       guide = 'none') +
  geom_sf(fill = "antiquewhite") +
  annotation_scale(text_family = "Times") +
  theme_bw() +
  theme(text = element_text(size = 10, family="Times"),
    panel.grid.major = element_line(
      color = "grey",
      linetype = "dashed",
      size = 0.25
    ),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  annotate(
    geom = "text",
    x = 3.7,
    y = 51.41,
    label = "Scheldt",
    fontface = "italic",
    color = "grey22",
    angle = -30,
    size = rel(0.5)
  ) +
  annotate(
    geom = "text",
    x = 3.1,
    y = 51.1,
    label = "Belgium",
    color = "grey22",
    family = "Times"
  ) +
  geom_label_repel(
    locations,
    mapping = aes(long,
                  lat,
                  label = location),
    size = 2,
    nudge_x = -0.1,
    nudge_y = 0.2,
    family = "Times"
  ) +
  geom_point(
    locations,
    mapping = aes(long, lat),
    pch = 21,
    size = 3,
    fill = "red",
    colour = "black"
  ) +
  xlab("Longitude (decimal degree)") +
  ylab("Latitude (decimal degree)") +
  guides(
    colour = guide_legend(
      title = expression(paste(tilde(x) ~ "salinity (psu):")),
      order = 1,
      override.aes = list(size = 3)
    ),
    size = guide_legend(title = "n:"),
    order = 2
  ) +
  coord_sf(xlim = c(2.4, 3.9),
           ylim = c(51, 51.9))

date_OOS <-
  as.numeric(c(max(df_sal$year[df_sal$location == "OOS"]),
               min(df_sal$year[df_sal$location == "OOS"])))
date_ZEE <-
  as.numeric(c(max(df_sal$year[df_sal$location == "ZEE"]),
               min(df_sal$year[df_sal$location == "ZEE"])))
date_BLA <-
  as.numeric(c(max(df_sal$year[df_sal$location == "BLA"]),
               min(df_sal$year[df_sal$location == "BLA"])))
date_ZOU <-
  as.numeric(c(max(df_sal$year[df_sal$location == "ZOU"]),
               min(df_sal$year[df_sal$location == "ZOU"])))
date_ZWA <-
  as.numeric(c(max(df_sal$year[df_sal$location == "ZWA"]),
               min(df_sal$year[df_sal$location == "ZWA"])))
date_KNO <-
  as.numeric(c(max(df_sal$year[df_sal$location == "KNO"]),
               min(df_sal$year[df_sal$location == "KNO"])))
date_DUI <-
  as.numeric(c(max(df_sal$year[df_sal$location == "DUI"]),
               min(df_sal$year[df_sal$location == "DUI"])))

# Boxplot salinity data
p2 <- ggplot(df_sal, aes(reorder(location,
                                 sal,
                                 FUN = median),
                         sal,
                         fill = median_sal)) +
  geom_boxplot(na.rm = TRUE,
               outlier.shape = NA) +
  annotate(
    "rect",
    xmin = 1.5,
    xmax = 2.5,
    ymin = 25,
    ymax = 39,
    fill = "#e0e0e0",
    alpha = .2
  ) +
  annotate(
    "rect",
    xmin = 3.5,
    xmax = 4.5,
    ymin = 25,
    ymax = 39,
    fill = "#e0e0e0",
    alpha = .2
  ) +
  annotate(
    "rect",
    xmin = 5.5,
    xmax = 6.5,
    ymin = 25,
    ymax = 39,
    fill = "#e0e0e0",
    alpha = .2
  ) +
  geom_jitter(
    position = position_jitter(0.2),
    pch = 21,
    alpha = 0.2,
    colour = "black",
    fill = "white"
  ) +
  theme_bw() +
  scale_fill_viridis() +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank()
  ) +
  labs(x = "", y = (expression(salinity ~ (psu)))) +
  annotate(
    "text",
    x = 4,
    y = c(36.5, 37.1, 37.6),
    label = c(date_OOS[1], "-", date_OOS[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 5,
    y = c(36.5, 37.1, 37.6),
    label = c(date_ZEE[1], "-", date_ZEE[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 1,
    y = c(36.5, 37.1, 37.6),
    label = c(date_BLA[1], "-", date_BLA[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 6,
    y = c(36.5, 37.1, 37.6),
    label = c(date_ZOU[1], "-", date_ZOU[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 7,
    y = c(36.5, 37.1, 37.6),
    label = c(date_ZWA[1], "-", date_ZWA[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 3,
    y = c(36.5, 37.1, 37.6),
    label = c(date_KNO[1], "-", date_KNO[2]),
    size = 2.5,
    family = "Times"
  ) +
  annotate(
    "text",
    x = 2,
    y = c(36.5, 37.1, 37.6),
    label = c(date_DUI[1], "-", date_DUI[2]),
    size = 2.5,
    family = "Times"
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(25, 39)) +
  coord_fixed(ratio = 0.5)

# Print plots to file
png("plots/supplemental_figures/sup_figure_2.png", width = 200, height = 100, unit = "mm", res = 1200)

ggarrange(
  p1,
  p2,
  ncol = 2,
  labels = c("(a)", "(b)"),
  align = "hv",
  font.label = list(size = 11,
                    family = "Times"),
  common.legend = TRUE,
  legend = "bottom"
)
dev.off()
