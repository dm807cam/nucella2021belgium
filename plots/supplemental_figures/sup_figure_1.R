library("tidyverse")
library("cowplot")
library("Momocs")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
nucella_coe <- readRDS("data/shell/shape_out/shell_coe_object.rds")
nucella_fac <- tibble(read_delim("data/shell/sample_list.csv",delim=";"))

# Change abbreviation of Oostende from OST to OOS as requested by Thierry.
nucella_fac$location[nucella_fac$location == "OST"] <- "OOS"

nucella_coe$fac <- nucella_fac
nucella_pca <- PCA(nucella_coe)

# Calculate mean shapes for sampling locations and sampling years
nucella_mean_location <- MSHAPES(nucella_coe, "location")
nucella_mean_year <- MSHAPES(nucella_coe, "year")

# Plot mean shapes as panel for a quick overview
panel(Out(nucella_mean_location$shp),
      cols = col_spring(7),
      names = T)
panel(Out(nucella_mean_year$shp),
      cols = col_spring(18),
      names = T)

# Location grid plot ------------------------------------------------------
l <- nucella_mean_location$shp
df_loc <- do.call(rbind.data.frame, l)
df_loc <- tibble::rownames_to_column(df_loc, "location")
df_loc$location <- gsub("\\..*", "", df_loc$location)

mo_pan_1.3 <- ggplot(subset(df_loc, location == "BLA"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[1],
               colour = "black") +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_2.3 <- ggplot(subset(df_loc, location == "DUI"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[2],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_3.3 <- ggplot(subset(df_loc, location == "KNO"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[3],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_4.3 <- ggplot(subset(df_loc, location == "OOS"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[4],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_5.3 <- ggplot(subset(df_loc, location == "ZEE"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[5],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_6.3 <- ggplot(subset(df_loc, location == "ZOU"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[6],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_7.3 <- ggplot(subset(df_loc, location == "ZWA"), aes(x, y)) +
  geom_polygon(size = 0.5,
               fill = Momocs::col_spring(7)[7],
               colour = "black") +
  geom_polygon(
    subset(df_loc, location == "BLA"),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = location
  ),
  size = 3,
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

title_mo_pan_2 <-
  ggdraw() + draw_label("Average shape by location",
                        fontface = 'italic',
                        fontfamily = "Times",
                        size = 10)

mo_pan_2 <- cowplot::plot_grid(
  mo_pan_1.3,
  mo_pan_2.3,
  mo_pan_3.3,
  mo_pan_4.3,
  mo_pan_5.3,
  mo_pan_6.3,
  mo_pan_7.3,
  ncol = 3
)

mo_pan_2 <-cowplot::plot_grid(title_mo_pan_2,
                     mo_pan_2,
                     ncol = 1,
                     rel_heights = c(0.1, 1))
mo_pan_2

# Year grid plot ----------------------------------------------------------
l <- nucella_mean_year$shp
df.year <- do.call(rbind.data.frame, l)
df.year <- tibble::rownames_to_column(df.year, "year")
df.year$year <- gsub("\\..*", "", df.year$year)

mo_pan_1.4 <- ggplot(subset(df.year, year == 1888), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[1]) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_2.4 <- ggplot(subset(df.year, year == 1904), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[2]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_3.4 <- ggplot(subset(df.year, year == 1911), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[3]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_4.4 <- ggplot(subset(df.year, year == 1928), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[4]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_5.4 <- ggplot(subset(df.year, year == 1929), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[5]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_6.4 <- ggplot(subset(df.year, year == 1932), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[6]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_7.4 <- ggplot(subset(df.year, year == 1936), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[7]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_8.4 <- ggplot(subset(df.year, year == 1937), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[8]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_9.4 <- ggplot(subset(df.year, year == 1938), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[9]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_10.4 <- ggplot(subset(df.year, year == 1945), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[10]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_11.4 <- ggplot(subset(df.year, year == 1946), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[11]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_12.4 <- ggplot(subset(df.year, year == 1947), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[12]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_13.4 <- ggplot(subset(df.year, year == 1948), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[13]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_14.4 <- ggplot(subset(df.year, year == 1949), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[14]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_15.4 <- ggplot(subset(df.year, year == 1967), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[15]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_16.4 <- ggplot(subset(df.year, year == 1977), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[16]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_17.4 <- ggplot(subset(df.year, year == 1978), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[17]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

mo_pan_18.4 <- ggplot(subset(df.year, year == 2019), aes(x, y)) +
  geom_polygon(size = 0.5,
               colour = "black",
               fill = Momocs::col_spring(18)[18]) +
  geom_polygon(
    subset(df.year, year == 1888),
    mapping = aes(x, y),
    size = 0.5,
    colour = "red",
    fill = NA
  ) +
  geom_text(aes(
    x = mean(range(x)),
    y = mean(range(y)),
    label = year
  ),
  colour = "black") +
  theme_void() +
  theme(text = element_text(size = 10, family="Times"),
        legend.position = "none") +
  ggtitle("") +
  coord_fixed()

title_mo_pan_3 <-
  ggdraw() + draw_label("Average shape by year",
                        fontface = 'italic',
                        fontfamily = "Times",
                        size = 10)

mo_pan_3 <- cowplot::plot_grid(
  mo_pan_1.4,
  mo_pan_2.4,
  mo_pan_3.4,
  mo_pan_4.4,
  mo_pan_5.4,
  mo_pan_6.4,
  mo_pan_7.4,
  mo_pan_8.4,
  mo_pan_9.4,
  mo_pan_10.4,
  mo_pan_11.4,
  mo_pan_12.4,
  mo_pan_13.4,
  mo_pan_14.4,
  mo_pan_15.4,
  mo_pan_16.4,
  mo_pan_17.4,
  mo_pan_18.4,
  ncol = 6,
  nrow = 3
)

mo_pan_3 <-
  cowplot::plot_grid(title_mo_pan_3,
                     mo_pan_3,
                     ncol = 1,
                     rel_heights = c(0.1, 1))

mo_pan_3


# Combine plots -----------------------------------------------------------

png("plots/supplemental_figures/sup_figure_1.png", width = 210, height = 120, unit = "mm", res = 1200)

cowplot::plot_grid(
  NULL,
  mo_pan_2,
  NULL,
  NULL,
  mo_pan_3,
  rel_widths = c(0.1, 0.25, 0.05, 0.05, 0.55),
  ncol = 5,
  label_size = 11,
  labels = c("(a)", "", "", "(b)", ""),
  label_fontfamily = "Times"
)
dev.off()
