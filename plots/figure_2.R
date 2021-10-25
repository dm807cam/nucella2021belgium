library("factoextra")
library("cowplot")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("../imp_func.R", local = TRUE)

# Import data
pca_lag_ST <- readRDS("../data/env/PCA_sea_temp.rds")
pca_lag_AT <- readRDS("../data/env/PCA_air_temp.rds")
pca_lag_WS <- readRDS("../data/env/PCA_wind_speed.rds")

# fviz_eig(pca_lag_ST)
# fviz_eig(pca_lag_AT)
# fviz_eig(pca_lag_WS)

# Plot PCA-Bi-Plots
p1 <- fviz_pca_biplot(
  pca_lag_ST,
  repel = TRUE,
  geom = "point",
  pointsize = 2,
  pointshape = 21,
  col.var = "red",
  arrowsize = 0.4,
  labelsize = 3
) +
  theme_science() +
  theme(aspect.ratio = 1) +
  ggtitle("Sea surface temperature") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  )

p2 <- fviz_pca_biplot(
  pca_lag_AT,
  repel = TRUE,
  geom = "point",
  pointsize = 2,
  pointshape = 21,
  col.var = "red",
  arrowsize = 0.4,
  labelsize = 3
) +
  theme_science() +
  theme(aspect.ratio = 1) +
  ggtitle("Air temperature") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  )

p3 <- fviz_pca_biplot(
  pca_lag_WS,
  repel = TRUE,
  geom = "point",
  pointsize = 2,
  pointshape = 21,
  col.var = "red",
  arrowsize = 0.4,
  labelsize = 3
) +
  theme_science() +
  theme(aspect.ratio = 1) +
  ggtitle("Wind speed") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(c)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  )

# Combine plots to panel
pdf("figure_2.pdf",
    height = 2.4,
    width = 6,
    onefile = F)
plot_grid(p1, p2, p3, ncol = 3)
dev.off()
