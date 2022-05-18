library("factoextra")
library("patchwork")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
pca_lag_ST <- readRDS("data/env/model_out/PCA_sea_temp.rds")
pca_lag_AT <- readRDS("data/env/model_out/PCA_air_temp.rds")
pca_lag_WS <- readRDS("data/env/model_out/PCA_wind_speed.rds")

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
  theme(text = element_text(size = 10, family="Times")) +
  ggtitle("Sea surface temperature") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
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
  theme(text = element_text(size = 10, family="Times")) +
  ggtitle("Air temperature") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
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
  theme(text = element_text(size = 10, family="Times")) +
  ggtitle("Wind speed") +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(c)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  )

png("plots/env_pca.png", width = 210, height = 80, unit = "mm", res = 1200)
p1 + p2 + p3
dev.off()

# Export to vector for journal publication
library(Cairo)
cairo_ps("plots/env_pca.eps", family = "Times", width = 210/25.4, height = 80/25.4)
p1 + p2 + p3
dev.off()
