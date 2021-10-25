library("tidyverse")
library("mgcv")
library("DHARMa")
library("gratia")
library("MuMIn")
library("factoextra")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
belgium_air_temperature <-
  read_delim("data/env/air_temperature_comp.csv")
glimpse(belgium_air_temperature)

belgium_sea_temperature <-
  read_delim("data/env/sea_surface_temperature.csv")
glimpse(belgium_sea_temperature)

belgium_wind_speed <- read_delim("data/env/wind_speed_comp.csv")
glimpse(belgium_wind_speed)

belgium_pH  <- read_delim("data/env/modeled_pH_comp.csv")
glimpse(belgium_pH)

shell_shape <- read_delim("data/shell/shell_shape.csv")
glimpse(shell_shape)

belgium_all <-
  inner_join(belgium_air_temperature, belgium_wind_speed) %>%
  inner_join(belgium_sea_temperature) %>%
  distinct()
# Double inner_join creates unexpected behavior by creating multiple entries.
# We use distinct() to remove these lines. Submit issue to Github!

sampling_years <- unique(shell_shape$year)

datalist <- list()
for (i in unique(sampling_years)) {
  kachy <- filter(belgium_all, year <= i & year >= i - 3)
  tmp <- c(
    i,
    mean(kachy$air_temperature_celsius, na.rm = TRUE),
    median(kachy$air_temperature_celsius, na.rm = TRUE),
    min(kachy$air_temperature_celsius, na.rm = TRUE),
    max(kachy$air_temperature_celsius, na.rm = TRUE),
    quantile(
      kachy$air_temperature_celsius,
      na.rm = TRUE,
      probs = 0.1
    ),
    quantile(
      kachy$air_temperature_celsius,
      na.rm = TRUE,
      probs = 0.25
    ),
    quantile(
      kachy$air_temperature_celsius,
      na.rm = TRUE,
      probs = 0.75
    ),
    quantile(
      kachy$air_temperature_celsius,
      na.rm = TRUE,
      probs = 0.9
    ),
    sd(kachy$air_temperature_celsius, na.rm = TRUE)
  )
  datalist[[i]] <- tmp
}
df_lag_AT <- as.data.frame(do.call("rbind", datalist))
names(df_lag_AT) <- c("year",
                      "mean",
                      "median",
                      "min",
                      "max",
                      "q10",
                      "q25",
                      "q75",
                      "q90",
                      "sd")

datalist <- list()
for (i in unique(sampling_years)) {
  kachy <- filter(belgium_all, year <= i & year >= i - 3)
  tmp <- c(
    i,
    mean(kachy$sea_temperature_celsius, na.rm = TRUE),
    median(kachy$sea_temperature_celsius, na.rm = TRUE),
    min(kachy$sea_temperature_celsius, na.rm = TRUE),
    max(kachy$sea_temperature_celsius, na.rm = TRUE),
    quantile(
      kachy$sea_temperature_celsius,
      na.rm = TRUE,
      probs = 0.1
    ),
    quantile(
      kachy$sea_temperature_celsius,
      na.rm = TRUE,
      probs = 0.25
    ),
    quantile(
      kachy$sea_temperature_celsius,
      na.rm = TRUE,
      probs = 0.75
    ),
    quantile(
      kachy$sea_temperature_celsius,
      na.rm = TRUE,
      probs = 0.9
    ),
    sd(kachy$sea_temperature_celsius, na.rm = TRUE)
  )
  datalist[[i]] <- tmp
}
df_lag_ST <- as.data.frame(do.call("rbind", datalist))
names(df_lag_ST) <- c("year",
                      "mean",
                      "median",
                      "min",
                      "max",
                      "q10",
                      "q25",
                      "q75",
                      "q90",
                      "sd")

datalist <- list()
for (i in unique(sampling_years)) {
  kachy <- filter(belgium_all, year <= i & year >= i - 3)
  tmp <- c(
    i,
    mean(kachy$wind_speed, na.rm = TRUE),
    median(kachy$wind_speed, na.rm = TRUE),
    min(kachy$wind_speed, na.rm = TRUE),
    max(kachy$wind_speed, na.rm = TRUE),
    quantile(kachy$wind_speed, na.rm = TRUE, probs = 0.1),
    quantile(kachy$wind_speed, na.rm = TRUE,  probs = 0.25),
    quantile(kachy$wind_speed, na.rm = TRUE,  probs = 0.75),
    quantile(kachy$wind_speed, na.rm = TRUE,  probs = 0.9),
    sd(kachy$wind_speed, na.rm = TRUE)
  )
  datalist[[i]] <- tmp
}
df_lag_WS <- as.data.frame(do.call("rbind", datalist))
names(df_lag_WS) <- c("year",
                      "mean",
                      "median",
                      "min",
                      "max",
                      "q10",
                      "q25",
                      "q75",
                      "q90",
                      "sd")

pca_lag_ST <- prcomp(df_lag_ST, scale = TRUE, center = TRUE)
pca_lag_AT <- prcomp(df_lag_AT, scale = TRUE, center = TRUE)
pca_lag_WS <- prcomp(df_lag_WS, scale = TRUE, center = TRUE)

saveRDS(pca_lag_ST, "data/env/PCA_sea_temp.rds")
saveRDS(pca_lag_AT, "data/env/PCA_air_temp.rds")
saveRDS(pca_lag_WS, "data/env/PCA_wind_speed.rds")


ind_sup_coord <- predict(pca_lag_ST, newdata = df_lag_ST)
df_lag_ST_pca <- tibble(year = df_lag_ST[, 1],
                        ST_PC1 = ind_sup_coord[, 1],
                        ST_PC2 = ind_sup_coord[, 2])

ind_sup_coord <- predict(pca_lag_AT, newdata = df_lag_AT)
df_lag_AT_pca <- tibble(year = df_lag_AT[, 1],
                        AT_PC1 = ind_sup_coord[, 1],
                        AT_PC2 = ind_sup_coord[, 2])

ind_sup_coord <- predict(pca_lag_WS, newdata = df_lag_WS)
df_lag_WS_pca <- tibble(year = df_lag_WS[, 1],
                        WS_PC1 = ind_sup_coord[, 1],
                        WS_PC2 = ind_sup_coord[, 2])

# Merge join data
df_global <- inner_join(df_lag_ST_pca, df_lag_AT_pca) %>%
  inner_join(df_lag_WS_pca) %>%
  inner_join(shell_shape)

write.csv(df_global, "data/nucella_global.csv", row.names = FALSE)


# Fit global GAMs ---------------------------------------------------------
df_global$year <- as.numeric(df_global$year)
df_global$location <- as.factor(df_global$location)

# Global GAM: PC1
shape_PC1_global <- gam(
  range01(PC1) ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "log"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(shape_PC1_global)
DHARMa::plotQQunif(shape_PC1_global)
acf(resid(shape_PC1_global), lag.max = 36, main = "ACF")
pacf(resid(shape_PC1_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(shape_PC1_global)
gratia::draw(shape_PC1_global)
summary(shape_PC1_global)

# ---

# Global GAM: PC2
shape_PC2_global <- gam(
  range01(PC2) ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "log"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(shape_PC2_global)
DHARMa::plotQQunif(shape_PC2_global)
acf(resid(shape_PC2_global), lag.max = 36, main = "ACF")
pacf(resid(shape_PC2_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(shape_PC2_global)
gratia::draw(shape_PC2_global)
summary(shape_PC2_global)

# ---

# Global GAM: PC3
shape_PC3_global <- gam(
  range01(PC3) ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "log"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(shape_PC3_global)
DHARMa::plotQQunif(shape_PC3_global)
acf(resid(shape_PC3_global), lag.max = 36, main = "ACF")
pacf(resid(shape_PC3_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(shape_PC3_global)
gratia::draw(shape_PC3_global)
summary(shape_PC3_global)

# ---

# Global GAM: PC4
shape_PC4_global <- gam(
  range01(PC4) ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "log"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(shape_PC4_global)
DHARMa::plotQQunif(shape_PC4_global)
acf(resid(shape_PC4_global), lag.max = 36, main = "ACF")
pacf(resid(shape_PC4_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(shape_PC4_global)
gratia::draw(shape_PC4_global)
summary(shape_PC4_global)

# ---

# Global GAM: Calcite layer thickness
calcite_global <- gam(
  calcite_thickness_mm ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "log"),
  data = filter(df_global,!(is.na(
    calcite_thickness_mm
  )))
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(calcite_global)
DHARMa::plotQQunif(calcite_global)
acf(resid(calcite_global), lag.max = 36, main = "ACF")
pacf(resid(calcite_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(calcite_global)
gratia::draw(calcite_global)
summary(calcite_global)

# ---

# Global GAM: Aragonite layer thickness
aragonite_global <- gam(
  aragonite_thickness_mm ~
    s(ST_PC1, k = -1, bs = "tp") +
    s(ST_PC2, k = -1, bs = "tp") +
    s(AT_PC1, k = -1, bs = "tp") +
    s(AT_PC2, k = -1, bs = "tp") +
    s(WS_PC1, k = -1, bs = "tp") +
    s(WS_PC2, k = -1, bs = "tp") +
    s(shell_height, k = -1, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "identity"),
  data = filter(df_global,!(is.na(
    aragonite_thickness_mm
  )))
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(aragonite_global)
DHARMa::plotQQunif(aragonite_global)
acf(resid(aragonite_global), lag.max = 36, main = "ACF")
pacf(resid(aragonite_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(aragonite_global)
gratia::draw(aragonite_global)
summary(aragonite_global)

# ---

# Global GAM: Shell height
height_global <- gam(
  shell_height ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "identity"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(height_global)
DHARMa::plotQQunif(height_global)
acf(resid(height_global), lag.max = 36, main = "ACF")
pacf(resid(height_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(height_global)
gratia::draw(height_global)
summary(height_global)

# ---

# Global GAM: Aperture size
aperture_global <- gam(
  aperture_ellipses ~
    s(ST_PC1, k = 4, bs = "tp") +
    s(ST_PC2, k = 4, bs = "tp") +
    s(AT_PC1, k = 4, bs = "tp") +
    s(AT_PC2, k = 4, bs = "tp") +
    s(WS_PC1, k = 4, bs = "tp") +
    s(WS_PC2, k = 4, bs = "tp") +
    s(shell_height, k = -1, bs = "tp") +
    s(location, bs = "re"),
  select = T,
  family = gaussian(link = "identity"),
  data = df_global
)

par(mfrow = c(2, 2), mai = c(.5, .5, .5, .5))
DHARMa::plotResiduals(aperture_global)
DHARMa::plotQQunif(aperture_global)
acf(resid(aperture_global), lag.max = 36, main = "ACF")
pacf(resid(aperture_global), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(aperture_global)
gratia::draw(aperture_global)
summary(aperture_global)

# Export all models -------------------------------------------------------
saveRDS(shape_PC1_global, "data/GAM_shape_PC1_global.rds")
saveRDS(shape_PC2_global, "data/GAM_shape_PC2_global.rds")
saveRDS(shape_PC3_global, "data/GAM_shape_PC3_global.rds")
saveRDS(shape_PC4_global, "data/GAM_shape_PC4_global.rds")
saveRDS(calcite_global, "data/GAM_calcite_global.rds")
saveRDS(aragonite_global, "data/GAM_aragonite_global.rds")
saveRDS(height_global, "data/GAM_height_global.rds")
saveRDS(aperture_global, "data/GAM_aperture_global.rds")
