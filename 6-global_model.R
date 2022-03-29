library("tidyverse")
library("mgcv")
library("DHARMa")
library("gratia")
library("MuMIn")
library("factoextra")

rm(list=ls())

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

pca_lag_ST <- prcomp(df_lag_ST[,-1], scale = TRUE, center = TRUE)
pca_lag_AT <- prcomp(df_lag_AT[,-1], scale = TRUE, center = TRUE)
pca_lag_WS <- prcomp(df_lag_WS[,-1], scale = TRUE, center = TRUE)

saveRDS(pca_lag_ST, "data/env/model_out/PCA_sea_temp.rds")
saveRDS(pca_lag_AT, "data/env/model_out/PCA_air_temp.rds")
saveRDS(pca_lag_WS, "data/env/model_out/PCA_wind_speed.rds")

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

write.csv(df_global, "data/shell/nucella_global.csv", row.names = FALSE)

# Fit global GAMs ---------------------------------------------------------
df_global$year <- as.numeric(df_global$year)
df_global$location <- as.factor(df_global$location)

# Check for (multi-) colinearity among descriptive variables
# and step wise exclusion of less relevant variables 
# based on a priori knowledge.
df_global %>% 
  ungroup() %>% 
  select(ST_PC1,
         ST_PC2,
         AT_PC1,
         AT_PC2,
         WS_PC1,
         WS_PC2,
         shell_height
  ) %>% 
  corvif() %>% 
  round(2)

# Simple function to calculate maximal VIF observed between predictors
# This function is based on the CORVIF function by Zuur et al.
max_corr <- function(x) {
  
  # Check if model is of class gam
  if(class(x)[1] != "gam"){
    print("Error: Invalid model class")}
  
  # Select model table as dataframe and drop dependent variable
  corr <- data.frame(x$model) %>% 
    select(-1)
  
  # Select only numeric terms and calculate VIF
  corr <-  corr %>% 
    select_if(is.numeric) %>% 
    # Use CROVIF function from the helper file
    corvif() %>% 
    round(2)
  
  # Vif table as matrix
  corrm <- as.matrix(corr)
  
  # Find mxx VIF value 
  if (length(corrm)<=1){
    corrm <- 0
    max(abs(corrm))
  } else {
    max(abs(corrm))
  }  
}

# Global GAM: Aperture size -----------------------------------------------
# Fit starting model
aperture_global <- gam(aperture_ellipses ~  
               s(ST_PC1, bs="cr", k = 3) + 
               s(ST_PC2, bs="cr", k = 3) + 
               s(AT_PC1, bs="cr", k = 3) + 
               s(AT_PC2, bs="cr", k = 3) + 
               s(WS_PC1, bs="cr", k = 3) +
               s(WS_PC2, bs="cr", k = 3) +
               s(shell_height, bs="cr", k = 3) +
               s(location, bs="re"),
             method = "ML", 
             family=gaussian(link="identity"),
             data = df_global, 
             control = gam.control(trace = F))

# Model selection accounting for VIF factor among predictors
options(na.action = na.fail)
d_tab <- dredge(aperture_global, rank = "AIC", m.lim = c(3, 7), extra = c(max_corr), trace = 2) 
# Retrieve best model from models with VIF < 3
aperture_global <- get.models(d_tab, subset = max_corr < 3)[[1]] 

# Check summary
summary(aperture_global)

# Fit preferred model with REML and validate 
aperture_global <- gam(aperture_ellipses ~  
                         s(ST_PC1, bs="cr", k = 3) + 
                         s(ST_PC2, bs="cr", k = 3) +
                         s(WS_PC1, bs="cr", k = 3) +
                         s(shell_height, bs="cr", k = 3),
                       method = "REML", 
                       family=gaussian(link="identity"),
                       data = df_global, 
                       control = gam.control(trace = F))

summary(aperture_global)
appraise(aperture_global)

# Global GAM: Calcite layer thickness -------------------------------------
# Fit starting model
# Remove NAs from Calcite layer measurements
df_global_calcite <- subset(df_global, !is.na(calcite_thickness_mm))
df_global_calcite$calcite_thickness_mm_log <- log(df_global_calcite$calcite_thickness_mm)

calcite_global <- gam(calcite_thickness_mm_log ~  
                         s(ST_PC1, bs="cr", k = 3) + 
                         s(ST_PC2, bs="cr", k = 3) + 
                         s(AT_PC1, bs="cr", k = 3) + 
                         s(AT_PC2, bs="cr", k = 3) + 
                         s(WS_PC1, bs="cr", k = 3) +
                         s(WS_PC2, bs="cr", k = 3) +
                         s(shell_height, bs="cr", k = 3) +
                         s(location, bs="re"),
                       method = "ML", 
                       family=gaussian(link="identity"),
                       data = df_global_calcite,
                       control = gam.control(trace = F))

# Model selection accounting for VIF factor among predictors
options(na.action = na.fail)
d_tab <- dredge(calcite_global, rank = "AIC", m.lim = c(3, 7), extra = c(max_corr), trace = 2) 
# Retrieve best model from models with VIF < 3
calcite_global <- get.models(d_tab, subset = max_corr < 3)[[1]] 

# Check summary
summary(calcite_global)

# Fit preferred model with REML and validate 
calcite_global <- gam(calcite_thickness_mm_log ~  
                         s(WS_PC1, bs="cr", k = 3) + 
                         s(WS_PC2, bs="cr", k = 3) +
                         s(location, bs="re"),
                       method = "REML", 
                       family=gaussian(link="identity"),
                       data = df_global_calcite, 
                       control = gam.control(trace = F))

summary(calcite_global)
appraise(calcite_global)

# Global GAM: Aragonite layer thickness -----------------------------------
df_global_aragonite <- subset(df_global, !is.na(aragonite_thickness_mm))
df_global_aragonite$aragonite_thickness_mm_log <- log(df_global_aragonite$aragonite_thickness_mm)

aragonite_global <- gam(aragonite_thickness_mm_log ~  
                        s(ST_PC1, bs="cr", k = 3) + 
                        s(ST_PC2, bs="cr", k = 3) + 
                        s(AT_PC1, bs="cr", k = 3) + 
                        s(AT_PC2, bs="cr", k = 3) + 
                        s(WS_PC1, bs="cr", k = 3) +
                        s(WS_PC2, bs="cr", k = 3) +
                        s(shell_height, bs="cr", k = 3) +
                        s(location, bs="re"),
                      method = "ML", 
                      family=gaussian(link="identity"),
                      data = df_global_aragonite,
                      control = gam.control(trace = F))

# Model selection accounting for VIF factor among predictors
options(na.action = na.fail)
d_tab <- dredge(aragonite_global, rank = "AIC", m.lim = c(3, 7), extra = c(max_corr), trace = 2) 
# Retrieve best model from models with VIF < 3
aragonite_global <- get.models(d_tab, subset = max_corr < 3)[[1]] 

# Check summary
summary(aragonite_global)

# Fit preferred model with REML and validate 
aragonite_global <- gam(aragonite_thickness_mm_log ~  
                        s(ST_PC1, bs="cr", k = 3) + 
                        s(ST_PC2, bs="cr", k = 3) +
                        s(AT_PC2, bs="cr", k = 3) +
                        s(location, bs="re"),
                      method = "REML", 
                      family=gaussian(link="identity"),
                      data = df_global_aragonite, 
                      control = gam.control(trace = F))

summary(aragonite_global)
appraise(aragonite_global)

# Global GAM: shape PCs ---------------------------------------------------
# Pivot long
df_global_pcs <- df_global %>% 
  pivot_longer(cols = c(PC1:PC5), names_to = "fshape") %>% 
  mutate(fshape = as.factor(fshape),
         location = as.factor(location)) %>% 
  group_by(fshape) %>% 
  mutate(svalue = range01(value)) %>% 
  ungroup() 

pcs_global <- gam(svalue ~ fshape +
                          s(ST_PC1, bs="cr", k = 3, by=fshape) + 
                          s(ST_PC2, bs="cr", k = 3, by=fshape) + 
                          s(AT_PC1, bs="cr", k = 3, by=fshape) + 
                          s(AT_PC2, bs="cr", k = 3, by=fshape) + 
                          s(WS_PC1, bs="cr", k = 3, by=fshape) +
                          s(WS_PC2, bs="cr", k = 3, by=fshape) +
                          s(shell_height, bs="cr", k = 3, by=fshape) +
                          s(location, bs="re"),
                        method = "ML", 
                        family=gaussian(link="identity"),
                        data = df_global_pcs,
                        control = gam.control(trace = F))

# Model selection accounting for VIF factor among predictors
options(na.action = na.fail)
d_tab <- dredge(pcs_global, rank = "AIC", m.lim = c(4, 10), extra = c(max_corr), trace = 2) 
# Retrieve best model from models with VIF < 3
pcs_global <- get.models(d_tab, subset = max_corr < 3)[[1]] 

# Check summary
summary(pcs_global)

# Fit preferred model with REML and validate 
pcs_global <- gam(svalue ~ fshape + 
                          s(ST_PC1, bs="cr", k = 3, by=fshape) + 
                          s(shell_height, bs="cr", k = 3, by=fshape) +
                          s(location, bs="re"),
                        method = "REML", 
                        family=gaussian(link="identity"),
                        data = df_global_pcs, 
                        control = gam.control(trace = F))

summary(pcs_global)
appraise(pcs_global)

# Export all models -------------------------------------------------------
saveRDS(aperture_global, "data/shell/model_out/Global_GAM_aperture_global.rds")
saveRDS(calcite_global, "data/shell/model_out/Global_GAM_calcite_global.rds")
saveRDS(aragonite_global, "data/shell/model_out/Global_GAM_aragonite_global.rds")
saveRDS(pcs_global, "data/shell/model_out/Global_GAM_shape_PCs.rds")
