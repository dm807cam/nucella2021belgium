library("tidyverse")
library("mgcv")
library("gratia")

# Import data
belgium_air_temperature <- read_delim("data/env/air_temperature_comp.csv")
glimpse(belgium_air_temperature)

belgium_sea_temperature <- read_delim("data/env/sea_surface_temperature.csv")
glimpse(belgium_sea_temperature)

belgium_wind_speed <- read_delim("data/env/wind_speed_comp.csv")
glimpse(belgium_wind_speed)

# GAMMs for temperature and wind speed time series -------------------------
# Extend controls
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

# Fit Air temperature GAMM
gamm_air_temp <- gamm(air_temperature_celsius ~ s(year, bs = "tp") + 
                        s(month, bs = "cr", k = 12) + 
                        te(year,month, bs = c("tp", "cr")), 
                      data = belgium_air_temperature, 
                      method="REML", 
                      family=gaussian(link = "identity"),
                      correlation = corARMA(form = ~ 1|year, p=2),
                      control = ctrl)

# Check for autocorrelation
layout(matrix(1:2, ncol = 2))
res <- resid(gamm_air_temp$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)

# Check model diagnostics
appraise(gamm_air_temp$gam)
draw(gamm_air_temp$gam)
summary(gamm_air_temp$gam)

# ---

# Fit Sea surface temperature GAMM
gamm_sea_temp <- gamm(sea_temperature_celsius ~ 
                        s(year, bs = "tp") + 
                        s(month, bs = "cr", k = 12) + 
                        te(year,month, bs = c("tp", "cr")), 
                      data = belgium_sea_temperature, 
                      method="REML", 
                      family=gaussian(link = "identity"),
                      correlation = corARMA(form = ~ 1|year, p=1),
                      control = ctrl)

# Check for autocorrelation
layout(matrix(1:2, ncol = 2))
res <- resid(gamm_sea_temp$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)

# Check model diagnostics
appraise(gamm_sea_temp$gam)
draw(gamm_sea_temp$gam)
summary(gamm_sea_temp$gam)

# ---

# Wind speed data show some unusually high values in the early 20th century. 
# I decided to remove these values as they caused issues with the GAMM fit.
# This is a bit of a controversial approach but I found it to be the best option here.
plot(wind_speed ~ year, data= belgium_wind_speed)
belgium_wind_speed <- belgium_wind_speed %>% 
  filter(wind_speed < 15)

# Fit wind speed GAMM
gamm_wind_speed <- gamm(wind_speed ~ 
                          s(year, bs = "tp") + 
                          s(month, bs = "cr", k = 12) + 
                          te(year,month, bs = c("tp", "cr")), 
                         data = belgium_wind_speed, 
                         method="REML", 
                         family=gaussian(link = "identity"),
                         correlation = corARMA(form = ~ 1|year, p=1),
                         control = ctrl)

# Check for autocorrelation
layout(matrix(1:2, ncol = 2))
res <- resid(gamm_wind_speed$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)

# Check model diagnostics
appraise(gamm_wind_speed$gam)
draw(gamm_wind_speed$gam)
summary(gamm_wind_speed$gam)

# Export all models -------------------------------------------------------
saveRDS(gamm_air_temp, "data/env/GAMM_air_temp.rds")
saveRDS(gamm_sea_temp, "data/env/GAMM_sea_temp.rds")
saveRDS(gamm_wind_speed, "data/env/GAMM_wind_speed.rds")
