library("tidyverse")
library("scales")
library("geosphere")

# Air temperature ---------------------------------------------------------
# Import data
df_coads <- read.table("data/env/COADS/MSG2.A.enh.180001.201912_1", 
                       fileEncoding="UTF-8-BOM", 
                       skip = 1,      
                       sep = "", 
                       header = TRUE, 
                       na.strings ="",
                       fill = TRUE)

# Select only data from 1880 to the present
df_coads <- df_coads %>% 
  filter(YEAR >= 1880) %>% 
  # Remove over-represented data by reducing the geographic range
  filter(BLA == 52 & BLO == 2 & BSZ == 2) %>% 
  tibble()

# Plot data for an overview
ggplot(df_coads, aes(YEAR, M)) +
  geom_point() +
  geom_smooth(method= "gam", size = 1, colour="red") +
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  theme_bw()

# Create new data frame with variables
df_air <- tibble(air_temperature_celsius = as.numeric(df_coads$M), 
                 year = as.numeric(df_coads$YEAR), 
                 month = as.numeric(df_coads$MON))

write.csv(df_air, "data/env/air_temperature_comp.csv", row.names = FALSE)
rm(list = ls())

# Sea surface temperature -------------------------------------------------
# Import data
df_coads <- read.table("data/env/COADS/MSG2.S.enh.180001.201912_1", 
                       fileEncoding="UTF-8-BOM", 
                       skip = 1, 
                       sep = "", 
                       header = TRUE, 
                       na.strings ="",
                       fill = TRUE)

# Select only data from 1880 to the present
df_coads <- df_coads %>% 
  filter(YEAR >= 1880) %>% 
  # Remove over-represented data by reducing the geographic range
  filter(BLA == 52 & BLO == 2 & BSZ == 2) %>% 
  tibble()

# Plot data for an overview
ggplot(df_coads, aes(YEAR, M)) +
  geom_point() +
  geom_smooth(method= "gam", size = 1, colour="red") +
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  theme_bw()

# Create new data frame with variables
df_sea <- tibble(sea_temperature_celsius = as.numeric(df_coads$M), 
                 year = as.numeric(df_coads$YEAR), 
                 month = as.numeric(df_coads$MON))

write.csv(df_sea, "data/env/sea_surface_temperature.csv", row.names = FALSE)
rm(list = ls())

# Wind speed --------------------------------------------------------------
# Import data
df_coads <- read.table("data/env/COADS/MSG2.W.enh.180001.201912_1", 
                       fileEncoding="UTF-8-BOM", 
                       skip = 1, 
                       sep = "", 
                       header = TRUE, 
                       na.strings ="",
                       fill = TRUE)

# Select only data from 1880 to the present
df_coads <- df_coads %>% 
  filter(YEAR >= 1880) %>% 
  # Remove over-represented data by reducing the geographic range
  filter(BLA == 52 & BLO == 2 & BSZ == 2) %>% 
  tibble()

# Plot data for an overview
ggplot(df_coads, aes(YEAR, M)) +
  geom_point() +
  geom_smooth(method= "gam", size = 1, colour="red") +
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  theme_bw()

# Create new data frame with variables
df_wind <- tibble(wind_speed = as.numeric(df_coads$M), 
                  year = as.numeric(df_coads$YEAR), 
                  month = as.numeric(df_coads$MON))

write.csv(df_wind, "data/env/wind_speed_comp.csv", row.names = FALSE)
rm(list = ls())

# Modeled pH -------------------------------------------------------------
# Import data
df_borges <- read_delim("data/env/pH/borges_pH.csv", delim=";")
df_schelde <- read_delim("data/env/pH/scheldemonitor.csv", delim=",")

# Clean and select data
df_schelde <- df_schelde %>% 
  mutate(date = as.Date(gsub("\\T.*","", datetime))) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  filter(stationname =="Walcheren 2 km uit de kust" & year == 2019) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(value < 10) %>% 
  group_by(year) %>%
  summarise(pH = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))

# Assign variables
df_schelde$from <- "in-situ"
df_borges$from <- "Borges & Gyphens, 2010"

df_pH <- rbind(df_schelde, df_borges)

# Plot data for an overview
ggplot(df_pH, aes(as.numeric(year), pH, colour=from)) +
  geom_errorbar(aes(as.numeric(year), ymin=pH-sd, ymax=pH+sd), colour="grey80") +
  geom_point() +
  theme_bw()

write.csv(df_pH, "data/env/modeled_pH_comp.csv", row.names = FALSE)
rm(list = ls())

# Sea surface salinity ----------------------------------------------------
# Import data
df_ices <- read_delim("data/env/ICES/Surf2161.csv", delim=",")

# Change date format and extract year as variable
df_ices <- df_ices %>% 
  separate(`yyyy-mm-ddThh:mm`, sep=" ", c("date","time")) %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d"),
         year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>% 
  separate(time, sep=":", c("hours", "minutes", "seconds"))

# # Round hours 
# for(i in 1:length(df_ices$hours)) {
#   if(df_ices$minutes[i] > 30) {
#     df_ices$rhour[i] <- as.numeric(df_ices$hours[i]) + 1
#   } else {
#     df_ices$rhour[i] <- as.numeric(df_ices$hours[i])
#   }
# }

# Select variables into data frame
df_ices <- tibble(lat = df_ices$`Latitude [degrees_north]`,
                  long = df_ices$`Longitude [degrees_east]`,
                  year = df_ices$year,
                  month = df_ices$month,
                  sal = df_ices$`PSAL [psu]`)

# Approximate coordinates of sampling locations
OST <- c(2.92, 51.23)
ZEE <- c(3.19, 51.33)
BLA <- c(3.13, 51.32)
ZOU <- c(3.475, 51.5)
ZWA <- c(3.439, 51.39)
KNO <- c(3.28, 51.35)
DUI <- c(3.256, 51.347)

# Calculate distance of environmental measurement stations to sampling locations
pb = txtProgressBar(min = 0, max = length(df_ices$lat), initial = 0, style = 3)
for(i in 1:length(df_ices$lat)) {
  df_ices$dist2OST[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), OST, fun=distGeo) 
  df_ices$dist2ZEE[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), ZEE, fun=distGeo) 
  df_ices$dist2BLA[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), BLA, fun=distGeo) 
  df_ices$dist2ZOU[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), ZOU, fun=distGeo) 
  df_ices$dist2ZWA[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), ZWA, fun=distGeo) 
  df_ices$dist2KNO[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), KNO, fun=distGeo) 
  df_ices$dist2DUI[i] <- distm(c(df_ices$long[i],df_ices$lat[i]), DUI, fun=distGeo) 
  setTxtProgressBar(pb,i)
  close(pb)
}

#Select radius of data points around the sampling sites
max_dist <- 10000

#Filter data points within the selected radius
df_ices.OST <- filter(df_ices, dist2OST < max_dist) %>% 
  mutate(location = "OST")
df_ices.ZEE <- filter(df_ices, dist2ZEE < max_dist) %>% 
  mutate(location = "ZEE")
df_ices.BLA <- filter(df_ices, dist2BLA < max_dist) %>% 
  mutate(location = "BLA")
df_ices.ZOU <- filter(df_ices, dist2ZOU < max_dist) %>% 
  mutate(location = "ZOU")
df_ices.ZWA <- filter(df_ices, dist2ZWA < max_dist) %>% 
  mutate(location = "ZWA")
df_ices.KNO <- filter(df_ices, dist2KNO < max_dist) %>% 
  mutate(location = "KNO")
df_ices.DUI <- filter(df_ices, dist2DUI < max_dist) %>% 
  mutate(location = "DUI")

# Bind tibbles to single tibble
df_sal <- rbind(
  df_ices.OST,
  df_ices.ZEE,
  df_ices.BLA,
  df_ices.ZOU,
  df_ices.ZWA,
  df_ices.KNO,
  df_ices.DUI
) %>% 
  select(-c(dist2OST:dist2DUI))

# Write tibble to csv
write.csv(df_sal, "data/env/salinity_comp.csv", row.names = FALSE)
