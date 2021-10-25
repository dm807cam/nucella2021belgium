library("tidyverse")
library("mgcv")
library("cowplot")
library("directlabels")

# Import data
belgium_air_temperature <-
  read_delim("../data/env/air_temperature_comp.csv")
belgium_sea_temperature <-
  read_delim("../data/env/sea_surface_temperature.csv")
belgium_wind_speed <- read_delim("../data/env/wind_speed_comp.csv")

# Load GAMM data
gamm_air_temp <- readRDS("../data/env/GAMM_air_temp.rds")
gamm_sea_temp <- readRDS("../data/env/GAMM_sea_temp.rds")
gamm_wind_speed <- readRDS("../data/env/GAMM_wind_speed.rds")

# Define months
Abbmonths <- c("Jan",
               "Feb",
               "Mar",
               "Apr",
               "May",
               "Jun",
               "Jul",
               "Aug",
               "Sep",
               "Oct",
               "Nov",
               "Dec")

# Define seasons
seasons <- rep(c("Spring", "Summer", "Fall", "Winter"), each = 3)

# Predict air temperature
temp_kachy <- list()
for (i in c(1:12)) {
  pdat_air_temp <-
    data.frame(year = unique(belgium_air_temperature$year),
               month = i)
  
  pred_air_temp <- predict(
    gamm_air_temp$gam,
    newdata = pdat_air_temp,
    type = "response",
    se.fit = T
  )
  
  predframe_air_temp <- data.frame(
    year = pdat_air_temp$year,
    pred_air_temp = pred_air_temp[[1]],
    se_air_temp = pred_air_temp[[2]]
  )
  
  predframe_air_temp$month <- i
  temp_kachy[[i]] <- predframe_air_temp
}
predframe_air_temp <- do.call(rbind, temp_kachy)

predframe_air_temp$monthAbb <- Abbmonths[predframe_air_temp$month]
predframe_air_temp$season[predframe_air_temp$month == 12 |
                            predframe_air_temp$month == 1 |
                            predframe_air_temp$month == 2] <-
  "Winter"
predframe_air_temp$season[predframe_air_temp$month == 3 |
                            predframe_air_temp$month == 4 |
                            predframe_air_temp$month == 5] <-
  "Spring"
predframe_air_temp$season[predframe_air_temp$month == 6 |
                            predframe_air_temp$month == 7 |
                            predframe_air_temp$month == 8] <-
  "Summer"
predframe_air_temp$season[predframe_air_temp$month == 9 |
                            predframe_air_temp$month == 10 |
                            predframe_air_temp$month == 11] <-
  "Autumn"
predframe_air_temp$season <- factor(predframe_air_temp$season,
                                    levels = c("Winter",
                                               "Spring",
                                               "Summer",
                                               "Autumn"))

# Predict sea surface temperature
temp_kachy <- list()
for (i in c(1:12)) {
  pdat_sea_temp <-
    data.frame(year = unique(belgium_sea_temperature$year),
               month = i)
  
  pred_sea_temp <- predict(
    gamm_sea_temp$gam,
    newdata = pdat_sea_temp,
    type = "response",
    se.fit = T
  )
  
  predframe_sea_temp <- data.frame(
    year = pdat_sea_temp$year,
    pred_sea_temp = pred_sea_temp[[1]],
    se_sea_temp = pred_sea_temp[[2]]
  )
  
  predframe_sea_temp$month <- i
  temp_kachy[[i]] <- predframe_sea_temp
}
predframe_sea_temp <- do.call(rbind, temp_kachy)

predframe_sea_temp$monthAbb <- Abbmonths[predframe_sea_temp$month]
predframe_sea_temp$season[predframe_sea_temp$month == 12 |
                            predframe_sea_temp$month == 1 |
                            predframe_sea_temp$month == 2] <-
  "Winter"
predframe_sea_temp$season[predframe_sea_temp$month == 3 |
                            predframe_sea_temp$month == 4 |
                            predframe_sea_temp$month == 5] <-
  "Spring"
predframe_sea_temp$season[predframe_sea_temp$month == 6 |
                            predframe_sea_temp$month == 7 |
                            predframe_sea_temp$month == 8] <-
  "Summer"
predframe_sea_temp$season[predframe_sea_temp$month == 9 |
                            predframe_sea_temp$month == 10 |
                            predframe_sea_temp$month == 11] <-
  "Autumn"
predframe_sea_temp$season <- factor(predframe_sea_temp$season,
                                    levels = c("Winter",
                                               "Spring",
                                               "Summer",
                                               "Autumn"))

# Predict sea surface temperature
temp_kachy <- list()
for (i in c(1:12)) {
  pdat_wind_speed <-
    data.frame(year = unique(belgium_wind_speed$year),
               month = i)
  
  pred_wind_speed <- predict(
    gamm_wind_speed$gam,
    newdata = pdat_wind_speed,
    type = "response",
    se.fit = T
  )
  
  predframe_wind_speed <- data.frame(
    year = pdat_wind_speed$year,
    pred_wind_speed = pred_wind_speed[[1]],
    se_wind_speed = pred_wind_speed[[2]]
  )
  
  predframe_wind_speed$month <- i
  temp_kachy[[i]] <- predframe_wind_speed
}
predframe_wind_speed <- do.call(rbind, temp_kachy)

predframe_wind_speed$monthAbb <-
  Abbmonths[predframe_wind_speed$month]
predframe_wind_speed$season[predframe_wind_speed$month == 12 |
                              predframe_wind_speed$month == 1 |
                              predframe_wind_speed$month == 2] <-
  "Winter"
predframe_wind_speed$season[predframe_wind_speed$month == 3 |
                              predframe_wind_speed$month == 4 |
                              predframe_wind_speed$month == 5] <-
  "Spring"
predframe_wind_speed$season[predframe_wind_speed$month == 6 |
                              predframe_wind_speed$month == 7 |
                              predframe_wind_speed$month == 8] <-
  "Summer"
predframe_wind_speed$season[predframe_wind_speed$month == 9 |
                              predframe_wind_speed$month == 10 |
                              predframe_wind_speed$month == 11] <-
  "Autumn"
predframe_wind_speed$season <- factor(predframe_wind_speed$season,
                                      levels = c("Winter",
                                                 "Spring",
                                                 "Summer",
                                                 "Autumn"))

# Plot predictions
air_temp_facet <- ggplot(predframe_air_temp, aes(year,
                                                 pred_air_temp,
                                                 group = monthAbb)) +
  geom_ribbon(
    aes(
      year,
      ymin = pred_air_temp - se_air_temp,
      ymax = pred_air_temp + se_air_temp,
      group = monthAbb
    ),
    fill = "grey",
    alpha = .5
  ) +
  geom_smooth(
    colour = "black",
    span = 0.1,
    linetype = 2,
    size = .5
  ) +
  geom_dl(aes(label = monthAbb),
          method = list("smart.grid", cex = .6)) +
  theme_bw() +
  facet_wrap(~ season,
             scales = "free_y",
             ncol = 1,
             strip.position = "right") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    legend.position = "none",
    title = element_text(size = 6),
    plot.margin = unit(c(.2, 0, .8, 1), "cm")
  ) +
  ggtitle(expression(paste(Air ~ Temperature ~ "(" * degree * C * ")")))

sea_temp_facet <- ggplot(predframe_sea_temp, aes(year,
                                                 pred_sea_temp,
                                                 group = monthAbb)) +
  geom_ribbon(
    aes(
      year,
      ymin = pred_sea_temp - se_sea_temp,
      ymax = pred_sea_temp + se_sea_temp,
      group = monthAbb
    ),
    fill = "grey",
    alpha = .5
  ) +
  geom_smooth(
    colour = "black",
    span = 0.1,
    linetype = 2,
    size = .5
  ) +
  geom_dl(aes(label = monthAbb),
          method = list("smart.grid", cex = .6)) +
  theme_bw() +
  facet_wrap(~ season,
             scales = "free_y",
             ncol = 1,
             strip.position = "right") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    legend.position = "none",
    title = element_text(size = 6),
    plot.margin = unit(c(.2, 0, .8, 1), "cm")
  ) +
  ggtitle(expression(paste(Sea ~ Temperature ~ "(" * degree * C * ")")))

wind_speed_facet <- ggplot(predframe_wind_speed,
                           aes(year,
                               pred_wind_speed,
                               group = monthAbb)) +
  geom_ribbon(
    aes(
      year,
      ymin = pred_wind_speed - se_wind_speed,
      ymax = pred_wind_speed + se_wind_speed
    ),
    fill = "grey",
    alpha = .5
  ) +
  geom_smooth(
    colour = "black",
    span = 0.1,
    linetype = 2,
    size = .5
  ) +
  geom_dl(aes(label = monthAbb),
          method = list("smart.grid", cex = .6)) +
  theme_bw() +
  facet_wrap(~ season,
             scales = "free_y",
             ncol = 1,
             strip.position = "right") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    legend.position = "none",
    title = element_text(size = 6),
    plot.margin = unit(c(.1, 0, .8, 1), "cm")
  ) +
  ggtitle(expression(paste(Wind ~ Speed ~ "(" * m ~ s ^ {
    -1
  } * ")")))

mon_gamm <- cowplot::plot_grid(
  air_temp_facet,
  sea_temp_facet,
  wind_speed_facet,
  ncol = 3,
  label_size = 11,
  rel_heights = c(1, 1, 1.2),
  labels = c("(a)", "(b)", "(c)")
)

title_mon_gamm <- ggdraw() +
  draw_label("GAMM predictions of environment descriptors",
             fontface = 'italic',
             size = 10)

pdf("sup_figure_4.pdf", height = 8, onefile = F)
cowplot::plot_grid(
  title_mon_gamm,
  mon_gamm,
  ncol = 1,
  nrow = 2,
  rel_heights = c(0.05, 1)
)
dev.off()
