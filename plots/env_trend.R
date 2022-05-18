library("tidyverse")
library("patchwork")
library("scales")
library("RColorBrewer")
library("gratia")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local = TRUE)

# Import data
belgium_air_temperature <-
  read_delim("data/env/air_temperature_comp.csv")
belgium_sea_temperature <-
  read_delim("data/env/sea_surface_temperature.csv")
belgium_wind_speed <- read_delim("data/env/wind_speed_comp.csv")

# Load GAMM data
gamm_air_temp <- readRDS("data/env/model_out/GAMM_air_temp.rds")
gamm_sea_temp <- readRDS("data/env/model_out/GAMM_sea_temp.rds")
gamm_wind_speed <- readRDS("data/env/model_out/GAMM_wind_speed.rds")

# Next steps follow Gavin Simpson's tutorial on derivative estimations :
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

# Predict air temperature -------------------------------------------------

want <- seq(1, nrow(belgium_air_temperature),
            length.out = 200)

pdatAT <- with(belgium_air_temperature,
               data.frame(year = year[want],
                          month = month[want]))

p2 <- data.frame(predict(
  gamm_air_temp$gam,
  newdata = pdatAT,
  type = "terms",
  se.fit = TRUE
))

pdatAT <- transform(pdatAT,
                    p2 = p2[, 1],
                    se2 = p2[, 4])

df.res <- df.residual(gamm_air_temp$gam)

crit.t <- qt(0.025, df.res, lower.tail = FALSE)

pdatAT <- transform(pdatAT,
                    upper = p2 + (crit.t * se2),
                    lower = p2 - (crit.t * se2))

gamm_air_temp.d <- fderiv(gamm_air_temp)

gamm_air_temp.dci <- confint(gamm_air_temp.d,
                             parm = "year",
                             type = "confidence")

gamm_air_temp.dsig <- signifD(
  pdatAT$p2,
  d = gamm_air_temp.d$derivatives$`s(year)`$deriv,
  gamm_air_temp.dci$upper,
  gamm_air_temp.dci$lower
)

# Plot air temperature -------------------------------------------------
ATY <- ggplot() +
  geom_point(
    belgium_air_temperature,
    mapping = aes(year,
                  air_temperature_celsius),
    fill = "grey",
    pch = 21,
    alpha = 0.15,
    colour = "black"
  ) +
  geom_ribbon(
    pdatAT,
    mapping = aes(
      year,
      ymin = lower + mean(belgium_air_temperature$air_temperature_celsius),
      ymax = upper + mean(belgium_air_temperature$air_temperature_celsius)
    ),
    fill = "black",
    alpha = .25
  ) +
  geom_line(pdatAT,
            mapping = aes(
              year,
              p2 + mean(belgium_air_temperature$air_temperature_celsius)
            ),
            linetype = 2) +
  geom_line(
    pdatAT,
    mapping = aes(
      year,
      unlist(gamm_air_temp.dsig$incr) + mean(belgium_air_temperature$air_temperature_celsius)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatAT,
    mapping = aes(
      year,
      unlist(gamm_air_temp.dsig$decr) + mean(belgium_air_temperature$air_temperature_celsius)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a2)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2,
                  end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylab(expression(AT ~ (degree * C))) +
  scale_x_continuous(expand = c(0.015, 0.015),
                     breaks = pretty_breaks(n = 6))

ATC <- ggplot(pdatAT, aes(year,
                          p2)) +
  geom_line(linetype = 2) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              fill = "black",
              alpha = .15) +
  geom_line(
    pdatAT,
    mapping = aes(year,
                  unlist(gamm_air_temp.dsig$incr)),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatAT,
    mapping = aes(year,
                  unlist(gamm_air_temp.dsig$decr)),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a1)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2,
                  end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  ylab(expression(AT ~ (degree * C * ":" ~ centred))) +
  scale_x_continuous(expand = c(0.015, 0.015),
                     breaks = pretty_breaks(n = 6))

# Predict sea surface temperature -------------------------------------------------

want <- seq(1, nrow(belgium_air_temperature),
            length.out = 200)

pdatST <- with(belgium_air_temperature,
               data.frame(year = year[want],
                          month = month[want]))

p2 <- data.frame(predict(
  gamm_sea_temp$gam,
  newdata = pdatST,
  type = "terms",
  se.fit = TRUE
))

pdatST <- transform(pdatST,
                    p2 = p2[, 1],
                    se2 = p2[, 4])

df.res <- df.residual(gamm_sea_temp$gam)

crit.t <- qt(0.025, df.res, lower.tail = FALSE)

pdatST <- transform(pdatST,
                    upper = p2 + (crit.t * se2),
                    lower = p2 - (crit.t * se2))

gamm_sea_temp.d <- fderiv(gamm_sea_temp)

gamm_sea_temp.dci <-
  confint(gamm_sea_temp.d, parm = "year", type = "confidence")

gamm_sea_temp.dsigST <- signifD(
  pdatST$p2,
  d = gamm_sea_temp.d$derivatives$`s(year)`$deriv,
  gamm_sea_temp.dci$upper,
  gamm_sea_temp.dci$lower
)

# Plot sea surface temperature -------------------------------------------------
STY <- ggplot() +
  geom_point(
    belgium_sea_temperature,
    mapping = aes(year,
                  sea_temperature_celsius),
    fill = "grey",
    pch = 21,
    alpha = .15,
    colour = "black"
  ) +
  geom_ribbon(
    pdatST,
    mapping = aes(
      year,
      ymin = lower + mean(belgium_sea_temperature$sea_temperature_celsius),
      ymax = upper + mean(belgium_sea_temperature$sea_temperature_celsius)
    ),
    fill = "black",
    alpha = .25
  ) +
  geom_line(pdatST,
            mapping = aes(
              year,
              p2 + mean(belgium_sea_temperature$sea_temperature_celsius)
            ),
            linetype = 2) +
  geom_line(
    pdatST,
    mapping = aes(
      year,
      unlist(gamm_sea_temp.dsigST$incr) + mean(belgium_sea_temperature$sea_temperature_celsius)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatST,
    mapping = aes(
      year,
      unlist(gamm_sea_temp.dsigST$decr) + mean(belgium_sea_temperature$sea_temperature_celsius)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b2)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2,
                  end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylab(expression(SST ~ (degree * C))) +
  scale_x_continuous(expand = c(0.015, 0.015),
                     breaks = pretty_breaks(n = 6))

STC <- ggplot(pdatST, aes(year,
                          p2)) +
  geom_line(linetype = 2) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              fill = "black",
              alpha = .15) +
  geom_line(
    pdatST,
    mapping = aes(year,
                  unlist(gamm_sea_temp.dsigST$incr)),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatST,
    mapping = aes(year, unlist(gamm_sea_temp.dsigST$decr)),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b1)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2,
                  end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  ylab(expression(SST ~ (degree * C * ":" ~ centred))) +
  scale_x_continuous(expand = c(0.015, 0.015), breaks = pretty_breaks(n = 6))

# Predict wind speed -------------------------------------------------
want <- seq(1, nrow(belgium_wind_speed), length.out = 200)
pdatWS <- with(belgium_wind_speed,
               data.frame(year = year[want], month = month[want]))

p2 <- data.frame(predict(
  gamm_wind_speed$gam,
  newdata = pdatWS,
  type = "terms",
  se.fit = TRUE
))

pdatWS <- transform(pdatWS, p2 = p2[, 1], se2 = p2[, 4])

df.res <- df.residual(gamm_wind_speed$gam)
crit.t <- qt(0.025, df.res, lower.tail = FALSE)
pdatWS <- transform(pdatWS,
                    upper = p2 + (crit.t * se2),
                    lower = p2 - (crit.t * se2))

gamm_wind_speed.d <- fderiv(gamm_wind_speed)

gamm_wind_speed.dci <- confint(gamm_wind_speed.d,
                               parm = "year",
                               type = "confidence")

gamm_wind_speed.dsig <- signifD(
  pdatWS$p2,
  d = gamm_wind_speed.d$derivatives$`s(year)`$deriv,
  gamm_wind_speed.dci$upper,
  gamm_wind_speed.dci$lower
)

# Plot wind speed -------------------------------------------------
WSY <- ggplot() +
  geom_point(
    belgium_wind_speed,
    mapping = aes(year, wind_speed),
    fill = "grey",
    pch = 21,
    alpha = 0.15,
    colour = "black"
  ) +
  geom_ribbon(
    pdatWS,
    mapping = aes(
      year,
      ymin = lower + mean(belgium_wind_speed$wind_speed),
      ymax = upper + mean(belgium_wind_speed$wind_speed)
    ),
    fill = "black",
    alpha = .25
  ) +
  geom_line(pdatWS,
            mapping = aes(year, p2 + mean(belgium_wind_speed$wind_speed)),
            linetype = 2) +
  geom_line(
    pdatWS,
    mapping = aes(
      year,
      unlist(gamm_wind_speed.dsig$incr) + mean(belgium_wind_speed$wind_speed)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatWS,
    mapping = aes(
      year,
      unlist(gamm_wind_speed.dsig$decr) + mean(belgium_wind_speed$wind_speed)
    ),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(c2)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2, end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylab(expression(WS ~ (m ~ s ^ {
    -1
  }))) +
  scale_x_continuous(expand = c(0.015, 0.015), breaks = pretty_breaks(n = 6)) +
  ylim(2, 14)

WSC <- ggplot(pdatWS, aes(year, p2)) +
  geom_line(linetype = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "black",
              alpha = .15) +
  geom_line(
    pdatWS,
    mapping = aes(year, unlist(gamm_wind_speed.dsig$incr)),
    colour = brewer.pal(n = 4, name = "Spectral")[1]
  ) +
  geom_line(
    pdatWS,
    mapping = aes(year, unlist(gamm_wind_speed.dsig$decr)),
    colour = brewer.pal(n = 4, name = "Spectral")[4]
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(c1)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Times"
  ) +
  theme_science() +
  scale_fill_grey(start = 0.2, end = 0.8) +
  theme(text = element_text(size = 10, family="Times"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  ylab(expression(WS ~ (m ~ s ^
                          {
                            -1
                          } * ":" ~ centred))) +
  scale_x_continuous(expand = c(0.015, 0.015), breaks = pretty_breaks(n = 6))

png("plots/env_trend.png", width = 210, height = 110, unit = "mm", res = 1200)
(ATC + STC + WSC) / (ATY + STY + WSY) + plot_layout(height = c(0.6,1))
dev.off()

# Export to vector for journal publication
library(Cairo)
cairo_ps("plots/env_trend.eps", family = "Times", width = 210/25.4, height = 110/25.4)
(ATC + STC + WSC) / (ATY + STY + WSY) + plot_layout(height = c(0.6,1))
dev.off()