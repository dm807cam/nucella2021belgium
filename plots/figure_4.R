library("tidyverse")
library("cowplot")
library("scales")
library("directlabels")
library("cmocean")
library("mgcv")

# Import helper file. This file contain proprietary code and
# will not be supplied with the rest of the code.
# All functions used from this file will be highlighted in the script.
source("../imp_func.R", local = TRUE)

# Import data
belgium_sea_temperature <-
  read_delim("../data/env/sea_surface_temperature.csv")
gamm_threshold <-
  readRDS("../data/env/GAMM_calcification_windows.rds")

new_sea <- list()
pred_new_sea <- list()
for (i in 10:15) {
  new_sea <- with(belgium_sea_temperature,
                  data.frame(
                    year = seq(min(year),
                               max(year),
                               length = 1000),
                    calc_threshold = i
                  ))
  
  tmp0 <- predict(
    gamm_threshold$gam,
    newdata = new_sea,
    se.fit = TRUE,
    type = "response"
  )
  tmp <- data.frame(cbind(tmp0$fit, tmp0$se.fit, i))
  pred_new_sea[[i]] <- tmp
}
pred_sea <- do.call(rbind, pred_new_sea)
names(pred_sea) <- c("fit", "se_fit", "calc_threshold")

sea_fit <- data.frame(
  cbind(
    px = as.numeric(new_sea$year),
    py = as.numeric(pred_sea$fit),
    pse = as.numeric(pred_sea$se_fit),
    calc_threshold = pred_sea$calc_threshold
  )
)

p1 <-
  ggplot(sea_fit, aes(px, py, group = as.factor(calc_threshold))) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(a)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  ) +
  geom_line(colour = "black",
            linetype = 2,
            size = 0.5) +
  geom_dl(aes(label = paste(" SST > ", calc_threshold, " °C", sep = "")),
          method = list("last.points", cex = 0.4)) +
  scale_fill_cmocean(start = 0.2, end = 0.8, name = "thermal") +
  geom_ribbon(aes(
    ymin = py - 2 * pse,
    ymax = py + 2 * pse,
    fill = calc_threshold
  ),
  alpha = 0.25) +
  theme_science() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    legend.background = element_blank(),
    aspect.ratio = 1
  ) +
  scale_y_continuous(name = "Number of days") +
  scale_x_continuous(
    limits = c(1890, 2065),
    expand = c(0.015, 0.015),
    breaks = pretty_breaks(n = 6)
  )

datalist <- list()
for (i in unique(sea_fit$calc_threshold)) {
  kachy <- filter(sea_fit, calc_threshold == i)
  tmp_max <- kachy$py[which(kachy == 2019)]
  tmp_max_se <- kachy$pse[which(kachy == 2019)]
  tmp_min <- kachy$py[which(kachy == 1880)]
  tmp_min_se <- kachy$pse[which(kachy == 1880)]
  abs_diff <- tmp_max - tmp_min
  perc_diff <- (tmp_max - tmp_min) / tmp_min * 100
  tmp <-
    c(tmp_min,
      tmp_min_se,
      tmp_max,
      tmp_max_se,
      abs_diff,
      perc_diff,
      i - 0.25,
      i + 0.25)
  datalist[[i]] <- tmp
}
ext_pred_sea <- as.data.frame(do.call(rbind, datalist))
names(ext_pred_sea) <- c(
  "min_fit",
  "min_se_fit",
  "max_fit",
  "max_se_fit",
  "change",
  "percent_change",
  "calc_threshold",
  "calc_threshold2"
)

write.csv(ext_pred_sea,
          "../data/env/calcification_percent_estimates.csv",
          row.names = FALSE)

p2 <- ggplot() +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "(b)",
    vjust = 1.5,
    hjust = -.25,
    parse = TRUE,
    size = 3
  ) +
  geom_col(
    ext_pred_sea,
    mapping = aes(calc_threshold, change),
    colour = "black",
    alpha = 0.8,
    width = 0.4
  ) +
  geom_col(
    ext_pred_sea,
    mapping = aes(calc_threshold2, percent_change),
    fill = "darkblue",
    colour = "black",
    alpha = 0.8,
    width = 0.4
  ) +
  theme_science() +
  theme(axis.title.y.right = element_text(colour = "darkblue"),
        aspect.ratio = 1) +
  scale_y_continuous(
    name = "Est. absolute change (days)",
    sec.axis = sec_axis(trans = ~ ., name = "Est. relative change (%)"),
    expand = c(0, 0),
    limits = c(0, 150)
  ) +
  scale_x_continuous(name = "Temperature threshold (°C)", breaks = c(10:15))

pdf("figure_4.pdf",
    height = 4.2,
    width = 3,
    onefile = F)
cowplot::plot_grid(
  p1,
  p2,
  nrow = 2,
  rel_heights = c(0.95, 1),
  align = "v"
)
dev.off()
