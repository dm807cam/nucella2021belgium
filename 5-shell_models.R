library("tidyverse")
library("mgcv")
library("DHARMa")
library("gratia")
library("car")

# Import helper file. This file contain proprietary code and 
# will not be supplied with the rest of the code. 
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local=TRUE)

# Import data 
nucella_PCs <- read_delim("data/shell/shell_outline_PCA.csv")
# Remove ID precursor from sample name
nucella_PCs <- nucella_PCs %>% 
  mutate(sample = str_replace(sample, "ID", "")) %>% 
  mutate(sample = str_replace(sample, "-", "_"))
glimpse(nucella_PCs)

nucella_caliper <- read_delim("data/shell/shell_caliper.csv")
glimpse(nucella_caliper)

nucella_thickness_o1 <- read_delim("data/shell/thickness/nucella_thickness_o1a.csv")
glimpse(nucella_thickness_o1)

nucella_thickness_o2 <- read_delim("data/shell/thickness/nucella_thickness_o2a.csv")
glimpse(nucella_thickness_o2)

nucella_thickness_o3 <- read_delim("data/shell/thickness/nucella_thickness_o3a.csv")
glimpse(nucella_thickness_o3)

nucella_storage <- read_delim("data/shell/sample_storage.csv")
glimpse(nucella_storage)

# Assign session name to shell thickness data
nucella_thickness_o1$session <- "o1"
nucella_thickness_o2$session <- "o2"
nucella_thickness_o3$session <- "o3"

# Combine shell layer thickness into single data frame
nucella_thickness <- na.omit(rbind(
  nucella_thickness_o1,
  nucella_thickness_o2,
  nucella_thickness_o3))

write.csv(nucella_thickness, "data/shell/nucella_thickness.csv", row.names = FALSE)


# Convert shell layer thickness from um to mm
nucella_thickness$aragonite_thickness_mm <- nucella_thickness$aragonite_thickness_um / 1000
nucella_thickness$calcite_thickness_mm <- nucella_thickness$calcite_thickness_um / 1000

# Combine storage conditions data frame with shell layer thickness data frame
nucella_thickness <- full_join(nucella_thickness, nucella_storage, by="sample")

# Check for consistency between measurement session and sampling dates
ggplot(nucella_thickness, aes(x=calcite_thickness_mm/aragonite_thickness_mm, fill=session)) + 
  theme_bw() +
  geom_density(colour = "black", alpha = 0.2) +
  facet_wrap(~date, scales = "free")
# Looks very consistent!

# Check for signs of dissolution
p1 <- ggplot(nucella_thickness, aes(x=storage_condition, y=aragonite_thickness_mm, fill=storage_condition)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position=position_jitter(0.2), pch=21, alpha=0.2, colour="black", fill="white") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(.3,1,.2,1), "cm")) +
  ylab(expression(paste(Aragonite~layer~thickness~"(mm)")))

p2 <- ggplot(nucella_thickness, aes(x=storage_condition, y=calcite_thickness_mm, fill=storage_condition)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(position=position_jitter(0.2), pch=21, alpha=0.2, colour="black", fill="white") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(.3,1,.2,1), "cm")) +
  ylab(expression(paste(Calcite~layer~thickness~"(mm)")))

dis_check <- cowplot::plot_grid(p1,p2,ncol=2, labels = c("(a)", "(b)"))

ggsave("plots/dissolution_check.svg", 
       plot = sal_thick,
       width = 21, 
       height = 9, 
       units = "cm")

# Save GGplot2 object
saveRDS(dis_check, "plots/GGPLOT_dissolution_check.rds")

# Calculate shell layer thickness averages for every specimen
nucella_thickness_m <- nucella_thickness %>%
  group_by(sample) %>%
  summarise(calcite_thickness_mm = mean(calcite_thickness_mm, na.rm = TRUE),
            aragonite_thickness_mm = mean(aragonite_thickness_mm, na.rm = TRUE),
            storage_condition = storage_condition[1])

# Merge all shell descriptors to single data frame
nucella_shape <- full_join(nucella_thickness_m, nucella_caliper)
nucella_shape <- full_join(nucella_shape, nucella_PCs)

# Estimate aperture area from height and width measurements
nucella_shape$aperture_ellipses <- pi * (0.5 * nucella_shape$aperture_width) * 
  (0.5 * nucella_shape$aperture_height)

write.csv(nucella_shape, "data/shell/shell_shape.csv", row.names = FALSE)

# Data exploration --------------------------------------------------------
# Inspecting the data frame
str(nucella_shape)

# NAs in the data frame could cause problems with the models, check if NAs are present
colSums(is.na(nucella_shape)) 
# Missing data for shell layer thickness

# Outlier detection using boxplots
par(mfrow=c(2,3), mai = c(0.3,0.3,0.3,0.3))
boxplot(nucella_shape$aperture_ellipses, main = "Aperture Size", xlab=NA, ylab=NA, pch = 20, cex = 2, pars=list(outcol="red"))
boxplot(nucella_shape$calcite_thickness_mm, main = "Calcite Layer \n Thickness", xlab=NA, ylab=NA, pch = 20, cex = 2, pars=list(outcol="red"))
boxplot(nucella_shape$aragonite_thickness_mm, main = "Aragonite Layer \n Thickness", xlab=NA, ylab=NA, pch = 20, cex = 2, pars=list(outcol="red"))
boxplot(nucella_shape$PC1, main = "PC1 \n Shell Shape", xlab=NA, ylab=NA, pch = 20, cex = 2, pars=list(outcol="red"))
boxplot(nucella_shape$PC2, main = "PC2 \n Shell Shape", xlab=NA, ylab=NA, pch = 20, cex = 2, pars=list(outcol="red"))
# All variables show a few potential outliers.
# This is no problem at this stage but could cause issues with the model later on.

# Normality 
par(mfrow=c(2,3), mai = c(0.3,0.3,0.3,0.3))
qqnorm(nucella_shape$aperture_ellipses, main = "Aperture Size", pch = 1, frame = TRUE, xlab=NA, ylab=NA)
qqline(nucella_shape$aperture_ellipses, col = "blue", lwd = 2, lty="dashed")
mtext(paste("Shapiro-Wilk's = ",round(as.numeric(shapiro.test(nucella.shape$aperture.ellipses)$p.value),2)), line = -2, adj = 0.1)

qqnorm(nucella_shape$calcite_thickness_mm, main = "Calcite Layer \n Thickness", pch = 1, frame = TRUE, xlab=NA, ylab=NA)
qqline(nucella_shape$calcite_thickness_mm, col = "blue", lwd = 2, lty="dashed")
mtext(paste("Shapiro-Wilk's = ",round(as.numeric(shapiro.test(nucella.shape$calcite.thickness.mm)$p.value),2)), line = -2, adj = 0.1)

qqnorm(nucella_shape$aragonite_thickness_mm, main = "Aragonite Layer \n Thickness", pch = 1, frame = TRUE, xlab=NA, ylab=NA)
qqline(nucella_shape$aragonite_thickness_mm, col = "blue", lwd = 2, lty="dashed")
mtext(paste("Shapiro-Wilk's = ",round(as.numeric(shapiro.test(nucella.shape$aragonite.thickness.mm)$p.value),2)), line = -2, adj = 0.1)

qqnorm(nucella_shape$PC1, main = "PC1 \n Shell Shape", pch = 1, frame = TRUE, xlab=NA, ylab=NA)
qqline(nucella_shape$PC1, col = "blue", lwd = 2, lty="dashed")
mtext(paste("Shapiro-Wilk's = ",round(as.numeric(shapiro.test(nucella.shape$PC1)$p.value),2)), line = -2, adj = 0.1)

qqnorm(nucella_shape$PC2, main = "PC2 \n Shell Shape", pch = 1, frame = TRUE, xlab=NA, ylab=NA)
qqline(nucella_shape$PC2, col = "blue", lwd = 2, lty="dashed")
mtext(paste("Shapiro-Wilk's = ",round(as.numeric(shapiro.test(nucella.shape$PC2)$p.value),2)), line = -2, adj = 0.1)
# aperture size is not normally distributed
# calcite thickness is not normally distributed
# aragonite thickness is not normally distributed
# PC1 is normally distributed
# PC2 is normally distributed
# NOTE: The QQ-plot ignore the interaction with the covariates meaning that not 
# normally distributed dependent variables could be normally distributed in the model.
# We can continue but need to bear in mind that some of the dependent variables are not normally distributed.

# Homogeneity of variance
leveneTest(nucella_shape$aperture_ellipses,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)

leveneTest(nucella_shape$calcite_thickness_mm,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)

leveneTest(nucella_shape$aragonite_thickness_mm,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)

leveneTest(nucella_shape$aperture_ellipses,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)

leveneTest(nucella_shape$PC1,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)

leveneTest(nucella_shape$PC2,
           as.factor(nucella_shape$year),
           location = c("median"),
           trim.alpha = 0.25)
# All looking good

# Multicollinearity among independent variables
# This function is called from the imp_function script and not included in the public available version.
corvif(nucella_shape[c("year", "shell_height")])
# VIFs are well below 1.1. There appears to be now problem with multicollinearity.

# Relationships among dependent and independent variables
# Aperture Size
par(mfrow=c(1,3))
plot(aperture_ellipses ~ year, nucella_shape, ylab="Aperture Size", xlab="Year", pch=16, cex=1)
abline(lm(aperture_ellipses ~ year, nucella_shape), col="blue", lty=2)
plot(aperture_ellipses ~ shell_height, nucella_shape, ylab="Aperture Size", xlab="Shell Height", pch=16, cex=1)
abline(lm(aperture_ellipses ~ shell_height, nucella_shape), col="blue", lty=2)
boxplot(aperture_ellipses ~ location, nucella_shape, ylab="Aperture Size", xlab="Location", pch=16, cex=1, pars=list(outcol="red"))
abline(lm(aperture_ellipses ~ as.numeric(location), nucella_shape), col="blue", lty=2)
# Good relationship between Aperture Size and Year
# Strong relationship between Aperture Size and Shell Height.
# Good relationship between Aperture Size and Location
# Relationship between dependent and independent variables can be assumed to be linear

# Calcite Thickness
par(mfrow=c(1,3))
plot(calcite_thickness_mm ~ year, nucella_shape, ylab="Calcite Layer Thickness", xlab="Year", pch=16, cex=1)
abline(lm(calcite_thickness_mm ~ year, nucella_shape), col="blue", lty=2)
plot(calcite_thickness_mm ~ shell_height, nucella_shape, ylab="Calcite Layer Thickness", xlab="Shell Height", pch=16, cex=1)
abline(lm(calcite_thickness_mm ~ shell_height, nucella_shape), col="blue", lty=2)
boxplot(calcite_thickness_mm ~ location, nucella_shape, ylab="Calcite Layer Thickness", xlab="Location", pch=16, cex=1, pars=list(outcol="red"))
abline(lm(calcite_thickness_mm ~ as.numeric(location), nucella_shape), col="blue", lty=2)
# Possibly relationship between Calcite Layer Thickness and Year
# Good relationship between Calcite Layer Thickness and Shell Height
# Possibly relationship between calcite Thickness and Location
# Relationship between dependent and independent variables can be assumed to be linear

# Aragonite Thickness
par(mfrow=c(1,3))
plot(aragonite_thickness_mm ~ year, nucella_shape, ylab="Aragonite Layer Thickness", xlab="Year", pch=16, cex=1)
abline(lm(aragonite_thickness_mm ~ year, nucella_shape), col="blue", lty=2)
plot(aragonite_thickness_mm ~ shell_height, nucella_shape, ylab="Aragonite Layer Thickness", xlab="Shell Height", pch=16, cex=1)
abline(lm(aragonite_thickness_mm ~ shell_height, nucella_shape), col="blue", lty=2)
boxplot(aragonite_thickness_mm ~ location, nucella_shape, ylab="Aragonite Layer Thickness", xlab="Location", pch=16, cex=1, pars=list(outcol="red"))
abline(lm(aragonite_thickness_mm ~ as.numeric(location), nucella_shape), col="blue", lty=2)
# Non linear relationship between Aragonite Layer Thickness and Year
# Non linear relationship between Aragonite Layer Thickness and Shell Height
# No obvious relationship between Aragonite Layer Thickness and Location
# Relationship between dependent and independent variables can be assumed to be linear

# PC1
par(mfrow=c(1,3))
plot(PC1 ~ year, nucella_shape, ylab="PC1 ", xlab="Year", pch=16, cex=1)
abline(lm(PC1 ~ year, nucella_shape), col="blue", lty=2)
plot(PC1 ~ shell_height, nucella_shape, ylab="PC1", xlab="Shell Height", pch=16, cex=1)
abline(lm(PC1 ~ shell_height, nucella_shape), col="blue", lty=2)
boxplot(PC1 ~ location, nucella_shape, ylab="PC1", xlab="Location", pch=16, cex=1, pars=list(outcol="red"))
abline(lm(PC1 ~ as.numeric(location), nucella_shape), col="blue", lty=2)
# Non linear relationship between PC1 and Year
# No obvious relationship between PC1 and Shell Height
# Possibly a small relationship between PC1 and Location
# Relationship between dependent and independent variables can be assumed to be linear

# PC2
par(mfrow=c(1,3))
plot(PC2 ~ year, nucella_shape, ylab="PC2", xlab="Year", pch=16, cex=1)
abline(lm(PC2 ~ year, nucella_shape), col="blue", lty=2)
plot(PC2 ~ shell_height, nucella_shape, ylab="PC2", xlab="Shell Height", pch=16, cex=1)
abline(lm(PC2 ~ shell_height, nucella_shape), col="blue", lty=2)
boxplot(PC2 ~ location, nucella_shape, ylab="PC2", xlab="Location", pch=16, cex=1, pars=list(outcol="red"))
abline(lm(PC2 ~ as.numeric(location), nucella_shape), col="blue", lty=2)
# No obvious relationship between PC2 and Year
# No obvious relationship between PC2 and Shell Height
# No obvious relationship between PC2 and Location
# Relationship between dependent and independent variables can be assumed to be linear

#---
# We have multiple observation per location and year. Which could become a problem. 
# Not all relationships are perfectly linear.
# We might need to consider adding location as a random effect.
# We need to address temporal autocorrelation later in the model. Check ACFs and pACFs.
# Fit GAMs 

# Model fitting -----------------------------------------------------------
nucella_shape$location <- as.factor(nucella_shape$location)

# GAM: Shell height
m_sh_null <- gam(shell_height ~ 1,
                 family = gaussian(link = "identity"), 
                 data = nucella_shape)

m_sh_I <- gam(shell_height ~ 
                s(year, k=3, bs="cr") + 
                s(location, bs="re"), 
              family = gaussian(link = "identity"), 
              data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_sh_I)
DHARMa::plotQQunif(m_sh_I)
acf(resid(m_sh_I), lag.max = 36, main = "ACF")
pacf(resid(m_sh_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_sh_I)
gratia::draw(m_sh_I)
summary(m_sh_I)

# vs Null-model
AIC(m_sh_I, m_sh_null)
anova.gam(m_sh_I, m_sh_null, test="Chisq")
# Signficantly better than null-model

# ---

# GAM: Aperture Size 
m_as_null <- gam(aperture_ellipses ~ 1,
                 family = gaussian(link = "identity"), 
                 data = nucella_shape)

m_as_I <- gam(aperture_ellipses ~ 
                s(year, k=3, bs="tp") + 
                s(shell_height, k=3, bs="tp") + 
                s(location, bs="re"), 
              family = gaussian(link = "identity"), 
              data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_as_I)
DHARMa::plotQQunif(m_as_I)
acf(resid(m_as_I), lag.max = 36, main = "ACF")
pacf(resid(m_as_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_as_I)
gratia::draw(m_as_I)
summary(m_as_I)

# vs Null-model
AIC(m_as_I, m_as_null)
anova.gam(m_as_I, m_as_null, test="Chisq")
# Signficantly better than null-model

# ---

# GAM: Calcite Layer Thickness 
m_ct_I <- gam(calcite_thickness_mm ~ 
                s(year, k=3, bs="tp") + 
                s(shell_height, k=3, bs="tp") + 
                s(location, bs="re"), 
              family = gaussian(link = "identity"), 
              data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_ct_I)
DHARMa::plotQQunif(m_ct_I)
acf(resid(m_ct_I), lag.max = 36, main = "ACF")
pacf(resid(m_ct_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_ct_I)
# Model diagnostic looks unsatisfying. Changing the model family link
# does not improve the diagnostic plots. log-transform dependent variable.
nucella_shape$calcite_thickness_mm_log <- log(nucella_shape$calcite_thickness_mm)

m_ct_null <- gam(calcite_thickness_mm_log ~ 1,
                 family = gaussian(link = "identity"), 
                 data = nucella_shape)

m_ct_II <- gam(calcite_thickness_mm_log ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_ct_II)
DHARMa::plotQQunif(m_ct_II)
acf(resid(m_ct_II), lag.max = 36, main = "ACF")
pacf(resid(m_ct_II), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_ct_II)
gratia::draw(m_ct_II)
summary(m_ct_II)

# vs Null-model
AIC(m_ct_II, m_ct_null)
anova.gam(m_ct_II, m_ct_null, test="Chisq")
# Signficantly better than null-model

# ---

#GAM: Aragonite Layer Thickness
m_ar_I <- gam(aragonite_thickness_mm ~ 
                s(year, k=3, bs="tp") + 
                s(shell_height, k=3, bs="tp") + 
                s(location, bs="re"), 
              family = gaussian(link = "identity"), 
              data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_ar_I)
DHARMa::plotQQunif(m_ar_I)
acf(resid(m_ar_I), lag.max = 36, main = "ACF")
pacf(resid(m_ar_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_ar_I)
# Model diagnostic looks unsatisfying. Changing the model family link
# does not improve the diagnostic plots. log-transform dependent variable.
nucella_shape$aragonite_thickness_mm_log <- log(nucella_shape$aragonite_thickness_mm)

m_ar_null <- gam(aragonite_thickness_mm_log ~ 1,
                 family = gaussian(link = "identity"), 
                 data = nucella_shape)

m_ar_II <- gam(aragonite_thickness_mm_log ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_ar_II)
DHARMa::plotQQunif(m_ar_II)
acf(resid(m_ar_II), lag.max = 36, main = "ACF")
pacf(resid(m_ar_II), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_ar_II)
gratia::draw(m_ar_II)
summary(m_ar_II)

# vs Null-model
AIC(m_ar_II, m_ar_null)
anova.gam(m_ar_II, m_ar_null, test="Chisq")
# Signficantly better than null-model

# ---

# PC1 Layer Thickness
m_pc1_null <- gam(PC1 ~ 1,
                  family = gaussian(link = "identity"), 
                  data = nucella_shape)

m_pc1_I <- gam(PC1 ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_pc1_I)
DHARMa::plotQQunif(m_pc1_I)
acf(resid(m_pc1_I), lag.max = 36, main = "ACF")
pacf(resid(m_pc1_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_pc1_I)
gratia::draw(m_pc1_I)
summary(m_pc1_I)

# vs Null-model
AIC(m_pc1_I, m_pc1_null)
anova.gam(m_pc1_I, m_pc1_null, test="Chisq")
# Signficantly better than null-model


# GAM: PC2 Layer Thickness
m_pc2_null <- gam(PC2 ~ 1,
                  family = gaussian(link = "identity"),
                  data = nucella_shape)

m_pc2_I <- gam(PC2 ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_pc2_I)
DHARMa::plotQQunif(m_pc2_I)
acf(resid(m_pc2_I), lag.max = 36, main = "ACF")
pacf(resid(m_pc2_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_pc2_I)
gratia::draw(m_pc2_I)
summary(m_pc2_I)

# vs Null-model
AIC(m_pc2_I, m_pc2_null)
anova.gam(m_pc2_I, m_pc2_null, test="Chisq")
# Signficantly better than null-model

# ---

# GAM: PC3 Layer Thickness
m_pc3_null <- gam(PC3 ~ 1,
                  family = gaussian(link = "identity"),
                  data = nucella_shape)

m_pc3_I <- gam(PC3 ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_pc3_I)
DHARMa::plotQQunif(m_pc3_I)
acf(resid(m_pc3_I), lag.max = 36, main = "ACF")
pacf(resid(m_pc3_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_pc3_I)
gratia::draw(m_pc3_I)
summary(m_pc3_I)

# vs Null-model
AIC(m_pc3_I, m_pc3_null)
anova.gam(m_pc3_I, m_pc3_null, test="Chisq")
# Signficantly better than null-model

# ---

# GAM: PC4 Layer Thickness
m_pc4_null <- gam(PC4 ~ 1,
                  family = gaussian(link = "identity"),
                  data = nucella_shape)

m_pc4_I <- gam(PC4 ~ 
                 s(year, k=3, bs="tp") + 
                 s(shell_height, k=3, bs="tp") + 
                 s(location, bs="re"), 
               family = gaussian(link = "identity"), 
               data = nucella_shape)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
DHARMa::plotResiduals(m_pc4_I)
DHARMa::plotQQunif(m_pc4_I)
acf(resid(m_pc4_I), lag.max = 36, main = "ACF")
pacf(resid(m_pc4_I), lag.max = 36, main = "pACF")
dev.off()

gratia::appraise(m_pc4_I)
gratia::draw(m_pc4_I)
summary(m_pc4_I)

# vs Null-model
AIC(m_pc4_I, m_pc4_null)
anova.gam(m_pc4_I, m_pc4_null, test="Chisq")
# Signficantly better than null-model

# Export all models -------------------------------------------------------
saveRDS(m_sh_I, "data/shell/GAM_shell_height.rds")
saveRDS(m_as_I, "data/shell/GAM_aperture_size.rds")
saveRDS(m_ct_II, "data/shell/GAM_calcite_thickness.rds")
saveRDS(m_ar_II, "data/shell/GAM_aragonite_thickness.rds")
saveRDS(m_pc1_I, "data/shell/GAM_shape_PC1.rds")
saveRDS(m_pc2_I, "data/shell/GAM_shape_PC2.rds")
saveRDS(m_pc3_I, "data/shell/GAM_shape_PC3.rds")
saveRDS(m_pc4_I, "data/shell/GAM_shape_PC4.rds")
