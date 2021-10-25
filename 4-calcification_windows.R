library("tidyverse")
library("mgcv")
library("DHARMa")
library("gratia")
library("forecast")

# Import helper file. This file contain proprietary code and 
# will not be supplied with the rest of the code. 
# All functions used from this file will be highlighted in the script.
source("imp_func.R", local=TRUE)

# Import data
df_coads <- read.table("data/env/COADS/MSG2.S.enh.180001.201912_1", 
                       fileEncoding="UTF-8-BOM", 
                       skip = 1, 
                       sep = "", 
                       header = TRUE, 
                       na.strings ="",
                       fill = TRUE)

# Select only data from 1890 to the present
df_coads <- filter(df_coads, YEAR >= 1880)

# Remove over-represented data by reducing the geographic range
df_coads <- filter(df_coads, BLA == 52 & BLO == 2 & BSZ == 2)
df_sea <- df_coads[,c(1,2,10,11,12)]

datalist <- list()
df_sea$perc <- NA
df_sea$numdays <- NA
df_sea$date <- as.Date(paste(df_sea$YEAR, df_sea$MON,1, sep="-"), "%Y-%m-%d")
for(i in 10:15) {
  # Estimate the percentage of days with T > calc_threshold
    for(j in 1:nrow(df_sea)) {
      df_sea$perc[j] <- round(as.numeric(1- pnorm(i, df_sea$M[j], df_sea$S[j])) * 100,2)
    }

    # Find the number of days in each month
    for(j in 1:nrow(df_sea)) {
      df_sea$numdays[j] <- numberOfDays(df_sea$date[j])
    }

    # Calculate the days with T > calc.threshold
    df_sea$days_thresh <- round(df_sea$numdays / 100 * df_sea$perc, 2)

    # Sum up the number of days with T > calc_threshold per year
    kachy <- df_sea %>% 
             group_by(YEAR) %>%
             summarise(sumdays = round(sum(days_thresh),0),
                       A = sum(N))

    # Rename data frame columns
    kachy$calc_threshold <- i
    datalist[[i]] <- kachy
    }
df_sea_year = do.call(rbind, datalist)
names(df_sea_year) <- c("year", "days", "numO", "calc_threshold")

# Check data  
ggplot(df_sea_year, aes(year, days, color=numO)) +
  geom_point()+
  facet_wrap(~calc_threshold)
# A couple very low values present which seem to coincide with low numbers of observations.

# Remove data with less than 100 observations.
df_sea_year <- filter(df_sea_year, numO >100)

ggplot(df_sea_year, aes(year, days, color=numO)) +
  geom_point()+
  facet_wrap(~calc_threshold)
# Better!!

# Model yearly calcification windows
ctrl <- list(niterEM = 0, msVerbose = TRUE, maxIter = 1e8, msMaxIter = 1e8, optimMethod="L-BFGS-B")

gamm_threshold <- gamm(days ~ s(year, by=calc_threshold, k=6, bs="tp"),
                       data=df_sea_year, 
                       method="REML",
                       family=gaussian(link = "identity"),
                       correlation = corARMA(form = ~ 1|year, p=4, q=1),
                       control = ctrl)

# Select best arima model using the auto.arima function in forecast
arma_res <- auto.arima(resid(gamm_threshold$lme, type = "normalized"),
                       stationary = TRUE, seasonal = FALSE)

arma_res$coef # Correct arima selected

# Check for autocorrelation
layout(matrix(1:2, ncol = 2))
res <- resid(gamm_threshold$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)
# Some autocorrelation still present but this is as good as it gets.

# Check model diagnostics
appraise(gamm_threshold$gam) 
# Some outliers present but main data looks very good.
draw(gamm_threshold$gam)
summary(gamm_threshold$gam)

# Export model -------------------------------------------------------
saveRDS(gamm_threshold, "data/env/GAMM_calcification_windows.rds")
