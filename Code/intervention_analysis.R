library(TelemetryR); library(lubridate); library(dplyr)

detects <- vemsort('P:/OBrien/Biotelemetry/Detections/Offshore MD/BSB Noise Impact')
detects <- filter(detects,
                  transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1)),
                  date.local <= '2016-12-31',
                  date.local >= '2016-06-12') %>%
  mutate(agg.date = floor_date(date.local, unit = 'day'),
         array = ifelse(grepl('Outer', station), 'Southern',
                        ifelse(grepl('Middle', station), 'Middle',
                               'Northern')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))

date.fill <- data.frame(agg.date = rep(seq(as.Date('2016-06-12'),
                                           as.Date('2016-12-31'),
                                           by = 'day'),
                                       each = 3),
                        array = ordered(rep(c('Northern', 'Middle', 'Southern'),
                                            times = 203),
                                        levels = c('Northern', 'Middle', 'Southern')),
                        stringsAsFactors = F)
date.fill$agg.date <- ymd(date.fill$agg.date, tz = 'America/New_York')

detect.ts <- detects %>%
  distinct(transmitter, array, agg.date, .keep_all =T) %>%
  group_by(array, agg.date) %>%
  summarize(n = n()) %>%
  full_join(date.fill) %>%
  mutate(n = ifelse(is.na(n), 0, n))




library(tsoutliers)
outliers <- sapply(levels(detect.ts$array),
       function(z){
         temporary <- filter(detect.ts, array == z) %>%
           arrange(agg.date)
         temporary <- ts(temporary[, 'n'], start = c(2016, 164), frequency = 365)
         tso(temporary, types = c('LS', 'TC'), tsmethod = 'auto.arima')
       },
       simplify = F, USE.NAMES = T)

plot(outliers$Northern)
lmtest::coeftest(outliers$Northern$fit)
