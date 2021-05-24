library(TelemetryR); library(lubridate); library(dplyr)

detects <- readRDS('data/bsb_detections.rds')
detects <- filter(detects,
                  transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1)),
                  grepl('Inner|Outer|Middle', station),
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
  distinct(transmitter, array, agg.date, .keep_all = T) %>%
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
         tso(temporary, types = c('LS', 'TC'), tsmethod = 'auto.arima',
             cval = 5)
       },
       simplify = F, USE.NAMES = T)

plot(outliers$Northern,
     args.lines.y = list(col = gray.colors(6)[2]),
     args.effects.axis = list(xaxt = 'n'),
     args.lines.effects = list(col = 'black', lty = 4),
     args.x.axis = list(at = seq(ymd('2016-06-12'), ymd('2016-12-31'),
                                 by = 'month', format = '%b')))
lmtest::coeftest(outliers$Northern$fit)

# $y is original time series, $yadj is fitted time series, $effects are effects
# Adjust plot.tsoutliers function to remove titles

p.tso <- function(array){
  par(mar = c(0, 2, 0, 2), oma = c(2, 0, 1.5, 0), mfcol = c(2, 1))
  plot(cbind(outliers[[array]]$y, outliers[[array]]$yadj), plot.type = 's',
       type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  lines(outliers[[array]]$y, col = gray.colors(6)[2])
  lines(outliers[[array]]$yadj)
  points(x = outliers[[array]]$times,
         y = outliers[[array]]$y[outliers[[array]]$outliers[, "ind"]], col = 'red')
  axis(2, at = pretty(outliers[[array]]$y), padj = 0.5)
  mtext(side = 3, text = paste(array, 'Site'), adj = 0)

  plot(x = seq(ymd('2016-06-12'), ymd('2016-12-31'), by = 'day'),
       y = outliers[[array]]$effects,
       col = 'black', type = 's', lty = 2, xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', bty = 'u')
  axis(4, at = pretty(outliers[[array]]$effects), padj = -0.5)

  axis.Date(1, at = seq(ymd('2016-06-01'), ymd('2017-01-01'), by = 'month'),
            format = '%b', padj = -0.5)
}

setEPS()
postscript("whatever.eps", width = 2.2, height = 2.1)
p.tso('Northern')
dev.off()
