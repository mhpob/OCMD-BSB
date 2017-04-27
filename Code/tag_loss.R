library(readxl)
tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb.xlsx')

library(ggplot2)
ggplot() + geom_histogram(data = tagdat, aes(x = `Length\r\n(TL, mm)`))


source('p:/obrien/biotelemetry/ocmd-bsb/code/bsb_detection_import.R')

library(dplyr)
max.date <- filter(bsb.detects, grepl('Inn|Out|Mid', station)) %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc)) %>%
  mutate(array = ifelse(grepl('5[0-9]$|6[0-4]$', transmitter), 'Outer',
                        ifelse(grepl('6[5-9]$|7[0-9]$', transmitter), 'Inner',
                               'Middle')))

library(lubridate)
max.date$interval <- interval(ymd_hms('2016-06-09 00:00:00'), max.date$max)

surv.all <- data.frame(date = seq.Date(ymd('2016-06-09'), ymd('2016-11-01'),
                                   by = 'day'))
surv.all$array <- 'All'
surv.all$num <- sapply(surv.all$date,
                             function(x) sum(x %within% max.date$interval == T))
surv.all$pct <- surv.all$num/45


surv.arr <- split(max.date, max.date$array)

surv.arr <- sapply(surv.arr, function(x) sapply(surv.all$date,
                                 function(y) sum(y %within% x$interval == T)))
surv.arr <- cbind(date = surv.all$date, as.data.frame(surv.arr))

library(reshape2)
surv.arr <- melt(surv.arr, id = 'date', measure = c('Inner', 'Middle', 'Outer'),
             variable.name = 'array', value.name = 'num')
surv.arr$pct <- surv.arr$num/15

surv.all <- rbind(surv.all, surv.arr)

ggplot() + geom_line(data = surv.all,
                     aes(x = date, y = pct, col = array),
                     lwd = 2) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  labs(x = 'Date', y = 'Percent remaining in array', color = 'Array') +
  theme_bw()


