library(readxl)
tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb_16.xlsx')

library(ggplot2)
# ggplot() + geom_histogram(data = tagdat, aes(x = `Length\r\n(TL, mm)`))


bsb.detects <- readRDS('data/bsb_detections.rds')

library(dplyr)
max.date <- filter(bsb.detects, grepl('Inn|Out|Mid', station),
                   date.utc <= '2016-12-31') %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc)) %>%
  mutate(array = ifelse(grepl('5[0-9]$|6[0-4]$', transmitter), 'Southern',
                        ifelse(grepl('6[5-9]$|7[0-9]$', transmitter), 'Northern',
                               'Middle')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))

library(lubridate)
max.date$interval <- interval(ymd_hms('2016-06-09 00:00:00'), max.date$max)

surv.all <- data.frame(date = seq.Date(ymd('2016-06-09'), ymd('2016-12-31'),
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
surv.arr <- melt(surv.arr, id = 'date',
                 measure = c('Northern', 'Middle', 'Southern'),
             variable.name = 'array', value.name = 'num')
surv.arr$pct <- surv.arr$num/15

surv.all <- rbind(surv.all, surv.arr)
surv.all$array <- ordered(surv.all$array,
                          levels = c('All', 'Northern', 'Middle', 'Southern'))

ggplot() + geom_line(data = surv.arr,
                     aes(x = date, y = num, col = array),
                     lwd = 1) +
  scale_color_manual(values = c('red', 'blue', 'purple')) +
  geom_vline(xintercept = as.numeric(as.Date('2016-09-02')),
             linetype = 5) +
  geom_vline(xintercept = as.numeric(as.Date('2016-09-06')),
             linetype = 5) +
  labs(x = 'Date', y = 'Fish remaining in array', color = 'Site') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', minor_breaks = NULL) +
  theme_bw() +
  theme(legend.position = 'none')


surv.all$doy <- yday(surv.all$date)
model_results <- sapply(levels(surv.all$array),
       function(x) summary(lm(num ~ doy,
                      data = distinct(surv.all, array, num, .keep_all = T),
                      subset = (array == x))),
       simplify = F, USE.NAMES = T)

anova(lm(num ~ doy + array,
         data =  distinct(surv.all, array, num, .keep_all = T),
         subset = (array != 'All')))
