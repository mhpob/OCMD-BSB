library(readxl)
tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb.xlsx')

library(ggplot2)
ggplot() + geom_histogram(data = tagdat, aes(x = `Length\r\n(TL, mm)`))


source('p:/obrien/biotelemetry/ocmd-bsb/code/bsb_detection_import.R')

library(dplyr)
max.date <- filter(bsb.detects, grepl('Inn|Out|Mid', station)) %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc))

tagdat <- inner_join(tagdat, max.date, by = c('Transmitter' = 'transmitter'))
tagdat$hermine <- ifelse(tagdat$max < '2016-09-03', 'removed',
                  ifelse(tagdat$max > '2016-09-03' &
                           tagdat$max < '2016-09-04 12:00:00', 'evac', 'remain'))

ggplot() + geom_histogram(data = tagdat,
                          aes(`Length\r\n(TL, mm)`,
                              fill = hermine),
                          alpha = 0.7,
                          position = 'identity') +
  facet_wrap(~hermine, ncol = 1)
