library(readxl)
tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb.xlsx')

library(ggplot2)
ggplot() + geom_histogram(data = tagdat, aes(x = `Length\r\n(TL, mm)`)) +
  theme_bw() +
  labs(x = 'Total Length (mm)', y = 'Count')


source('p:/obrien/biotelemetry/ocmd-bsb/code/bsb_detection_import.R')

library(dplyr)
max.date <- filter(bsb.detects, grepl('Inn|Out|Mid', station)) %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc))

tagdat <- inner_join(tagdat, max.date, by = c('Transmitter' = 'transmitter'))
tagdat$hermine <- ifelse(tagdat$max < '2016-09-03', 'Removed',
                  ifelse(tagdat$max > '2016-09-03' &
                           tagdat$max < '2016-09-04 12:00:00', 'Evacuated',
                         'Remained'))
tagdat$hermine <- ordered(tagdat$hermine,
                          levels = c('Removed', 'Evacuated', 'Remained'))

ggplot() + geom_histogram(data = tagdat,
                          aes(`Length\r\n(TL, mm)`,
                              fill = `Release Array`),
                          position = 'stack') +
  facet_wrap(~hermine, ncol = 1) +
  scale_fill_manual(values = c('red', 'blue', 'purple')) +
  labs(x = 'Total Length (mm)', y = 'Count', fill = 'Array') +
  theme_bw() +
  theme(legend.position = c(0.9, 0.5))


