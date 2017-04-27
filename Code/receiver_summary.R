library(reshape2); library(lubridate); library(dplyr)

log <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/vue_export.csv',
                stringsAsFactors = F)

slim.log <- filter(log, grepl('Tilt|Average [depth|temperature|noise]',
                              Description)) %>%
  mutate(Date.Time = ymd_hms(Date.Time),
         Data = as.numeric(Data),
         Date.Time = ceiling_date(Date.Time, unit = 'day')) %>%
  filter(Date.Time > ymd_hms('2016-06-12 23:59:59'))

slim.log <- dcast(slim.log, Date.Time + Receiver ~ Description,
           fun.aggregate = mean, value.var = 'Data')

slim.log <- filter(slim.log, `Average seawater depth` > 15)
slim.log <- melt(slim.log, id = c('Date.Time', 'Receiver'),
                 variable.name = 'Description',
                 value.name = 'Data')
slim.log <- mutate(slim.log,
                   array = ifelse(grepl('[1|8|9]$', Receiver), 'Inner',
                                  ifelse(grepl('[2|5|6]$', Receiver), 'Outer',
                                         'Middle')))

library(reshape2)
avg.log <- group_by(slim.log, Date.Time, Receiver, Description) %>%
  summarize(avg.data = mean(Data))


avg.log.wide <- dcast(avg.log, Date.Time + Receiver ~ Description,
                      fun.aggregate = mean, value.var = 'avg.data')
pairs(avg.log.wide[, c(1, 3:5)])


summ.log <- group_by(slim.log, Receiver, array, Description) %>%
  summarize(min = min(Data),
            avg = mean(Data),
            std = sd(Data),
            max = max(Data))



