library(lubridate); library(dplyr)

log <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/receiver logs/vue_export.csv',
                stringsAsFactors = F)
log$Date.Time <- ymd_hms(log$Date.Time)
log <- filter(log, grepl('depth|angle|temperature|noise', Description),
              Date.Time > ymd_hms('2016-06-12 23:59:59'))

log$Data <- as.numeric(log$Data)

log$array <- ifelse(grepl('01|08|09', log$Receiver), 'Inner',
             ifelse(grepl('03|04|07', log$Receiver), 'Middle', 'Outer'))

# library(reshape2)
# test <- dcast(data = log, Date.Time + Receiver ~ Description, value.var = 'Data')

library(ggplot2)
temp.dat <- filter(log, Description == 'Average noise')
ggplot() + geom_smooth(data = temp.dat,
                       aes(x = Date.Time, y = Data)) +
  facet_wrap(array ~ Receiver, dir = 'v')
