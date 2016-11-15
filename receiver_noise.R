library(lubridate); library(dplyr)

log <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/vue_export.csv',
                stringsAsFactors = F)
log$Date.Time <- ymd_hms(log$Date.Time)
log <- filter(log, grepl('depth|angle|temperature|noise', Description),
              Date.Time > ymd_hms('2016-06-12 23:59:59')) %>%
  mutate(Date.Time = ceiling_date(Date.Time, unit = 'hour'))


log$Data <- as.numeric(log$Data)

log$array <- ifelse(grepl('01|08|09', log$Receiver), 'Inner',
             ifelse(grepl('03|04|07', log$Receiver), 'Middle', 'Outer'))


library(TelemetryR)
detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
detects <- filter(detects,
                  date.local > ymd_hms('2016-06-12 23:50:00', tz = 'America/New_York'),
                  transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)))

ten_ceiling <- function(x){
  as.POSIXct(ceiling(as.numeric(x)/(10*60))*(10*60),
           origin='1970-01-01', tz = 'UTC')}

# detects$agg.date <- ten_ceiling(detects$date.utc)
detects$agg.date <- ceiling_date(detects$date.local, unit = 'hour')
names(detects)[2] <- 'Receiver'

j <- distinct(detects, Receiver, transmitter, agg.date) %>%
  group_by(Receiver,agg.date)
k <- summarize(j, n=n())

library(ggplot2)
temp.dat <- filter(log, Description == 'Average noise')
ggplot() + geom_smooth(data = temp.dat,
                       aes(x = Date.Time, y = Data)) +
  facet_wrap(~ Receiver, dir = 'v')

ggplot() + geom_smooth(data = k,
                       aes(x = agg.date, y = n)) +
  facet_wrap(~ Receiver, dir = 'v')


test <- left_join(temp.dat, k, by = c("Receiver", "Date.Time" = "agg.date"))
ggplot() + geom_point(data = filter(test, Data > 50), aes(Data, n)) +
  labs(x = 'Noise', y = 'Detections')



