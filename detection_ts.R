library(TelemetryR); library(lubridate); library(dplyr)
detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
detects <- filter(detects,
                  transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1))) %>%
  mutate(agg.date = ceiling_date(date.local, unit = 'hour'))



j <- distinct(detects, transmitter, station, agg.date) %>%
  group_by(transmitter, agg.date)
k <- filter(j, grepl('Inner', station)) %>%
  summarize(n=n())


library(ggplot2)
ggplot() + geom_smooth(data = k, aes(x = agg.date, y = n)) +
  geom_point(data = k, aes(x = agg.date, y = n)) +
  ylim(c(1,3)) +
  facet_wrap(~transmitter)


bsb <- vemsort('p:/obrien/biotelemetry/detections/offshore md/marine mammal monitoring')
bsb <- filter(bsb,
                  transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)))



detects <- filter(detects,
              transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)),
              date.local < ymd_hms('2016-08-03 00:00:00', tz = 'America/New_York'))
j <- group_by(detects, station) %>% summarize(test = length(unique(transmitter)))
