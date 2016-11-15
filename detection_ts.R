library(TelemetryR); library(lubridate); library(dplyr)
detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
detects <- filter(detects,
                  transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1))) %>%
  mutate(agg.date = ceiling_date(date.local, unit = 'hour'))



j <- distinct(detects, transmitter, station, agg.date) %>%
  group_by(transmitter, agg.date)
k <- filter(j, grepl('Middle', station)) %>%
  summarize(n=n())


library(ggplot2)
ggplot() + geom_smooth(data = k, aes(x = agg.date, y = n)) +
  geom_point(data = k, aes(x = agg.date, y = n)) +
  ylim(c(1,3)) +
  facet_wrap(~transmitter)
