library(TelemetryR); library(ggplot2); library(lubridate); library(dplyr)
detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
detects <- filter(detects,
                  transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1))) %>%
  mutate(agg.date = ceiling_date(date.local, unit = 'hour'),
         array = ifelse(grepl('Outer', station), 'Outer',
                 ifelse(grepl('Middle', station), 'Middle',
                              'Inner')))


# j <- distinct(detects, transmitter, station, agg.date) %>%
#   group_by(transmitter, agg.date)
# k <- filter(j, grepl('Inner', station)) %>%
#   summarize(n=n())

# ggplot() + geom_smooth(data = k, aes(x = agg.date, y = n)) +
#   geom_point(data = k, aes(x = agg.date, y = n)) +
#   ylim(c(1,3)) +
#   facet_wrap(~transmitter)

j <- distinct(detects, transmitter, station, agg.date, .keep_all =T) %>%
  group_by(transmitter, array, agg.date) %>%
  summarize(n = n())

ggplot() + geom_smooth(data = j, aes(x = agg.date, y = n)) +
  geom_point(data = j, aes(x = agg.date, y = n)) +
  facet_wrap(~array) +
  scale_y_continuous(breaks = c(1, 2, 3)) +
  labs(x = 'Date', y = expression(Receivers~hour^{-1})) +
  theme_bw()



bsb <- vemsort('p:/obrien/biotelemetry/detections/offshore md/marine mammal monitoring')
bsb <- filter(bsb,
                  transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)))



detects <- filter(detects,
              transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)),
              date.local < ymd_hms('2016-08-03 00:00:00', tz = 'America/New_York'))
j <- group_by(detects, station) %>% summarize(test = length(unique(transmitter)))
