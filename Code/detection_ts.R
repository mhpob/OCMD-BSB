library(TelemetryR); library(ggplot2); library(lubridate); library(dplyr)
detects <- readRDS('data/bsb_detections.rds')
detects <- filter(detects,
                  transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1)),
                  date.local <= '2016-12-31',
                  date.local >= '2016-06-12') %>%
  mutate(agg.date = floor_date(date.local, unit = 'day'),
         array = ifelse(grepl('Outer', station), 'Southern',
                 ifelse(grepl('Middle', station), 'Middle',
                              'Northern')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))


# j <- distinct(detects, transmitter, station, agg.date) %>%
#   group_by(transmitter, agg.date)
# k <- filter(j, grepl('Inner', station)) %>%
#   summarize(n=n())

# ggplot() + geom_smooth(data = k, aes(x = agg.date, y = n)) +
#   geom_point(data = k, aes(x = agg.date, y = n)) +
#   ylim(c(1,3)) +
#   facet_wrap(~transmitter)

# j <- distinct(detects, transmitter, station, agg.date, .keep_all =T) %>%
#   group_by(transmitter, array, agg.date) %>%
#   summarize(n = n())
#
# ggplot() + geom_smooth(data = j, aes(x = agg.date, y = n)) +
#   geom_point(data = j, aes(x = agg.date, y = n)) +
#   facet_wrap(~array) +
#   scale_y_continuous(breaks = c(1, 2, 3)) +
#   labs(x = 'Date', y = expression(Receivers~hour^{-1})) +
#   theme_bw()
date.fill <- data.frame(agg.date = rep(seq(as.Date('2016-06-12'),
                                       as.Date('2016-12-31'),
                                       by = 'day'),
                                       each = 3),
                        array = ordered(rep(c('Northern', 'Middle', 'Southern'),
                                    times = 203),
                                    levels = c('Northern', 'Middle', 'Southern')),
                        stringsAsFactors = F)
date.fill$agg.date <- ymd(date.fill$agg.date, tz = 'America/New_York')

j <- distinct(detects, transmitter, array, agg.date, .keep_all =T) %>%
  group_by(array, agg.date) %>%
  summarize(n = n()) %>%
  full_join(date.fill) %>%
  mutate(n = ifelse(is.na(n), 0, n))

ggplot() + geom_line(data = j, aes(x = agg.date, y = n)) +
  facet_wrap(~array)+
  lims(y = c(0, 15)) +
  labs(x = 'Date', y = 'Fish detected') +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b', minor_breaks = NULL) +
  theme_bw()

# bsb <- vemsort('p:/obrien/biotelemetry/detections/offshore md/marine mammal monitoring')
# bsb <- filter(bsb,
#                   transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)))
#
#
#
# detects <- filter(detects,
#               transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)),
#               date.local < ymd_hms('2016-08-03 00:00:00', tz = 'America/New_York'))
# j <- group_by(detects, station) %>% summarize(test = length(unique(transmitter)))
