library(TelemetryR); library(lubridate); library(dplyr)

## Data munging ----
source('p:/obrien/biotelemetry/ocmd-bsb/code/bsb_detection_import.R')

dets <- bsb.detects %>%
  filter(date.utc < '2016-11-11') %>%
  mutate(hr_floor = floor_date(date.utc, unit = 'hour'))

dets <- split(dets, factor(dets$transmitter))
dets <- lapply(dets, function(x){split(x, x$hr_floor)})

dets <- lapply(dets, function(x){lapply(x, track,
                                          dates = 'date.utc',
                                          ids = c('station'))})

dets <- lapply(dets, function(x){lapply(x, dim)})
dets <- lapply(dets, function(x){lapply(x, '[', 1)})

dets <- lapply(dets, function(x){as.data.frame(do.call(rbind, x))})
dets <- do.call(rbind, dets)

dets$m.index <- dets$V1 - 1

dets$transmitter <- as.character(lapply(
  strsplit(row.names(dets), '[.]'),
  '[', 1))

dets$date <- ymd_hms(lapply(
  strsplit(row.names(dets), '[.]'),
  '[', 2))

row.names(dets) <- NULL
dets <- dets[, c('transmitter', 'date', 'm.index')]

library(ggplot2)

ggplot() + geom_point(data = dets, aes(x = date, y = m.index)) +
  facet_wrap(~transmitter)



max.date <- bsb.detects %>%
  filter(date.utc < '2016-11-11') %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc))

max.date$hermine.exit <- ifelse(max.date$max < '2016-09-03', 'Removed',
                           ifelse(max.date$max > '2016-09-03' &
                                    max.date$max < '2016-09-04 12:00:00',
                                  'Evacuated',
                                  'Remained'))
max.date$hermine.exit <- ordered(max.date$hermine.exit,
                                 levels = c('Removed', 'Evacuated', 'Remained'))

hermine_dets <- left_join(dets, max.date) %>%
  mutate(h.event = ifelse(date < '2016-09-04 12:00:00', 'Pre', 'Post'),
         h.event = ordered(h.event, levels = c('Pre', 'Post'))) %>%
  group_by(transmitter, h.event, hermine.exit) %>%
  summarize(avg.move = mean(m.index))

ggplot() +
  geom_violin(data = hermine_dets, aes(x = h.event, y = avg.move,
                                       fill = hermine.exit),
                       draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_dotplot(data = hermine_dets, aes(x = h.event, y = avg.move,
                                        col = hermine.exit),
             binaxis = 'y', stackdir = 'center', position = 'dodge',
             binwidth = 0.25) +
  theme(legend.position = c(0.9, 0.75))

