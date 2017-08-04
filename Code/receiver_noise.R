library(lubridate); library(dplyr)

log <- readRDS('data/bsb_events.rds')
log <- log %>%
  filter(grepl('Tilt|Average [depth|angle|temperature|noise]', Description),
              Date.Time >= '2016-06-13',
              Date.Time < '2016-12-31') %>%
  mutate(Date.Time = ceiling_date(Date.Time, unit = 'day'),
         Data = as.numeric(Data))


log <- log %>%
  group_by(Date.Time, Array, Description) %>%
  summarize(value = mean(Data)) %>%
  ungroup() %>%
  mutate(array = ordered(Array, levels = c('Northern', 'Middle', 'Southern')))

# library(TelemetryR)
# detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
# detects <- filter(detects,
#                   date.local >= '2016-06-13',
#                   date.local < '2016-12-31',
#                   transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)))

## ten_ceiling <- function(x){
##   as.POSIXct(ceiling(as.numeric(x)/(10*60))*(10*60),
##            origin='1970-01-01', tz = 'UTC')}

## detects$agg.date <- ten_ceiling(detects$date.utc)
# detects$agg.date <- ceiling_date(detects$date.local, unit = 'hour')
# names(detects)[2] <- 'Receiver'

# j <- distinct(detects, Receiver, transmitter, agg.date) %>%
#   group_by(Receiver,agg.date)
# k <- summarize(j, n=n())

library(ggplot2)
plot.dat <- filter(log,
                   Description == 'Average temperature' |
                   Description == 'Average noise' & value > 50 |
                   Description == 'Tilt angle' & value < 75)
plot.dat$Description <- ordered(plot.dat$Description,
                               levels = c('Average temperature',
                                          'Average noise', 'Tilt angle'))
data_lab <- c(
  'Average temperature' = 'Temperature (°C)',
  'Average noise' = 'Ambient Noise (mV)',
  'Tilt angle' = 'Tilt Angle (°)'
)

windows(11,7)
ggplot() + geom_point(data = plot.dat,
                       aes(x = Date.Time, y = value)) +
  facet_grid(Description ~ array, scales = 'free_y',
             labeller = labeller(Description = data_lab)) +
  labs(x = 'Date', y = 'Value') +
  scale_x_datetime(date_breaks = '1 month',
                   date_labels = '%b') +
  # Hourly "zoomed" plot
  # scale_x_datetime(limits = c(ymd_hms('2016-09-01 00:00:00'),
  #                             ymd_hms('2016-09-07 23:59:59')),
  #                  breaks = as.POSIXct(c('2016-09-01', '2016-09-03',
  #                                        '2016-09-05', '2016-09-07'),
  #                                      tz = 'UTC'),
  #                  date_labels = '%m/%d') +
  theme_bw()
savePlot('Noise_Tilt_Temp', 'bmp')

ggplot() + geom_smooth(data = k,
                       aes(x = agg.date, y = n)) +
  facet_wrap(~ Receiver, dir = 'v')


test <- left_join(temp.dat, k, by = c("Receiver", "Date.Time" = "agg.date"))
ggplot() + geom_point(data = filter(test, Data > 50), aes(Data, n)) +
  labs(x = 'Noise', y = 'Detections')



