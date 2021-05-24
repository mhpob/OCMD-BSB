# Load packages
library(data.table)

# MDNR receiver events ----
events <- fread('p:/obrien/biotelemetry/ocmd-bsb/data/vue_export_2019.csv',
                col.names = function(x) gsub('[ ()]', '.', tolower(x)))

events <- events[description == 'Average temperature']
events <- events[, `:=`(date.utc = lubridate::ymd_hms(date.and.time..utc.),
                        data = as.numeric(data))]
events <- events[, date.edt := lubridate::with_tz(date.utc, 'America/New_York')]
events <- events[date.edt >= '2019-06-07 15:00' &
                   date.edt <= '2019-10-29 09:00']

events <- events[, date := as.Date(lubridate::floor_date(date.edt, 'day'))]
events <- events[, .(bwt = mean(data)), by = c('receiver', 'date')]


# MDNR receiver metadata ----
receiver_metadata <- data.table(
  readxl::read_xlsx('e:/phase 1b/vps/vps recovery.xlsx')
)
names(receiver_metadata) <- gsub('[ ()]', '.', tolower(names(receiver_metadata)))

receiver_metadata <- receiver_metadata[grepl('Inner|Middle', site.id),]
receiver_metadata <- receiver_metadata[, `:=`(receiver = paste0('VR2AR-', vr2ar.id),
                                              site = gsub(' ', '', site.id))]
receiver_metadata <- receiver_metadata[, c('site', 'receiver', 'long_dd', 'lat_dd')]

events <- receiver_metadata[events, on = 'receiver']

sst <- data.table(readRDS('data and imports/sst/sst.rds'))

events <- sst[events, on = c('site','date')]
events <- events[, `:=`(dt = sst - bwt,
                        array = substr(site, 1, 3))]

library(ggplot2)
ggplot(data = events, aes(x = date, y = dt, color = array, group = site)) +
  geom_line(lwd = 1) +
  geom_vline(xintercept = as.Date(c('2019-08-25', '2019-09-07',
                                    '2019-09-18', '2019-10-10'))) +
  labs(x = NULL, y = 'ΔT (°C)', color = 'Array',
       title = 'Difference between surface and bottom water temperatures',
       subtitle = 'Each line represents one station in the MDNR arrays') +
  theme_bw()
