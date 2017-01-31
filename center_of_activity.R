library(TelemetryR); library(lubridate); library(dplyr)

detects <- vemsort('p:/obrien/biotelemetry/ocmd-bsb/receiver logs')
detects <- filter(detects, transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1))) %>%
  mutate(agg.date = ceiling_date(date.local, unit = 'hour'),
         array = ifelse(grepl('Outer', station), 'Outer',
                        ifelse(grepl('Middle', station), 'Middle',
                               'Inner')))

coa.data <- group_by(detects, transmitter, station, lat, long, agg.date, array) %>%
  summarize(n = n()) %>%
  group_by(array, transmitter, agg.date) %>%
  summarize(coa.lat = sum(n*lat)/sum(n),
            coa.long = sum(n*long)/sum(n)) %>%
  arrange(transmitter, agg.date)
# save(coa.data, file = 'data/coa.data.rda')

test <- coa.data[coa.data$transmitter == 'A69-1601-44950',]
j <- filter(detects, grepl('Outer', station)) %>% distinct(long, lat)

library(ggplot2); library(animation)

base <- ggplot() +geom_point(data = j, aes(x = long, y = lat))

saveHTML(
  for (i in 1:2838){
    plot <- base + geom_point(data = test[i,], aes(x = coa.long, y = coa.lat),
                              color = 'red', size = 5)
    print(plot)
    ani.pause()
  },
  interval = 0.05,
ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
ani.height = 720, ani.width = 1280,
other.opts = "-b 300k")
