library(readxl); library(dplyr)
tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb_16.xlsx')

library(ggplot2)
# ggplot() + geom_histogram(data = tagdat, aes(x = `Length\r\n(TL, mm)`))


bsb.detects <- readRDS('data/bsb_detections.rds')

bsb.det <- bsb.detects %>%
  filter(grepl('Inn|Out|Mid', station),
         transmitter %in% paste0('A69-1601-', seq(44950, 44994, 1)),
         date.local <= '2016-12-31') %>%
  mutate(array = case_when(grepl('Outer', station) ~ 'Outer',
                           grepl('Middle', station) ~ 'Middle',
                           T ~ 'Inner'))


library(TelemetryR)
surv.arr <- split(bsb.det, bsb.det$array)
surv.arr <- lapply(surv.arr, trans_loss, 'date.local', 'transmitter',
            enddate = lubridate::ymd('2016-10-31', tz = 'America/New_York'))
for(i in 1:length(surv.arr)){
  surv.arr[[i]]$array <- names(surv.arr)[i]
}

surv.arr <- do.call(rbind, surv.arr)
row.names(surv.arr) <- NULL
surv.arr$pct <- surv.arr$remaining/15

surv.all <- trans_loss(bsb.det, 'date.local', 'transmitter',
                       enddate = lubridate::ymd('2016-10-31', tz = 'America/New_York'))
surv.all$array <- 'All'
surv.all$pct <- surv.all$remaining/45

surv.all <- rbind(surv.all, surv.arr)
surv.all$array <- ordered(surv.all$array,
                          levels = c('All', 'Northern', 'Middle', 'Southern'))

ggplot() + geom_line(data = surv.arr,
                     aes(x = date, y = remaining, col = array),
                     lwd = 1) +
  # scale_color_manual(values = c('pink', 'green', 'lightblue')) +
  geom_vline(xintercept = as.numeric(as.Date('2016-09-02')),
             linetype = 5) +
  geom_vline(xintercept = as.numeric(as.Date('2016-09-06')),
             linetype = 5) +
  labs(x = 'Date', y = 'Fish remaining in array', color = 'Site') +
  lims(y = c(0, 16)) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b', minor_breaks = NULL) +
  theme_bw() +
  theme(legend.position = c(0.9,0.85))


surv.all$doy <- yday(surv.all$date)
model_results <- sapply(levels(surv.all$array),
       function(x) summary(lm(remaining ~ doy,
                      data = distinct(surv.all, array, remaining, .keep_all = T),
                      subset = (array == x))),
       simplify = F, USE.NAMES = T)

anova(lm(remaining ~ doy + array,
         data =  distinct(surv.all, array, remaining, .keep_all = T),
         subset = (array != 'All')))



bsb.det <- bsb.detects %>%
  filter(grepl('Inn|Out|Mid', station),
         transmitter %in% paste0('A69-1602-', seq(1411, 1448, 1))) %>%
  mutate(array = case_when(grepl('Outer', station) ~ 'Outer',
                           grepl('Middle', station) ~ 'Middle',
                           T ~ 'Inner'))


library(TelemetryR)
surv.arr <- split(bsb.det, bsb.det$array)
surv.arr <- lapply(surv.arr, trans_loss, 'date.local', 'transmitter',
                   enddate = lubridate::ymd('2017-10-23', tz = 'America/New_York'))
for(i in 1:length(surv.arr)){
  surv.arr[[i]]$array <- names(surv.arr)[i]
}

surv.arr <- do.call(rbind, surv.arr)
row.names(surv.arr) <- NULL
surv.arr$pct <- surv.arr$remaining/15

surv.all <- trans_loss(bsb.det, 'date.local', 'transmitter',
                       enddate = lubridate::ymd('2017-10-23', tz = 'America/New_York'))
surv.all$array <- 'All'
surv.all$pct <- surv.all$remaining/45

surv.all <- rbind(surv.all, surv.arr)
surv.all$array <- ordered(surv.all$array,
                          levels = c('All', 'Northern', 'Middle', 'Southern'))

ggplot() + geom_line(data = surv.arr,
                     aes(x = date, y = remaining, col = array),
                     lwd = 1) +
  # scale_color_manual(values = c('red', 'blue', 'purple')) +
  labs(x = 'Date', y = 'Fish remaining in array', color = 'Site') +
  lims(y = c(0, 16)) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b', minor_breaks = NULL) +
  theme_bw() +
  theme(legend.position = c(0.9,0.85))
