# Jun16_Nov16 <- read.csv(
#   'C:/Users/secor/Desktop/OCMD_BSB Logs/BSB_201606_201611_VUE_Export.csv',
#   stringsAsFactors = F)
# Nov16_May17 <- read.csv(
#   'C:/Users/secor/Desktop/OCMD_BSB Logs/BSB_201611_201705_VUE_Export.csv',
#   stringsAsFactors = F)
#
# bsb_events <- rbind(Jun16_Nov16, Nov16_May17)

bsb_events <- read.csv(
  'c:/users/secor/desktop/OCMD_BSB_events_corrected.csv',
  stringsAsFactors = F)

bsb_events$Date.Time <- lubridate::ymd_hms(bsb_events$Date.Time)
bsb_events$Date.Time.Local <- lubridate::with_tz(bsb_events$Date.Time,
                                                 tzone = 'America/New_York')

label <- function(x){
  switch(x,
         'VR2AR-546301' = 'Inner SW',
         'VR2AR-546302' = 'Outer SW',
         'VR2AR-546303' = 'Middle N',
         'VR2AR-546304' = 'Middle SW',
         'VR2AR-546305' = 'Outer N',
         'VR2AR-546306' = 'Outer SE',
         'VR2AR-546307' = 'Middle SE',
         'VR2AR-546308' = 'Inner N',
         'VR2AR-546309' = 'Inner SE')
}

bsb_events$Site <- sapply(bsb_events$Receiver, label)

bsb_events$Array <- ifelse(grepl('In', bsb_events$Site), 'Northern',
                        ifelse(grepl('Out', bsb_events$Site), 'Southern',
                        'Middle'))
bsb_events$Array <- ordered(bsb_events$Array,
                            levels = c('Northern', 'Middle', 'Southern'))

# Remove false Southern data caused by deployment/recovery
bsb_events <- bsb_events[!(bsb_events$Array == 'Southern' &
                    (bsb_events$Date.Time.Local < '2016-06-09 11:05:00' |
                       (bsb_events$Date.Time.Local >= '2017-05-04 08:30:00' &
                        bsb_events$Date.Time.Local <= '2017-06-29 10:00:00') |
                      bsb_events$Date.Time.Local >= '2017-10-26 10:40:00')),]
# Remove false Middle data caused by deployment/recovery
bsb_events <- bsb_events[!(bsb_events$Array == 'Middle' &
                    (bsb_events$Date.Time.Local < '2016-06-12 12:05:00' |
                       bsb_events$Date.Time.Local >= '2017-10-26 09:30:00')),]
# Remove false Northern data caused by deployment/recovery
bsb_events <- bsb_events[!(bsb_events$Array == 'Northern' &
                    (bsb_events$Date.Time.Local < '2016-06-10 10:05:00' |
                       bsb_events$Date.Time.Local >= '2017-10-26 07:30:00')),]


bsb_events <- bsb_events[, c('Date.Time', 'Date.Time.Local', 'Receiver',
                             'Description', 'Data', 'Units', 'Site', 'Array')]


saveRDS(bsb_events, file = 'data/bsb_events.rds')
