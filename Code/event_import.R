Jun16_Nov16 <- read.csv(
  'C:/Users/secor/Desktop/OCMD_BSB Logs/BSB_201606_201611_VUE_Export.csv',
  stringsAsFactors = F)
Nov16_May17 <- read.csv(
  'C:/Users/secor/Desktop/OCMD_BSB Logs/BSB_201611_201705_VUE_Export.csv',
  stringsAsFactors = F)

bsb_events <- rbind(Jun16_Nov16, Nov16_May17)

bsb_events$Date.Time <- lubridate::ymd_hms(bsb_events$Date.Time)

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

bsb_events$site <- sapply(bsb_events$Receiver, label)

bsb_events$array <- ifelse(grepl('In', bsb_events$site), 'Northern',
                        ifelse(grepl('Out', bsb_events$site), 'Southern',
                        'Middle'))
bsb_events$array <- ordered(bsb_events$array,
                            levels = c('Northern', 'Middle', 'Southern'))

names(bsb_events) <- c('Date.Time', 'Receiver', 'Description', 'Data', 'Units',
                       'Site', 'Array')


saveRDS(bsb_events, file = 'data/bsb_events.rds')
