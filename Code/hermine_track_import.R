# Data download from a NOAA server
# Hermine was storm 9 in 2016: "AL092016"

temp <- tempfile()
download.file('ftp://ftp.nhc.noaa.gov/atcf/archive/2016/bal092016.dat.gz', temp)
hermine_track <- read.csv(temp,
                          header = F, stringsAsFactors = F, strip.white = T)
unlink(temp)

# Metadata is here:
# https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt
names(hermine_track) <- c('BASIN', 'CY', 'YYYYMMDDHH', 'MIN', 'TECH',
                          'TAU', 'Lat', 'Lon', 'VMAX', 'MSLP', 'TY', 'RAD',
                          'WINDCODE', 'RAD1', 'RAD2', 'RAD3', 'RAD4', 'POUTER',
                          'ROUTER', 'RMW', 'GUSTS', 'EYE', 'SUBREGION',
                          'MAXSEAS', 'INITIALS', 'DIR', 'SPEED', 'STORMNAME',
                          'DEPTH', 'SEAS', 'SEASCODE', 'SEAS1', 'SEAS2', 'SEAS3',
                          'SEAS4', 'USERDEFINED', 'userdata1', 'userdata2',
                          'userdata3', 'userdata4')

# Adjust time
library(lubridate)
hermine_track[is.na(hermine_track$MIN), 'MIN'] <- 00
hermine_track$date <- ymd_hm(paste0(hermine_track$YYYYMMDDHH, hermine_track$MIN))

# Adjust Lat/Lon
latlongconvert <- function(x){
  temp <- paste(substr(x, 1, 2),
                substr(x, 3, 3), sep = '.')
  as.numeric(temp)
}

hermine_track$Lat <- latlongconvert(hermine_track$Lat)
hermine_track$Lon <- -latlongconvert(hermine_track$Lon)

# Drop columns that contain little/no data
hermine_track <- hermine_track[, !names(hermine_track) %in%
                                 c('BASIN', 'CY', 'YYYYMMDDHH', 'MIN', 'TECH',
                                   'TAU', 'EYE', 'MAXSEAS', 'INITIALS', 'DIR',
                                   'SPEED', 'USERDEFINED', 'userdata1',
                                   'userdata2', 'userdata3', 'userdata4')]

hermine_track <- hermine_track[, c(25, 1 : 24)]

write.csv(hermine_track, 'data/hermine_track.csv', row.names = F)
