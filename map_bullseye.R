#### BSB DNR/BOEM ---------------------------------
library(dplyr)
met <- c(-74.753546, 38.352747)

sites <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/log4map.csv')

library(rgdal)
blocks <- readOGR('c:/users/secor/desktop/gis products/md mammals/wind_planning_areas',
                  'Wind_Planning_Areas_06_20_2014')
midatl <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')

# circ_500m <- TelemetryR::ptcirc(met, 500)
# circ_1k <- TelemetryR::ptcirc(met, 1000)
# circ_2k <- TelemetryR::ptcirc(met, 2000)
# circ_4k <- TelemetryR::ptcirc(met, 4000)

circ_5k <- TelemetryR::ptcirc(met, 5000)
circ_10k <- TelemetryR::ptcirc(met, 10000)
circ_20k <- TelemetryR::ptcirc(met, 20000)

library(ggplot2)
blocks <- fortify(blocks)
midatl <- fortify(midatl)

ggplot() +
  geom_path(data = circ_20k, aes(x = long, y = lat), color= 'green', lwd = 1.5) +
  geom_path(data = circ_10k, aes(x = long, y = lat), color= 'orange', lwd = 1.5) +
  geom_path(data = circ_5k, aes(x = long, y = lat), color = 'red', lwd = 1.5) +
  geom_path(data = blocks, aes(long, lat, group = group)) +
  geom_polygon(data = midatl, aes(long, lat, group = group)) +
  coord_map(xlim = c(-75.22, -74.45), ylim = c(38.12, 38.56)) +
  geom_point(aes(x = met[1], y = met[2]), pch = 8) +
  geom_point(data = filter(sites, Type == 'Receiver'),
             aes(x = Long, y = Lat, color = Site.ID, shape = Trip)) +
  geom_point(data = filter(sites, Type == 'Tag'), aes(x = Long, y = Lat),
             color = 'black') +
  geom_point(data = filter(sites, Type == 'Fishing'),
             aes(x = Long, y = Lat, shape = Trip)) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', color = '')

# Export to Google Earth
TelemetryR::GEcircle(met[2], met[1], 500, 'red', F)
TelemetryR::GEcircle(met[2], met[1], 1000, 'red', F)
TelemetryR::GEcircle(met[2], met[1], 2000, 'orange', F)
TelemetryR::GEcircle(met[2], met[1], 4000, 'orange', F)
TelemetryR::GEcircle(met[2], met[1], 20000, 'green', F)

## Squares
sqr <- function(x){
  data.frame(long.min = min(x$long), long.max = max(x$long),
             lat.min = min(x$lat), lat.max = max(x$lat),
             name = unique(x$circle))
}
squares <- rbind(sqr(circ_500m), sqr(circ_1k), sqr(circ_2k),
              sqr(circ_4k), sqr(circ_20k))
squares <- cbind(squares, band= c('A', 'A', 'B', 'B', 'C'))

ggplot() +
  geom_rect(data = squares, aes(xmin = long.min, xmax = long.max,
                             ymin = lat.min, ymax = lat.max, color = band),
                             fill = NA, lwd = 1.5) +
  geom_path(data = blocks, aes(long, lat, group = group)) +
  geom_polygon(data = midatl, aes(long, lat, group = group)) +
  coord_map(xlim = c(-75.22, -74.45), ylim = c(38.15, 38.56)) +
  geom_point(aes(x = met[1], y = met[2])) +
  scale_color_manual(values = c('red', 'orange', 'green')) +
  theme_bw() +
  theme(legend.position = 'none')+
  labs(x = 'Longitude', y = 'Latitude')

temp <- function(x){
print(rbind(c(test[x,1], test[x,3],0),
            c(test[x,1], test[x,4],0),
            c(test[x,2], test[x,4],0),
            c(test[x,2], test[x,3],0),
            c(test[x,1], test[x,3],0)), digits = 15)
}
temp(5)
