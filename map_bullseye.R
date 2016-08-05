#### BSB DNR/BOEM ---------------------------------
met <- c(-74.753546, 38.352747)

rec_loc <- data.frame(lat = c(38.15564, 38.14732, 38.14488,
                              38.43768, 38.42872, 38.42619,
                              38.23071, 38.22120, 38.21864),
                      long = c(-74.94863, -74.93908, -74.95380,
                               -74.77002, -74.75971, -74.77468,
                               -74.75808, -74.74715, -74.76294),
                      depth = c(71.2, 70, 71,
                                86, 92, 87,
                                76, 105, 89),
                      site = rep(c('S1', 'S2', 'S3'), each = 3))

rel_loc <- data.frame(lat = c(38.148047, 38.430944, 38.223683),
                      long = c(-74.947197, -74.768028, -74.756183))

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
  geom_point(data = rec_loc, aes(x = long, y = lat, color = site)) +
  geom_point(data = rel_loc, aes(x = long, y = lat), color = 'black') +
  theme_bw() +
  scale_color_discrete(breaks = c('S1', 'S3', 'S2'),
                       labels = c('Outer', 'Middle', 'Inner')) +
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
