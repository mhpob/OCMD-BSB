library(dplyr)
met <- c(-74.753546, 38.352747)
sites <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/log4map.csv')
sites <- sites %>%
  mutate(array = ifelse(Site.ID == 'Outer', 'Southern',
                    ifelse(Site.ID == 'Inner', 'Northern', 'Middle')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))

library(rgeos); library(rgdal)
close <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')
far <- readOGR('c:/users/secor/desktop/gis products/natural earth/10m coastline',
             'ne_10m_land')
far <- spTransform(far, CRS(proj4string(close)))
plot(far, xlim = c(-80, -72), ylim = c(32, 42),col='red')
mem.crop <- cbind(c(-80, -80, -70, -70, -80), c(33, 42, 42, 33, 33))
mem.crop <- SpatialPolygons(list(Polygons(list(Polygon(mem.crop)),
                                          'Things I want to keep')),
                            proj4string = CRS(proj4string(far)))
mem.crop <- gIntersection(far, mem.crop)
detach('package:rgeos', unload = T); detach('package:rgdal', unload = T)

library(ggplot2)
close.f <- fortify(close)
far.f <- fortify(mem.crop)

# Need to add in MAB inset w/ rect annotation
inset <- ggplot() +
  geom_polygon(data = far.f, aes(long, lat, group = group), fill = NA, color ='black') +
  coord_map(xlim = c(-77, -72), ylim = c(35.25, 41.5)) +
  # geom_point(aes(x = met[1], y = met[2]), pch = 23, bg = 'black') +
  annotate('segment', lwd = 0.5,
           x = -75, xend = -74.5,
           y = 38.402, yend = 38.302) +
  annotate('rect', xmin = -75.22, xmax = -74.45,
           ymin = 38.12, ymax = 38.56, fill = NA, color = 'black') +
  # geom_point(data = filter(sites, Type == 'Receiver'),
  #            aes(x = Long, y = Lat), lwd = 0.1) +
  geom_point(data = filter(sites, Type == 'Tag'), aes(x = Long, y = Lat), lwd = 0.1) +
  theme_void() +
  theme(plot.background = element_rect(color = 'black'))
inset

study <- ggplot() +
  geom_polygon(data = close.f, aes(long, lat, group = group)) +
  coord_map(xlim = c(-75.22, -74.45), ylim = c(38.12, 38.56)) +
  geom_point(aes(x = met[1], y = met[2]), pch = 23, lwd = 5.5, bg = 'black') +
  annotate('segment', lwd = 1.5,
           x = -75, xend = -74.5,
           y = 38.402, yend = 38.302) +
  geom_point(data = filter(sites, Type == 'Receiver'),
             aes(x = Long, y = Lat, color = array), lwd = 3.5) +
  geom_point(data = filter(sites, Type == 'Tag'), aes(x = Long, y = Lat),
             color = 'black', lwd = 3.5) +
  scale_color_manual(values = c('red', 'blue', 'purple')) +
  theme_bw() +
  theme(legend.position = c(0.08, 0.9)) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Site')
study

library(grid)
v1 <- viewport(width = 1, height = 1,
               x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport(width = 0.3, height = 0.3,
               x = 0.9, y = 0.825) #plot area for the inset map
grid.newpage()
print(study, vp = v1)
print(inset, vp = v2)

