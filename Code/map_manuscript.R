library(dplyr)
met <- c(-74.753546, 38.352747)
sites <- read.csv('p:/obrien/biotelemetry/ocmd-bsb/log4map.csv')
sites <- sites %>%
  mutate(array = ifelse(Site.ID == 'Outer', 'Southern',
                    ifelse(Site.ID == 'Inner', 'Northern', 'Middle')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))

library(rgdal)
midatl <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')

library(ggplot2)
midatl <- fortify(midatl)

ggplot() +
  geom_polygon(data = midatl, aes(long, lat, group = group)) +
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
  theme(legend.position = c(0.06, 0.9)) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Site')
