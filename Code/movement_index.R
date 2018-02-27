library(TelemetryR); library(lubridate); library(dplyr)

## Data munging ----
bsb.detects <- readRDS('data/bsb_detections.rds')

dets <- bsb.detects %>%
  filter(date.utc <= '2016-12-31') %>%
  mutate(hr_floor = floor_date(date.utc, unit = 'hour'))

dets <- split(dets, factor(dets$transmitter))
dets <- lapply(dets, function(x){split(x, x$hr_floor)})

dets <- lapply(dets, function(x){lapply(x, track,
                                          dates = 'date.utc',
                                          ids = c('station'))})

dets <- lapply(dets, function(x){lapply(x, dim)})
dets <- lapply(dets, function(x){lapply(x, '[', 1)})

dets <- lapply(dets, function(x){as.data.frame(do.call(rbind, x))})
dets <- do.call(rbind, dets)

dets$m.index <- dets$V1 - 1

dets$transmitter <- as.character(lapply(
  strsplit(row.names(dets), '[.]'),
  '[', 1))

dets$date <- ymd_hms(lapply(
  strsplit(row.names(dets), '[.]'),
  '[', 2))

row.names(dets) <- NULL
dets <- dets[, c('transmitter', 'date', 'm.index')]

library(ggplot2)

# ggplot() + geom_point(data = dets, aes(x = date, y = m.index)) +
#   facet_wrap(~transmitter)



max.date <- bsb.detects %>%
  filter(date.utc <= '2016-12-31') %>%
  group_by(transmitter) %>%
  summarize(max = max(date.utc))

max.date$hermine.exit <- ifelse(max.date$max < '2016-09-03', 'Removed',
                           ifelse(max.date$max > '2016-09-03' &
                                    max.date$max < '2016-09-04 12:00:00',
                                  'Evacuated',
                                  'Remained'))
max.date$hermine.exit <- ordered(max.date$hermine.exit,
                                 levels = c('Removed', 'Evacuated', 'Remained'))

hermine_dets <- left_join(dets, max.date) %>%
  mutate(h.event = ifelse(date < '2016-09-04 12:00:00', 'Before', 'After'),
         h.event = ordered(h.event, levels = c('Before', 'After'))) %>%
  group_by(transmitter, h.event, hermine.exit) %>%
  summarize(avg.move = mean(m.index))

# library(readxl)
# tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb_16.xlsx')

# hermine_dets <- left_join(hermine_dets,
                          # tagdat[, c('Transmitter', 'Release Array')],
                          # by = c('transmitter' = 'Transmitter'))

ggplot() +
  geom_violin(data = hermine_dets, aes(x = h.event, y = avg.move,
                                       fill = hermine.exit),
                       draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_dotplot(data = hermine_dets, aes(x = h.event, y = avg.move,
                                        col = hermine.exit),
             binaxis = 'y', stackdir = 'center', position = 'dodge',
             binwidth = 0.25) +
  theme(legend.position = c(0.9, 0.75))

# Contrasts ----
fate.cons <- lm(log1p(avg.move) ~ hermine.exit +
                 `Release Array`, subset = (h.event == 'Pre'),
                data = hermine_dets)
summary(fate.cons)
anova(fate.cons)

move.cons <- lm(avg.move ~ h.event +
                  `Release Array` , subset = (hermine.exit == 'Remained'),
                data = hermine_dets)
summary(move.cons)
anova(move.cons)

# Manuscript box plots ----
pre_post <- ggplot() +
  geom_boxplot(data = hermine_dets, aes(x = h.event, y = avg.move)) +
  labs(x = NULL, y = 'Activity Index') +
  lims(y = c(0, 16.19)) +
  theme_bw() +
  theme(axis.text = element_text(color = 'black'), panel.grid = element_blank())

go_stay <- ggplot() +
  geom_boxplot(data = filter(hermine_dets,
                             grepl('Evac|Remain', hermine.exit),
                             h.event == 'Before'),
               aes(x = hermine.exit, y = avg.move)) +
  labs(x = NULL, y = NULL) +
  lims(y = c(0, 16.19)) +
  theme_bw()+
  theme(axis.text = element_text(color = 'black'), panel.grid = element_blank())

library(gridExtra)
man.plot <- grid.arrange(pre_post, go_stay, nrow = 1)
ggsave('ActivityIndex_boxplot.eps', man.plot, device = 'eps',
       width = 85, height = 56.61, units = 'mm')


# Check movement v TL/Weight ----
tl.wt.dat <- tagdat %>%
  filter(!is.na(Transmitter)) %>%
  left_join(hermine_dets,
                  by = c('Transmitter' = 'transmitter',
                         'Release Array' = 'Release Array')) %>%
  select(Transmitter, `Length\r\n(TL, mm)`, `Weight (kg)`, h.event,
         hermine.exit, avg.move)


summary(lm(avg.move ~ `Length\r\n(TL, mm)`,
           subset = (h.event == 'Pre'),
           data = tl.wt.dat))

summary(lm(avg.move ~ `Length\r\n(TL, mm)`,
           subset = (h.event == 'Post'),
           data = tl.wt.dat))

summary(lm(avg.move ~ `Weight (kg)`,
           subset = (h.event == 'Pre'),
           data = tl.wt.dat))

summary(lm(avg.move ~ `Weight (kg)`,
           subset = (h.event == 'Post'),
           data = tl.wt.dat))

