library(reshape2); library(lubridate); library(dplyr)

log <- readRDS('data/bsb_events.rds')

slim.log <- filter(log, grepl('Tilt|Average [depth|temperature|noise]',
                              Description)) %>%
  mutate(Data = as.numeric(Data),
         Date.Time = ceiling_date(Date.Time, unit = 'day')) %>%
  filter(Date.Time >= '2016-06-13',
         Date.Time <= '2016-12-31')

slim.log <- dcast(slim.log, Date.Time + Receiver + Array ~ Description,
           fun.aggregate = mean, value.var = 'Data')

slim.log <- filter(slim.log, `Average seawater depth` > 15)
pairs(slim.log[, c(1, 4:7)])

slim.log <- melt(slim.log, id = c('Date.Time', 'Array', 'Receiver'),
                 variable.name = 'Description',
                 value.name = 'Data')

slim.log$hermine <- ifelse(slim.log$Date.Time < '2016-09-03', 'pre', 'post')

summ.log <- group_by(slim.log, Array, Description, hermine) %>%
  summarize(min = min(Data),
            avg = mean(Data),
            std = sd(Data),
            max = max(Data))

aov.log <- group_by(slim.log, Date.Time, Array, Description, hermine) %>%
  summarize(mean = mean(Data)) %>%
  mutate(doy = yday(Date.Time))

wt.aov <- lm(sqrt(mean) ~ doy + Array, subset = (hermine == 'pre' &
                                       Description == 'Average temperature'),
                data = aov.log)

summary(wt.aov)
summary(aov(wt.aov))
j <- TukeyHSD(aov(wt.aov), 'Array')
plot(j)
j
