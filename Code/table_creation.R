library(readxl); library(dplyr)

tagdat <- read_excel('p:/obrien/biotelemetry/ocmd-bsb/data/taggingdata_ocbsb_16.xlsx')
sumdat <- tagdat %>%
  filter(!is.na(Transmitter)) %>%
  mutate(array = ifelse(`Release Array` == 'Inner', 'Northern',
                        ifelse(`Release Array` == 'Outer', 'Southern', 'Middle'))) %>%
  group_by(array) %>%
  summarize(wt.avg = mean(`Weight (kg)` * 1000),
            wt.sd = sd(`Weight (kg)` * 1000),
            tl.avg = mean(`Length\r\n(TL, mm)`),
            tl.sd = sd(`Length\r\n(TL, mm)`))

sumdat_all <- tagdat %>%
  summarize(wt.avg = mean(`Weight (kg)` * 1000),
            wt.sd = sd(`Weight (kg)` * 1000),
            tl.avg = mean(`Length\r\n(TL, mm)`),
            tl.sd = sd(`Length\r\n(TL, mm)`))
sumdat_comb <- rbind(sumdat, c(array = 'All', sumdat_all))
sumdat_comb <- sumdat_comb %>%
  mutate(wt = paste(sprintf('%.1f', wt.avg), '$\\pm$',
                    sprintf('%.1f', wt.sd), sep = ' '),
         tl = paste(sprintf('%.1f', tl.avg), '$\\pm$',
                    sprintf('%.1f', tl.sd), sep = ' ')) %>%
  select(array, wt, tl)
sumdat_comb <- mutate(sumdat_comb,
                      array = ordered(array,
                              levels = c('Northern', 'Middle',
                                         'Southern', 'All'))) %>%
                         arrange(array)
names(sumdat_comb) <- c('Array', 'Weight (g)',
                        'Total Length (mm)')

library(xtable)
table <- xtable(sumdat_comb, align = rep('r', 4))
print(table,
      include.rownames = F,
      hline.after = c(-1, 0, 3, 4),
      sanitize.text.function = function(x){x})

# Plug output into Sweave doc and compile as PDF.
