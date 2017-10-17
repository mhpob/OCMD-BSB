library(TelemetryR); library(lubridate); library(dplyr)

detects <- readRDS('data/bsb_detections.rds')
detects <- filter(detects,
                  transmitter %in%
                    paste0('A69-1601-', seq(44950, 44994, 1)),
                  date.local <= '2016-12-31',
                  date.local >= '2016-06-12') %>%
  mutate(agg.date = floor_date(date.local, unit = 'day'),
         array = ifelse(grepl('Outer', station), 'Southern',
                        ifelse(grepl('Middle', station), 'Middle',
                               'Northern')),
         array = ordered(array, levels = c('Northern', 'Middle', 'Southern')))

date.fill <- data.frame(agg.date = rep(seq(as.Date('2016-06-12'),
                                           as.Date('2016-12-31'),
                                           by = 'day'),
                                       each = 3),
                        array = ordered(rep(c('Northern', 'Middle', 'Southern'),
                                            times = 203),
                                        levels = c('Northern', 'Middle', 'Southern')),
                        stringsAsFactors = F)
date.fill$agg.date <- ymd(date.fill$agg.date, tz = 'America/New_York')

detect.ts <- detects %>%
  distinct(transmitter, array, agg.date, .keep_all =T) %>%
  group_by(array, agg.date) %>%
  summarize(n = n()) %>%
  full_join(date.fill) %>%
  mutate(n = ifelse(is.na(n), 0, n))




library(tsoutliers)
outliers2 <- sapply(levels(detect.ts$array),
       function(z){
         temporary <- filter(detect.ts, array == z) %>%
           arrange(agg.date)
         temporary <- ts(temporary[, 'n'], start = c(2016, 164), frequency = 365)
         tso(temporary, types = c('LS', 'TC'), tsmethod = 'auto.arima',
             cval = 5)
       },
       simplify = F, USE.NAMES = T)

plot(outliers$Northern,
     args.lines.y = list(col = gray.colors(6)[2]),
     args.effects.axis = list(xaxt = 'n'))
lmtest::coeftest(outliers$Northern$fit)


# $y is original time series, $yadj is fitted time series, $effects are effects
plot(cbind(outliers$Northern$y, outliers$Northern$yadj) ~
       seq(ymd('2016-06-12'), ymd('2016-12-31'), by = 'day'), plot.type = 'single',
     type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis.Date(1, at = seq(ymd('2016-06-01'), ymd('2017-01-01'), by = 'month'),
          format = '%b')

# Adjust plot.tsoutliers function to remove titles
p.tso <- function (x, args.lines.y = list(col = "gray80"),
                   args.lines.yadj = list(col = "blue"),
                   args.lines.effects = list(type = "s", col = "red"),
                   args.points = list(col = "gray80", bg = "red", pch = 21),
                   plot.points = TRUE,
                   args.x.axis = list(at = pretty(time(x$y)),
                                      tcl = -0.5, lwd = 0, lwd.ticks = 1),
                   args.y.axis = list(at = pretty(x$y), tcl = -0.5, lwd = 0,
                                      lwd.ticks = 1),
                   args.effects.axis = list(at = pretty(x$effects), tcl = -0.5,
                                            lwd = 0, lwd.ticks = 1), ...){
  fargs.linesy <- formals(plot.tsoutliers)$args.lines.y
  efargs.linesy <- eval(fargs.linesy)
  if (!identical(args.lines.y, efargs.linesy)) {
    args.lines.y <- c(args.lines.y, efargs.linesy)
    id <- which(duplicated(names(args.lines.y)))
    if (length(id) > 0)
      args.lines.y <- args.lines.y[-id]
  }
  fargs.linesyadj <- formals(plot.tsoutliers)$args.lines.yadj
  efargs.linesyadj <- eval(fargs.linesyadj)
  if (!identical(args.lines.yadj, efargs.linesyadj)) {
    args.lines.yadj <- c(args.lines.yadj, efargs.linesyadj)
    id <- which(duplicated(names(args.lines.yadj)))
    if (length(id) > 0)
      args.lines.yadj <- args.lines.yadj[-id]
  }
  fargs.linesef <- formals(plot.tsoutliers)$args.lines.effects
  efargs.linesef <- eval(fargs.linesef)
  if (!identical(args.lines.effects, efargs.linesef)) {
    args.lines.effects <- c(args.lines.effects, efargs.linesef)
    id <- which(duplicated(names(args.lines.effects)))
    if (length(id) > 0)
      args.lines.effects <- args.lines.effects[-id]
  }
  fargs.points <- formals(plot.tsoutliers)$args.points
  efargs.points <- eval(fargs.points)
  if (!identical(args.points, efargs.points)) {
    args.points <- c(args.points, efargs.points)
    id <- which(duplicated(names(args.points)))
    if (length(id) > 0)
      args.points <- args.points[-id]
  }
  fargs.xaxis <- formals(plot.tsoutliers)$args.x.axis
  efargs.xaxis <- eval(fargs.xaxis)
  if (!identical(args.x.axis, efargs.xaxis)) {
    args.x.axis <- c(args.x.axis, efargs.xaxis)
    id <- which(duplicated(names(args.x.axis)))
    if (length(id) > 0)
      args.x.axis <- args.x.axis[-id]
  }
  if (is.null(args.x.axis$labels))
    args.x.axis$labels <- args.x.axis$at
  args.x.axis$side <- 1
  fargs.yaxis <- formals(plot.tsoutliers)$args.y.axis
  efargs.yaxis <- eval(fargs.yaxis)
  if (!identical(args.y.axis, efargs.yaxis)) {
    args.y.axis <- c(args.y.axis, efargs.yaxis)
    id <- which(duplicated(names(args.y.axis)))
    if (length(id) > 0)
      args.y.axis <- args.y.axis[-id]
  }
  if (is.null(args.y.axis$labels))
    args.y.axis$labels <- args.y.axis$at
  args.y.axis$side <- 2
  fargs.eaxis <- formals(plot.tsoutliers)$args.effects.axis
  efargs.eaxis <- eval(fargs.eaxis)
  if (!identical(args.effects.axis, efargs.eaxis)) {
    args.effects.axis <- c(args.effects.axis, efargs.eaxis)
    id <- which(duplicated(names(args.effects.axis)))
    if (length(id) > 0)
      args.effects.axis <- args.effects.axis[-id]
  }
  if (is.null(args.effects.axis$labels))
    args.effects.axis$labels <- args.effects.axis$at
  if (is.null(args.effects.axis$side))
    args.effects.axis$side <- 4
  if (nrow(x$outliers) == 0) {
    cat(paste(sQuote("x"), "does not contain outliers to display\\n"))
    return()
  }
  oldpar <- par(mar = c(0, 3, 0, 2.1), oma = c(3, 0, 3, 0),
                mfcol = c(2, 1), ...)
  on.exit(par(oldpar))
  plot(cbind(x$y, x$yadj), plot.type = "single", type = "n",
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  do.call("lines", args = c(list(x = x$y), args.lines.y))
  do.call("lines", args = c(list(x = x$yadj), args.lines.yadj))
  do.call("axis", args = args.y.axis)
  if (plot.points) {
    do.call("points", args = c(list(x = x$times, y = x$y[x$outliers[, "ind"]]),
                               args.points))
  }
  plot(outliers$Northern$effects,
       type = "n", xaxt = "n", yaxt = "n", xlab = "",
       ylab = "", bty = "u")
  do.call("lines", args = c(list(x = x$effects), args.lines.effects))
  do.call("axis", args = args.effects.axis)
  # do.call("axis.Date", args = list(1, labels = seq(ymd('2016-06-01'), ymd('2017-01-01'), by = 'month'),
  #                                  format = '%b'))
  axis.Date(1, at = seq(ymd('2016-06-01'), ymd('2017-01-01'), by = 'month'),
            format = '%b')
}

p.tso(outliers$Middle)

plot(x = seq(ymd('2016-06-12'), ymd('2016-12-31'), by = 'day'),
     y = outliers$Northern$effects,
     col = 'red', type = 's', xaxt = 'n', xlab = '', ylab = '')
axis.Date(1, at = seq(ymd('2016-06-01'), ymd('2017-01-01'), by = 'month'),
             format = '%b')
