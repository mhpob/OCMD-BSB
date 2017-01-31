library(ks); library(dplyr)
load('data/coa.data.rda')

# Calculate Kkernel density estimate (KDE)
coa.list <- split(as.data.frame(coa.data[, c('coa.long','coa.lat')]),
                  as.factor(coa.data$transmitter))

coa.bandw <- lapply(X = coa.list, FUN = Hpi)

coa.kde <- lapply(X = names(coa.list),
                  FUN = function(i){kde(coa.list = coa.list[[i]],
                                        H = coa.bandw[[i]])})

# Plotting
kde.plot <- contourLines(x = coa.kde[[1]]$eval.points[[1]],
                         y = coa.kde[[1]]$eval.points[[2]],
                         z = coa.kde[[1]]$estimate,
                         levels = coa.kde[[1]]$cont['2%'])
names(kde.plot) <- seq(1, length(kde.plot), 1)
kde.plot <- lapply(kde.plot, data.frame)
kde.plot <- do.call(rbind, kde.plot)
kde.plot$contour <- gsub("[.].*", "", row.names(kde.plot))


library(ggplot2)
ggplot() +
  # geom_point(data=coa.list[[1]], aes(coa.long, coa.lat))+
  geom_path(data=kde.plot, aes(x, y, group = contour))
