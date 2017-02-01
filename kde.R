library(ks)
load('data/coa.data.rda')

# Calculate Kkernel density estimate (KDE)
coa.list <- split(as.data.frame(coa.data[, c('coa.long','coa.lat')]),
                  as.factor(coa.data$transmitter))

coa.bandw <- lapply(X = coa.list, FUN = Hpi)

coa.kde <- lapply(X = names(coa.list),
                  FUN = function(i){kde(x = coa.list[[i]],
                                        H = coa.bandw[[i]])})
names(coa.kde) <- names(coa.list)

# Plotting
# Create a list (transmitters) of lists (Contour groups)
temporary.contour.function <- function(i){
  contourLines(x = coa.kde[[i]]$eval.points[[1]],
               y = coa.kde[[i]]$eval.points[[2]],
               z = coa.kde[[i]]$estimate,
               levels = coa.kde[[i]]$cont['5%'])
  }

kde.plot <- lapply(names(coa.kde), temporary.contour.function)

names(kde.plot) <- names(coa.list)

for(i in seq(1, length(kde.plot), 1)){
  names(kde.plot[[i]]) <- seq(1, length(kde.plot[[i]]), 1)
  kde.plot[[i]] <- lapply(kde.plot[[i]], data.frame)
}

kde.plot <- lapply(names(kde.plot), function(i){do.call(rbind, kde.plot[[i]])})
names(kde.plot) <- names(coa.list)
kde.plot <- do.call(rbind, kde.plot)

kde.plot$transmitter <- gsub("[.].*", "", row.names(kde.plot))
kde.plot$contour <- unlist(lapply(strsplit(row.names(kde.plot), "[.]"),
                                      `[[`, 2))
kde.plot$contour <- paste(kde.plot$transmitter, kde.plot$contour, sep = ':')
row.names(kde.plot) <- NULL

library(ggplot2)
ggplot() +
  # geom_point(data=coa.list[[1]], aes(coa.long, coa.lat))+
  geom_path(data = kde.plot[1:1683,], aes(x, y, group = contour, color = transmitter))
