
offshore.detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections/offshore md')
recieved.detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections/received')

bsb.detects <- rbind(offshore.detects, recieved.detects)
bsb.detects <- bsb.detects[bsb.detects$transmitter %in%
                             c(paste0('A69-1601-', seq(44950, 44994, 1)),
                               paste0('A69-1602-', seq(1411, 1448, 1)),
                               paste0('A69-1602-', seq(1449, 1455, 1)),
                               paste0('A69-1602-', seq(9540, 9584, 1)),
                               paste0('A69-1602-', seq(17709, 17724, 1)),
                               paste0('A69-9006-', seq(7594, 7607, 1))),]
rm(offshore.detects, recieved.detects)




saveRDS(bsb.detects, file = 'bsb_detections.rds')
