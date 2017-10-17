
offshore.detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections/offshore md')
recieved.detects <- TelemetryR::vemsort('p:/obrien/biotelemetry/detections/received')

bsb.detects <- rbind(offshore.detects, recieved.detects)
bsb.detects <- bsb.detects[bsb.detects$transmitter %in%
                             paste0('A69-1601-', seq(44950, 44994, 1)) |
                             bsb.detects$transmitter %in%
                             paste0('A69-1602-', seq(1411, 1448, 1)),]
rm(offshore.detects, recieved.detects)

saveRDS(bsb.detects, file = 'data/bsb_detections.rds')
