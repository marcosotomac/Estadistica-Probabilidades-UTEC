library(readr)
DF <- read_csv("~/Library/Containers/net.whatsapp.WhatsApp/Data/Desktop/Estadistica y Probabilidades/Simulacros/L1C Data.csv")

### Parte 1

#observaciones
nrow(DF)

#columnas
ncol(DF)

#datos faltantes
sum(is.na(DF))

#observaciones incompletas por datos faltantes
sum(!complete.cases(DF))

### Parte 2

#si eliminamos todas las observaciones incompletas
nrow(DF) - (sum(!complete.cases(DF)))

#perderiamos
(sum(!complete.cases(DF)))/nrow(DF)

#menor tamaño efectivo de la muestra
min(colSums(!is.na(DF)))
which.min(colSums(!is.na(DF)))



