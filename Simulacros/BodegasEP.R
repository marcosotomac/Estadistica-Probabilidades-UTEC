library(readr)
install.packages("dplyr")
library(dplyr)
DF <- read_csv("~/Library/Containers/net.whatsapp.WhatsApp/Data/Desktop/Estadistica y Probabilidades/Simulacros/BodegasEP.csv")

#DFT se obtiene a partir de DF usando complete.cases y mutate
#DFD se obtiene a partir de DFT usando group_by y summarize

#Comandos adicionales
#is.na
#which.max
#which.min

#cuantas bodegas participaron en el estudio
nrow(DF)

#numero de variables numericas en DF
sum(sapply(DF, is.numeric))

#bodegas con todos sus datos completos
DFT <- DF[complete.cases(DF), ]
nrow(DFT)

#en DF, cuantos datos faltantes hay y que bodegas son las que tienen esos datos faltantes
sum(is.na(DF))
which(!complete.cases(DF))


#cuantas bodegas, con todos sus datos completos, participaron en Miraflores (DFT), usando pipe

nrow(DFT %>% filter(Distrito == "Miraflores"))

#total de ventas en todos los distritos
sum(DFT$Ventas1) + sum(DFT$Ventas2) + sum(DFT$Ventas3) + sum(DFT$Ventas4)

#en que distrito esta la bodega con mayor venta mensual
DFT %>% filter((Ventas1 == max(Ventas1)) | (Ventas2 == max(Ventas2)) | (Ventas3 == max(Ventas3)) | (Ventas4 == max(Ventas4))) %>% select(Distrito)
#como se llama esa bodega
DFT %>% filter((Ventas1 == max(Ventas1)) | (Ventas2 == max(Ventas2)) | (Ventas3 == max(Ventas3)) | (Ventas4 == max(Ventas4))) %>% select(Bodega)


#en que distrito esta la bodega con mayor venta semanal
DFT %>% filter((Ventas1/4 == max(Ventas1/4)) | (Ventas2/4 == max(Ventas2/4)) | (Ventas3/4 == max(Ventas3/4)) | (Ventas4/4 == max(Ventas4/4))) %>% select(Distrito)
#como se llama esa bodega
DFT %>% filter((Ventas1/4 == max(Ventas1/4)) | (Ventas2/4 == max(Ventas2/4)) | (Ventas3/4 == max(Ventas3/4)) | (Ventas4/4 == max(Ventas4/4))) %>% select(Bodega)

#bodega con el nombre mas largo y cuanto vendio en total
DFT %>% mutate(Longitud = nchar(Bodega)) %>% filter(Longitud == max(Longitud)) %>% select(Bodega)

total_ventas <- DFT %>% mutate(Longitud = nchar(Bodega)) %>% filter(Longitud == max(Longitud)) %>% mutate(TotalVentas = Ventas1 + Ventas2 + Ventas3 + Ventas4) %>% select(Bodega, TotalVentas)


#cual es el nombre del distrito que tiene las menores ventas en el estudio
DFD <- DFT %>% group_by(Distrito) %>% summarize(VentasTotales = sum(Ventas1) + sum(Ventas2) + sum(Ventas3) + sum(Ventas4)) %>% arrange(VentasTotales)

