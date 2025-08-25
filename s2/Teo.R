edad <- c(23,34,17,18,NA)
distrito <- c("San Borja", 'Lince', "Lince", NA, "Breña")
peso <- c(NA, 56, 78.5, 89.4, 59.9)
df <- data.frame(edad, distrito, peso)
df

getwd() # ver el directorio de trabajo actual

#calcula la media de la edad
media_edad <- mean(df$edad, na.rm = TRUE) #na.rm = True para ignorar los NA
media_edad

#hallar cantidad NA en el dataframe
cantidad_na <- sum(is.na(df))
cantidad_na

#Numero de observaciones completas (por fila) :
observaciones_completas <- sum(complete.cases(df)) # return int
observaciones_completas

#Visualizar las filas con datos completos
datos_completos <- df[complete.cases(df), ]
datos_completos

#visualizar las filas con datos incompletos
datos_incompletos <- df[!complete.cases(df), ]
datos_incompletos

#porcentaje de perdida al quitar las filas incompletas
porcentaje_perdida <- (1 - (observaciones_completas / nrow(df))) * 100
porcentaje_perdida

#menor tamaño efectivo de la muestra
tamaño_efectivo <- min(colSums(!is.na(df)))
variable_tamaño_efectivo <- names(which.min(colSums(!is.na(df))))
variable_tamaño_efectivo
tamaño_efectivo

df
df[,1] #todas las filas de la primera columna
df[1,] #todas las columnas de la primera fila


#Valores atipicos (outliers)
peso1 <- c(1,40,45,48,50,56,101,1000,1500)
boxplot(peso1) # visualizar outliers

