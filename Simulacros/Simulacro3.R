library(readr)
df <- read_csv("Desktop/Estadistica y Probabilidades/Simulacros/piloto.csv")

nrow(df)
ncol(df)

#variables numericas
summary(df)

#datos faltantes
sum(is.na(df))

#cantidad de datos
nrow(df)*ncol(df)

round((1437/(nrow(df)*ncol(df)))*100, 2)

sum(!complete.cases(df))


#cual es la variable con mas datos faltantes
colSums(is.na(df))

#promedio de datos faltantes por observacion 
round(mean(colSums(is.na(df))),2)


#Es necesario hacer una limpieza de este DF. Para esto la política de manejo de datos indica que debes descartar las filas donde faltan los valores de la variable Peso y al menos de una de las variables Talla o Largo
to_drop <- is.na(df$Peso) & (is.na(df$Talla) | is.na(df$Largo))
df_clean <- df[!to_drop, ]
nrow(df_clean)



#tamaño efectivo de la variable Edad
sum(complete.cases(df$Edad))
round(sd(df$Edad, na.rm = TRUE), 2)

#rango de edad
round(range(df$Edad, na.rm = TRUE),2 )

#rango intercuartil
round(IQR(df_clean$Edad, na.rm = TRUE, type = 3),2 )

#la variable esta relativamente concentrada alrededor de su mediana, esta muy concentrada alrededor de su mediana o esta relativamente dispersa a su mediana
round(mad(df$Edad, na.rm = TRUE),2 )
boxplot(df$Edad)
hist(df$Edad)

#tamaño efectivo de la muestra de la variable Sexo
sum(complete.cases(df_clean$Sexo))
#valores distintos de la muestra
unique(df$Sexo)


#mediana de la variable Sexo
table(df$Sexo)



#tamaño efectivo de la muestra variable Edad
sum(complete.cases(df$Edad))

#coeficiente de variacion de la variable Edad
round((sd(df$Edad, na.rm = TRUE)/mean(df$Edad, na.rm = TRUE)),2)
