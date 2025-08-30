

list <- c(1,23,2,423,53423,4,4,4,4,453, NA)


#funcion que calcula la hipotenusa en un triangulo rectangulo
hipotenusa <- function(a, b) {
  return(sqrt(a^2 + b^2))
}

#funcion para calcular la moda de manera simple
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#variable "grado" Como factor
gradp <- factor(c("Primero", "Segundo", "Tercero", "Cuarto", "Quinto", "Sexto"), ordered = TRUE)

#mean (media)
mean(list, na.rm = TRUE)

#donde haya un valor faltante colocale una media
list[is.na(list)] <- mean(list, na.rm = TRUE)

library("gapminder")




hipotenusa(3,4)