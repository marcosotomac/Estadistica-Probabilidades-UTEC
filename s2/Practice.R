library("gapminder")
library("dplyr")

#Nivel Basico
#1.	¿Cuál es el promedio de lifeExp en el año 2007?
#2.	¿Qué países tienen una población mayor a 100 millones en 2007?
#3.	Filtra solo los datos de Perú y muestra la evolución de gdpPercap a lo largo de los años.

#⸻

#Nivel intermedio
#4.	Calcula el promedio de gdpPercap por continente en el año 2007.
#5.	¿Qué país de África tenía la menor esperanza de vida en 1952?
#6.	Genera una tabla con el máximo y mínimo de lifeExp por continente en el año 2007.

#Nivel avanzado
#7 Crea una nueva columna que calcule el PIB total (gdpPercap * pop) y muestra el top 5 de países con mayor PIB total en 2007.

data("gapminder")
gapminder

summary(gapminder)

data("gapminder")
gapminder


#obtain all the columns names of gapminder
colnames(gapminder)


#promedio de life exp
mean(gapminder$lifeExp, na.rm = TRUE)

#promedio de lifeExp en el año 2007
consulta1 <- gapminder %>% filter(year==2007) %>% summarise(mean(lifeExp))
consulta1


#paises con poblacion mayor a 100 millones en el año 2007
consulta2 <- gapminder %>% filter(year==2007 & pop>100000000 ) %>% select(country)
consulta2

#consulta3e
consulta3 <- gapminder %>% filter(country=="Peru") %>% select(year,gdpPercap)
consulta3

#consulta 4
consulta4 <- gapminder %>% 
  filter(year==2007) %>% 
  group_by(continent) %>% 
  summarise(promedio_gdpPercap = mean(gdpPercap))
consulta4

#consulta 5
consulta5 <- gapminder %>% filter(continent=="Africa") %>% filter(lifeExp == min(lifeExp)) %>% select(country, lifeExp)
consulta5

#consulta 6
consulta6 <- gapminder %>% filter(year==2007) %>% group_by(continent) %>% summarise(vida_min= min(lifeExp), vida_max = max(lifeExp))
consulta6

#consulta 7
consulta8 <- gapminder %>% filter(year==2007) %>% mutate(pbi_total = gdpPercap * pop)%>% select(country, pbi_total) %>% arrange(desc(pbi_total)) %>% head(5)
consulta8

