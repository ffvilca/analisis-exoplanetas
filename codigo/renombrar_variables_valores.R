library(dplyr)
library(gapminder)
library(countrycode)

desarrollo <- gapminder %>%
  mutate(region = countrycode(sourcevar = country, 
                              origin = "country.name", 
                              destination = "region23"), 
         .after = country, 
         country = countrycode(sourcevar = country, 
                               origin = "country.name", 
                               destination = "cldr.name.es")) %>%
  rename(pais = country, 
         continente = continent, 
         anio = year, 
         expectativa_vida = lifeExp, 
         poblacion = pop, 
         pib_per_capita = gdpPercap)