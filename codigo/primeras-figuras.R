# el enlace directo a este archivo es: https://www.dropbox.com/s/2p9hjhsfsa4llsu/primeras-figuras.R?dl=0

# primero, vamos a cargar los paquetes 

require(ggplot2)
require(gapminder)
require(dplyr)
require(countrycode)
require(gghighlight)
require(ggtext)

# modificar un poco los datos: voy a trducir
# los nombres de paise y voy a agregar una 
# variables nueva que va a ser "region"

# %>% == |> es como y luego

desarrollo <- gapminder |> 
  mutate(region = countrycode(sourcevar = country, origin = "country.name",
                                          destination = "region23"), .after = country,
         country = countrycode(sourcevar = country, origin = "country.name",
                               destination = "cldr.name.es"))


# EJERCICIO 1: gráfico de dispersión ----

# versión 1 -> sirve para explorar los datos
ggplot(desarrollo, aes(x = gdpPercap, y = lifeExp))+
  geom_point(alpha = 0.4)+
  scale_x_log10()

# alpha en geom_point -> agrega transparencia

# versión 2
ggplot(desarrollo, aes(x = gdpPercap, y = lifeExp))+
  geom_point(alpha = 0.4)+
  scale_x_log10(labels = scales::dollar)+
  labs(title = "Crecimiento económico y esperanza de vida en el mundo (1952-2007)",
       subtitle = "Cada punto representa un país/año",
       x = "PIB per capita en dólares",
       y = "Esperanza de vida al nacer",
       caption = "Elaboración propia a partir de datos de gapminder.org")+
  theme_minimal() + 
  scale_y_continuous(limits = c(0,90), breaks = c(0,20,40,60,80))

ggsave("figuras/dispersion_pib-expvida_gapminder.png", height = 7, width = 10)

# EJERCICIO 2: gráfico de barras ----

desarrollo |> 
  filter(country == "Chile") |> 
    
