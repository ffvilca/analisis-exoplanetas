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
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_col(fill = "turquoise3") +
  theme_minimal() +
  labs(title = "Evolución del PIB en Chile (1952 - 2007)",
       y = "PIB percapita en dolares",
       x = NULL)+
  scale_y_continuous(labels = scales::dollar)+
  geom_text(aes(label = round(gdpPercap)), vjust = -0.5)+ # sirve para poner etiquetas
  scale_x_continuous(breaks = seq(1952,2007, by = 10))
    
ggsave("figuras/columnas_pib-chile.png", height = 7, width = 10)

# version alternativa

desarrollo |> 
  filter(country == "Chile") |> 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_line(color = "turquoise3") +
  geom_point(color = "turquoise4") +
  theme_minimal() +
  labs(title = "Evolución del PIB en Chile (1952 - 2007)",
       y = "PIB percapita en dolares",
       x = NULL)+
  scale_y_continuous(labels = scales::dollar)+
  geom_text(aes(label = round(gdpPercap)), vjust = -0.8, hjust = 1, color = "turquoise4")+
  scale_x_continuous(limits = c(1945,2010), breaks = seq(1945,2010,10))

# geom_text sirve para poner etiquetas

ggsave("figuras/linea_pib-chile.png", height = 7, width = 10)


# EJERCICIO 3: gráfico de barras ------------------------------------------
desarrollo |> 
  filter(country %in% c("Chile", "Argentina")) |> 
  ggplot(aes(year,gdpPercap, fill= country))+
  geom_col(position = "dodge") + 
  theme_minimal()+
  scale_fill_manual(values = c("#FF4D00","turquoise3"))+
  labs(x = NULL,
       y = "PIB per capita en dolares",
       title = "Evolución del PIB per capita en Chile y Argentina",
       subtitle = "1952 a 2007",
       fill = "pais")

# dodge -> poner a los lados
# EJERCICIO 4 : anotar y destacar ----------------------------------------

desarrollo |> 
  filter(region == "South America") |>
  ggplot(aes(year, gdpPercap, color=country))+
  geom_line()+
  theme_minimal()+
  labs(x = NULL,
       y = "PIB per capita en dolares",
       title = "Evolución del PIB en Sud América",
       subtitle = "años 1952 a 2007") +
  scale_y_continuous(limits = c(0,15000), labels = scales::dollar) +
  geom_vline(xintercept = 1997, color = "gray" , linetype = "dashed")+
  geom_label(aes(x = 1989, y = 14000, label = "1997: Crisis Asiatica"),
             color = "gray51", size =3)

# versión 2 enfoque de solo 1 país

desarrollo |> 
  filter(region == "South America") |>
  ggplot(aes(year, gdpPercap, color=country))+
  geom_line()+
  gghighlight(country == "Chile")+
  scale_color_manual(values = "turquoise3")+
  theme_minimal()+
  labs(x = NULL,
       y = "PIB per capita en dolares",
       title = "Evolución del PIB en Chile entre 1952 y 2007",
       subtitle = "Comparación con otros países de Sudamerica") +
  scale_y_continuous(limits = c(0,15000), labels = scales::dollar) +
  geom_vline(xintercept = 1997, color = "gray" , linetype = "dashed")+
  geom_label(aes(x = 1989, y = 14000, label = "1997: Crisis Asiatica"),
             color = "gray51", size =3)



# EJERCICIO 5 : otra forma de construir una leyenda para un gráfico -------

  desarrollo |> 
  filter(country %in% c("Chile", "Argentina")) |> 
  ggplot(aes(year,gdpPercap, fill= country))+
  geom_col(position = "dodge", show.legend = F) + 
  theme_minimal()+
  scale_fill_manual(values = c("#e35d6a","#005B96"))+
  labs(x = NULL,
       y = "PIB per capita en dolares",
       title = "Evolución del PIB per capita en <b style='color:#005B96'> Chile </b> y <b style='color:#e35d6a'> Argentina </b>",
       subtitle = "1952 a 2007",
       fill = "pais")+
  theme(plot.title = element_markdown())

# dodge -> poner a los lados

