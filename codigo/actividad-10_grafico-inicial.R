# ACTIVIDAD 10 ------------------------------------------------------------

# En los siguientes gráficos lo que nos interesará será buscar a aquellos 
# exoplanetas con las caracteristicas más similares a las de la Tierra, por lo 
# que para cada gráfico seleccionaremos una característica de interes y 
# compararemos con la Tierra

# PAQUETES REQUERIDOS ----

require(ggplot2)
require(ggrepel)
require(dplyr)
require(gghighlight)
require(ggtext)
require(tidyverse)

# Primero recordemos como son nuestros datos

exo_planetas <- readxl::read_excel(here("datos/catalogo-exoplanetas.xlsx")) |> as.data.frame()

View(exo_planetas)
names(exo_planetas)
str(exo_planetas)

## PERIODO ORBITAL EN DIAs -------------------------------------------------------------

# Ahora llamaremos a nuestra base de datos

# Para analizar solo la variable del périodo usando la librería dplyr crearemos
# una base en la que nos sea más fácil trabajar

periodo <- exo_planetas |> 
  rename(nombre = Name, periodo_dias = `Period (day)`) |> 
  select(nombre,periodo_dias) |> 
  na.omit()

View(periodo)

grafico_periodo <- periodo |> 
  arrange(periodo_dias) |> 
  filter(periodo_dias >= 360 & periodo_dias <= 400) |>  
  ggplot(aes(nombre, periodo_dias, color=nombre))+
  geom_point(size = 5)+
  scale_color_manual(values = rainbow(24))+
  labs(x = "Exoplanetas",
       y = "Periodo Órbital (días)",
       title = "Exoplanetas con Periodo Órbital similar a la Tierra",
       subtitle = "Se considera una Periodo Órbital de entre los 360 y 400 días",
       caption = "Datos extraídos de http://exoplanet.eu",
       color = "Exoplanetas")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")

grafico_periodo

ggsave("/Users/home/Documents/LET/analisis-exoplanetas/figuras/exoplanetas-periodo-orbital.png",
       height = 7, width = 9)



## TEMPERATURA -------------------------------------------------------------

# Para esta variable haremos algo similar, entonces:

temperatura <- exo_planetas |> 
  rename(nombre = Name, temperatura = `Temp calculated (K)`) |> 
  mutate(temperaturaC = (temperatura-273)) |> 
  select(nombre,temperaturaC) |> 
  na.omit()

View(temperatura)  

# Sabemos que la vida en la Tierra para ser posible debe tener una Temperatura 
# de entre los 15º-39ºC, por lo que buscamos exoplanetas dentro de ese rango, de
# aquellos planetas que tenemos información están:

grafico_temperatura <- temperatura |> 
  arrange(temperaturaC) |> 
  filter(temperaturaC >= -5 & temperaturaC <= 42) |>  
  ggplot(aes(nombre, temperaturaC, color=nombre))+
  geom_point(size = 5)+
  scale_color_manual(values = rainbow(21))+
  labs(x = "Exoplanetas",
       y = "Temperatura (ºC)",
       title = "Exoplanetas con Temperatura similar a la Tierra",
       subtitle = "Se considera una temperatura de entre los -5ºC a los 42ºC",
       caption = "Datos extraídos de http://exoplanet.eu",
       color = "Exoplanetas")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")

grafico_temperatura

ggsave("/Users/home/Documents/LET/analisis-exoplanetas/figuras/exoplanetas-temperatura.png",
       height = 7, width = 9)


## RADIO ------------------------------------------------------------------

# Debido a que las orbitas de los planetas son de grandes tamaños a pesar de 
# ser elipses, se tratan de como si fueran circunferencias

radio_jupiter <- 142984/2
radio_tierra <- 12756/2

radio_t_j <- radio_jupiter/radio_tierra

radios <- exo_planetas |> 
  rename(nombre = Name, radio = `Radius (R Jup / R Earth)`)|> 
  mutate(radio_con_respecto_la_tierra = round(radio*radio_t_j,digits = 3)) |> 
  select(nombre,radio_con_respecto_la_tierra) |> 
  na.omit()

grafico_radios <- radios |> 
  filter(radio_con_respecto_la_tierra > 0.5 | radio_con_respecto_la_tierra < 2) |> 
  ggplot(aes(nombre, radio_con_respecto_la_tierra))+
  geom_point(size = 5,alpha = 0.4,col='darkblue')+
  labs(x = "Exoplanetas",
       y = "Radio con respeccto a la Tierra",
       title = "Exoplanetas con radio similar a la Tierra",
       subtitle = "Se considera a los planetas de tipo terrestre, de entre 0.5 a 2 radios Terrestres",
       caption = "Datos extraídos de http://exoplanet.eu")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  ylim(c(0.25,2.25))+
  theme_light()


grafico_radios

ggsave("/Users/home/Documents/LET/analisis-exoplanetas/figuras/exoplanetas-radios.png",
       height = 7, width = 9)


## MOLECULAS ---------------------------------------------------------------

# este dato es mejor solo trabajarlo y no gráficarlo quizás añadirlo en una tabla

moleculas_para_la_vida <- exo_planetas |> 
  rename(nombre = Name, moleculas = `Molecules`) |> 
  select(nombre,moleculas) |> 
  na.omit()

#prueba <- moleculas_para_la_vida |> 
#  separate(moleculas,sep = ",", into = as.character(c(1:19)))
#  filter(str_detect(moleculas, "H2O")) |> 
#  select(nombre) 

#View(prueba)
## MASA --------------------------------------------------------------------

# Una forma de obtener la masa del planeta es comparandolo directamente con la 
# masa de Júpiter, ya que es un valor conocido y que nos puede entregar mucha 
# informacion por lo que haremos algunas transformaciones a la masa entregada por
# la base, para que sea más facil la comparación, recordemos
# que segun un articulo https://www.europapress.es/ciencia/astronomia/noticia-cual-tamano-minimo-planeta-pueda-ser-habitable-20190911143452.html
# es necesario que la masa de un planeta habitable sea como minimo el 2,7% de la
# masa de la tierra
# 
# Con los datos obtenidos desde https://solarsystem.nasa.gov/planets/earth/by-the-numbers/
# tenemos en kg :

masa_jupiter <- 1898130000000000000000000000
masa_tierra <- 5972190000000000000000000

# Por lo que ahora solo debemos armar bien nuestra base

masas <- exo_planetas |> 
  rename(nombre = Name, masacrJ= `Mass (M Jup)`) |> 
  mutate(masa = masacrJ*masa_jupiter) |> 
  select(nombre,masa) |> 
  na.omit()

View(masas) 

masa_minima <- masa_tierra*(2.7/100)

grafico_masas <- masas |> 
  arrange(masa) |> 
  filter(masa >= 0.5*masa_tierra & masa <= 2*masa_tierra) |>  
  ggplot(aes(nombre, masa, color= nombre))+
  geom_point(size = 4)+
  scale_color_manual(values = rainbow(35))+
  labs(x = "Exoplanetas",
       y = "Masas",
       title = "Exoplanetas con la masa necesaria para ser habitable",
       subtitle = "Se considera a aquellos planetas de tipo Terrestres (0.5 - 2 masas terrestres)",
       caption = "Datos extraídos de http://exoplanet.eu",
       color = "Exoplanetas")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")


grafico_masas

ggsave("/Users/home/Documents/LET/analisis-exoplanetas/figuras/exoplanetas-masas.png",
       height = 7, width = 9)
