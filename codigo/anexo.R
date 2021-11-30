# ANEXO-CODIGO-GRAFICO-TABLAS
 
# Aquí se puede visualizar los códigos para obtener los diferentes gráficos y/o tablas

# Paquetes utilizados

require(ggplot2)
require(dplyr)
require(gghighlight)
require(plotly)
require(ggrepel)
require(kableExtra)

# RADIO
# 
radio_jupiter <- 142984/2
radio_tierra <- 12756/2

radio_t_j <- radio_jupiter/radio_tierra

radios <- exo_planetas |> 
  rename(nombre = Name, radio = `Radius (R Jup / R Earth)`)|> 
  mutate(radio_con_respecto_la_tierra = round(radio*radio_t_j,digits = 3)) |> 
  select(nombre,radio_con_respecto_la_tierra) |> 
  na.omit()

grafico_radios <- radios |> 
  ggplot(aes(nombre, radio_con_respecto_la_tierra))+
  geom_point(size = 5,alpha = 0.4, col = "darkblue")+
  gghighlight((radio_con_respecto_la_tierra < 2)) +
  scale_color_manual(values = "darkblue")+
  labs(x = "Exoplanetas",
       y = "Radio con respeccto a la Tierra",
       title = "Exoplanetas con radio similar a la Tierra",
       subtitle = "Se considera a los exoplanetas de tipo terrestre, de entre 0.5 a 2 radios Terrestres",
       caption = "Datos extraídos de http://exoplanet.eu")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  ylim(0,20)+
  theme_light()

ggplotly(grafico_radios)

# MASA

masa_jupiter <- 1898130000000000000000000000
masa_tierra <- 5972190000000000000000000

masas <- exo_planetas |> 
  rename(nombre = Name, masacrJ= `Mass (M Jup)`) |> 
  mutate(masa = masacrJ*masa_jupiter) |> 
  select(nombre,masa) |> 
  na.omit()

grafico_masas <- masas |> 
  arrange(nombre) |> 
  filter(masa >= 0.5*masa_tierra & masa <= 2*masa_tierra) |>  
  ggplot(aes(nombre, masa))+
  geom_point(size = 4, col = "darkblue")+
  labs(x = "Exoplanetas",
       y = "Masas",
       title = "Exoplanetas con la masa necesaria para ser habitable",
       subtitle = "Se considera a aquellos exoplanetas de tipo Terrestres (0.5 - 2 masas terrestres)",
       caption = "Datos extraídos de http://exoplanet.eu",
       color = "Exoplanetas")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")


grafico_masas

# TEMPERATURA

grafico_temperatura <- temperatura |> 
  arrange(nombre) |> 
  filter(temperaturaC >= -5 & temperaturaC <= 42) |>  
  ggplot(aes(nombre, temperaturaC))+
  geom_point(size = 5, col = 'darkblue' )+
  labs(x = "Exoplanetas",
       y = "Temperatura (ºC)",
       title = "Exoplanetas con Temperatura similar a la Tierra",
       subtitle = "Se considera una temperatura de entre los -5ºC a los 42ºC",
       caption = "Datos extraídos de http://exoplanet.eu")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")

grafico_temperatura

# PERIODO ÓRBITAL

periodo <- exo_planetas |> 
  rename(nombre = Name, periodo_dias = `Period (day)`) |> 
  select(nombre,periodo_dias) |> 
  na.omit()

grafico_periodo <- periodo |> 
  arrange(nombre) |> 
  filter(periodo_dias >= 360 & periodo_dias <= 400) |>  
  ggplot(aes(nombre, periodo_dias))+
  geom_point(size = 5, col="darkblue")+
  labs(x = "Exoplanetas",
       y = "Periodo Órbital (días)",
       title = "Exoplanetas con Periodo Órbital similar a la Tierra",
       subtitle = "Se considera una Periodo Órbital de entre los 360 y 400 días",
       caption = "Datos extraídos de http://exoplanet.eu")+
  scale_x_discrete(name="Exoplanetas", breaks= NULL,labels = NULL)+
  geom_text_repel(aes(label = nombre),nudge_y = 1.5)+
  theme_light()+
  theme(legend.position = "none")

grafico_periodo

# MOLECULAS

moleculas_para_la_vida <- exo_planetas |> 
  rename(nombre = Name, moleculas = `Molecules`) |> 
  select(nombre,moleculas) |> 
  na.omit()

exoplanetas_con_agua <- moleculas_para_la_vida |> 
  arrange(nombre) |> 
  filter(str_detect(moleculas, "H2O")) |> 
  select(nombre, moleculas) 

kable(exoplanetas_con_agua,
      digits =8,
      row.names = TRUE,
      booktabs = TRUE, 
      centering = TRUE,
      align = "rrrrrrr",
      options= list(pageLengtj = 11)) |> 
  scroll_box(width = "85%",height = "500px") |> 
  kable_minimal()
